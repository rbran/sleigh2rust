use std::collections::HashMap;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::semantic::disassembly::{GlobalSet, Variable};
use sleigh_rs::semantic::pattern::{
    ProducedTable, ProducedTokenField, Verification,
};
use sleigh_rs::semantic::GlobalAnonReference;
use sleigh_rs::{IntTypeU, NonZeroTypeU};

use crate::builder::formater::from_sleigh;
use crate::builder::{Disassembler, DisassemblyGenerator};

use super::{ConstructorStruct, DisassemblyPattern, TableField};

pub fn root_pattern_function(
    parse_fun: &Ident,
    constructor: &ConstructorStruct,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let context_trait_name = &disassembler.memory.spaces_trait.name;
    let inst_work_type = &disassembler.inst_work_type;
    let inst_start = format_ident!("inst_start");
    let pattern_len = format_ident!("pattern_len");
    let tokens_current = format_ident!("tokens_current");
    let context_instance = format_ident!("context_instance");
    let mut root_tables = HashMap::new();
    let mut root_token_fields = HashMap::new();
    let mut blocks_iter =
        constructor.sleigh.pattern.blocks().iter().enumerate();

    //first block will be parsed with the disassembly
    let first_block_parse = blocks_iter.next().map(|(index, block)| {
        body_block(
            constructor,
            Some(&constructor.sleigh.disassembly),
            index,
            block,
            &pattern_len,
            &inst_start,
            &context_instance,
            &tokens_current,
            &HashMap::new(),
            &HashMap::new(),
            &mut root_tables,
            &mut root_token_fields,
        )
    });

    let blocks_parse: TokenStream = blocks_iter
        .map(|(index, block)| {
            body_block(
                constructor,
                None,
                index,
                block,
                &pattern_len,
                &inst_start,
                &context_instance,
                &tokens_current,
                &HashMap::new(),
                &HashMap::new(),
                &mut root_tables,
                &mut root_token_fields,
            )
        })
        .collect();
    let table_fields = root_tables.iter().map(|(ptr, gen_name)| {
        let struct_field = constructor
            .table_fields
            .get(&ptr)
            .expect("LOGIC_ERROR: Produced table is not part of the struct");
        let struct_field_name = &struct_field.name;
        (struct_field_name, gen_name)
    });
    let token_fields =
        root_token_fields
            .iter()
            .filter_map(|(ptr, gen_field_name)| {
                let struct_field = constructor.ass_fields.get(&ptr)?;
                let struct_field_name = &struct_field.name;
                Some((struct_field_name, gen_field_name))
            });
    let fields =
        table_fields
            .chain(token_fields)
            .map(|(struct_field, gen_field)| {
                if struct_field == gen_field {
                    struct_field.to_token_stream()
                } else {
                    quote! { #struct_field: #gen_field }
                }
            });
    quote! {
        fn #parse_fun<T>(
            mut #tokens_current: &[u8],
            context: &mut T,
            #inst_start: #inst_work_type,
        ) -> Option<(#inst_work_type, Self)>
            where T: #context_trait_name + Clone
        {
            //each block will increment this value by its size
            let mut #pattern_len = 0 as #inst_work_type;
            let mut #context_instance = context.clone();

            #first_block_parse

            //the current_token will be increseased by each block, so the
            //next block knows when to start parsing
            #blocks_parse
            //only on instruction table, otherwise this is on a function
            *context = #context_instance;
            Some((
                #pattern_len,
                Self{ #(#fields,)* },
            ))
        }
    }
}

fn sub_pattern_closure(
    constructor: &ConstructorStruct,
    pattern: &sleigh_rs::Pattern,
    inst_start: &Ident,
    root_tables: &HashMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &HashMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let tokens_current = format_ident!("tokens");
    let inst_work_type = &disassembler.inst_work_type;
    let pattern_len = format_ident!("pattern_len");
    let context_instance = format_ident!("context_instance");
    let mut produced_tables = HashMap::new();
    let mut produced_token_fields = HashMap::new();
    let blocks_parse: TokenStream = pattern
        .blocks()
        .iter()
        .enumerate()
        .map(|(index, block)| {
            body_block(
                constructor,
                None,
                index,
                block,
                &pattern_len,
                &inst_start,
                &context_instance,
                &tokens_current,
                root_tables,
                root_token_fields,
                &mut produced_tables,
                &mut produced_token_fields,
            )
        })
        .collect();
    let table_fields = pattern.tables().map(|table_field| {
        produced_tables
            .get(&table_field.table().element_ptr())
            .expect("LOGIC_EROR: Table not produced")
    });
    let token_fields = pattern.token_fields().map(|token_field| {
        produced_token_fields
            .get(&token_field.token_field().element_ptr())
            .expect("LOGIC_EROR: TokenField not produced")
    });
    quote! {
        |tokens: &[u8], context_param: &mut T| {
            //each block will increment this value by its size
            let mut #pattern_len = 0 as #inst_work_type;
            let mut #context_instance = context_param.clone();
            //the current_token will be increseased by each block, so the next
            //block knows when to start parsing
            let mut #tokens_current = tokens;
            #blocks_parse
            *context_param = #context_instance;
            Some((
                (#(#table_fields),*),
                (#(#token_fields),*),
                #pattern_len
            ))
        }
    }
}

fn token_parser_creation(
    constructor: &ConstructorStruct,
    token_len: IntTypeU,
    tokens: &Ident,
) -> Option<(Ident, TokenStream)> {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    NonZeroTypeU::new(token_len).map(|token_len| {
        let name = format_ident!("token_parser");
        let creation_call =
            disassembler.token_parser.gen_new_call(token_len, &tokens);
        let creation = quote! {let #name = #creation_call?;};
        (name, creation)
    })
}

fn disassembly_pre_match(
    constructor: &ConstructorStruct,
    disassembly: &sleigh_rs::Disassembly,
    token_parser: Option<&Ident>,
    context_param: &Ident,
    inst_start: &Ident,
    root_tables: &HashMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &HashMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let disassembly_pattern = DisassemblyPattern {
        disassembler: &disassembler,
        token_parser,
        context_param,
        inst_start,
        root_tables,
        root_token_fields,
        vars: std::cell::RefCell::default(),
    };
    disassembly_pattern
        .disassembly(&disassembly.vars, &disassembly.assertations)
}

fn body_block(
    constructor: &ConstructorStruct,
    disassembly: Option<&sleigh_rs::Disassembly>,
    block_index: usize,
    block: &sleigh_rs::Block,
    pattern_len: &Ident,
    inst_start: &Ident,
    context_param: &Ident,
    tokens: &Ident,
    root_tables: &HashMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &HashMap<*const sleigh_rs::TokenField, Ident>,
    produced_tables: &mut HashMap<*const sleigh_rs::Table, Ident>,
    produced_fields: &mut HashMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    match block {
        sleigh_rs::Block::And {
            len,
            token_len,
            token_fields,
            tables,
            verifications,
        } => body_block_and(
            constructor,
            disassembly,
            block_index,
            len,
            *token_len,
            token_fields,
            tables,
            verifications,
            pattern_len,
            inst_start,
            context_param,
            tokens,
            root_tables,
            root_token_fields,
            produced_tables,
            produced_fields,
        ),
        sleigh_rs::Block::Or {
            len,
            token_fields,
            tables,
            branches,
        } => {
            let block_closure = block_or_closure(
                constructor,
                disassembly,
                len,
                token_fields,
                tables,
                branches,
                pattern_len,
                inst_start,
                root_tables,
                root_token_fields,
            );
            //create the fields produced by this closure
            let fields = block.token_fields().iter().map(|field| {
                let token_field = field.token_field().element();
                let name = format_ident!("{}", from_sleigh(token_field.name()));
                produced_fields
                    .entry(token_field.element_ptr())
                    .and_modify(|_| unreachable!())
                    .or_insert_with(|| name.clone());
                name
            });
            let tables = block.tables().iter().map(|field| {
                let table = field.table().element();
                let name = format_ident!("{}", from_sleigh(table.name()));
                produced_tables
                    .entry(table.element_ptr())
                    .and_modify(|_| unreachable!())
                    .or_insert_with(|| name.clone());
                name
            });
            let block_name = format_ident!("block_{}", block_index);
            let block_len = format_ident!("block_{}_len", block_index);
            quote! {
                let #block_name = #block_closure;
                let (
                    (#(#tables),*),
                    (#(#fields),*),
                    #block_len,
                ) = #block_name(#tokens, &mut #context_param)?;
                #pattern_len += #block_len;
                #tokens = &#tokens[usize::try_from(#block_len).unwrap()..];
            }
        }
    }
}

fn body_block_and(
    constructor: &ConstructorStruct,
    disassembly: Option<&sleigh_rs::Disassembly>,
    block_index: usize,
    len: &sleigh_rs::PatternLen,
    token_len: IntTypeU,
    sleigh_token_fields: &[ProducedTokenField],
    sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    pattern_len: &Ident,
    inst_start: &Ident,
    context: &Ident,
    tokens: &Ident,
    root_tables: &HashMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &HashMap<*const sleigh_rs::TokenField, Ident>,
    produced_tables: &mut HashMap<*const sleigh_rs::Table, Ident>,
    produced_token_fields: &mut HashMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let block_len = format_ident!("block_{}_len", block_index);
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let inst_work_type = &disassembler.inst_work_type;

    let (token_parser, code_pre) = body_block_and_pre(
        constructor,
        disassembly,
        len,
        token_len,
        sleigh_token_fields,
        sleigh_tables,
        verifications,
        &block_len,
        inst_start,
        root_tables,
        root_token_fields,
        &tokens,
        context,
    );
    let disassembly = disassembly.map(|disassembly| {
        disassembly_pre_match(
            constructor,
            disassembly,
            token_parser.as_ref(),
            context,
            inst_start,
            root_tables,
            root_token_fields,
        )
    });
    let code_pos = body_block_and_pos(
        constructor,
        len,
        token_len,
        sleigh_token_fields,
        sleigh_tables,
        verifications,
        &block_len,
        inst_start,
        root_tables,
        root_token_fields,
        token_parser.as_ref(),
        &tokens,
        context,
        produced_tables,
        produced_token_fields,
    );
    quote! {
        let mut #block_len = #token_len as #inst_work_type;
        #code_pre
        #disassembly
        #code_pos
        #pattern_len += #block_len;
        #tokens = &#tokens[usize::try_from(#block_len).unwrap()..];
    }
}

fn body_block_and_pre(
    constructor: &ConstructorStruct,
    _disassembly: Option<&sleigh_rs::Disassembly>,
    _len: &sleigh_rs::PatternLen,
    token_len: IntTypeU,
    _sleigh_token_fields: &[ProducedTokenField],
    _sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    _block_len: &Ident,
    inst_start: &Ident,
    _root_tables: &HashMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &HashMap<*const sleigh_rs::TokenField, Ident>,
    tokens: &Ident,
    context_instance: &Ident,
) -> (Option<Ident>, TokenStream) {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let (token_parser_name, token_parser_code) =
        token_parser_creation(constructor, token_len, &tokens)
            //FUTURE unzip
            .map(|(x, y)| (Some(x), Some(y)))
            .unwrap_or((None, None));
    //verify all the values and build all the tables
    let verifications = verifications.iter().filter_map(|ver| match ver {
        Verification::ContextCheck { op, value, .. }
        | Verification::TokenFieldCheck { op, value, .. } => {
            let value = BlockParserValuesDisassembly {
                disassembler: &disassembler,
                context_instance: &context_instance,
                inst_start,
                produced_fields: root_token_fields,
                token_parser: token_parser_name.as_ref(),
            }
            .expr(&value.expr());
            let cons_op = pattern_cmp_token_neg(op);
            let field = match ver {
                Verification::ContextCheck { context, .. } => disassembler
                    .memory
                    .spaces_trait
                    .build_context_disassembly_read_call(
                        &context_instance,
                        &context.element(),
                    ),
                Verification::TokenFieldCheck { field, .. } => {
                    disassembler.token_parser.gen_disassembly_read_call(
                        token_parser_name.as_ref().unwrap(),
                        field.element_ptr(),
                    )
                }
                _ => unreachable!(),
            };
            Some(quote! { #field #cons_op #value })
        }
        //table and sub_pattern is done on pos
        Verification::TableBuild { .. } | Verification::SubPattern { .. } => {
            None
        }
    });

    let code = quote! {
        #token_parser_code
        #(if #verifications {
            return None;
        })*
    };
    (token_parser_name, code)
}

fn body_block_and_pos(
    constructor: &ConstructorStruct,
    _len: &sleigh_rs::PatternLen,
    _token_len: IntTypeU,
    sleigh_token_fields: &[ProducedTokenField],
    _sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    block_len: &Ident,
    inst_start: &Ident,
    root_tables: &HashMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &HashMap<*const sleigh_rs::TokenField, Ident>,
    token_parser_name: Option<&Ident>,
    tokens: &Ident,
    context_instance: &Ident,
    produced_tables: &mut HashMap<*const sleigh_rs::Table, Ident>,
    produced_token_fields: &mut HashMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    //verify all the values and build all the tables
    let verifications: TokenStream = verifications
        .iter()
        .filter_map(|ver| match ver {
            //done in pre
            Verification::ContextCheck { .. }
            | Verification::TokenFieldCheck { .. } => None,
            Verification::TableBuild {
                produced_table,
                verification,
            } => {
                if verification.is_some() {
                    //TODO implement this
                    todo!("implement table return value verification")
                }
                let table = produced_table.table().element();
                let table_name = format_ident!("{}", from_sleigh(table.name()));
                produced_tables
                    .entry(table.element_ptr())
                    .and_modify(|_| unreachable!())
                    .or_insert_with(|| table_name.clone());
                let table_parsing = gen_table_parser(
                    &disassembler,
                    &context_instance,
                    &tokens,
                    &inst_start,
                    &table,
                );
                let inst_work_type = &disassembler.inst_work_type;
                let table_inner = format_ident!("table");
                let table_inner_output = if produced_table.recursive() {
                    //recursive need to go inside a box
                    quote! {Box::new(#table_inner)}
                } else {
                    table_inner.to_token_stream()
                };
                Some(quote! {
                    let #table_name = if let Some((len, #table_inner)) =
                            #table_parsing {
                        #block_len = #block_len.max(len as #inst_work_type);
                        #table_inner_output
                    } else {
                        return None;
                    };
                })
            }
            Verification::SubPattern { location, pattern } => {
                //TODO check recursive/always for table
                let sub_func = sub_pattern_closure(
                    constructor,
                    pattern,
                    inst_start,
                    root_tables,
                    root_token_fields,
                );
                let tables = pattern.tables().map(|table_field| {
                    let table = table_field.table().element();
                    let table_name =
                        format_ident!("{}", from_sleigh(table.name()));
                    produced_tables
                        .entry(table.element_ptr())
                        .and_modify(|_| unreachable!())
                        .or_insert_with(|| table_name.clone());

                    if table_field.recursive() {
                        //recursive need to go inside a box
                        quote! {Box::new(#table_name)}
                    } else {
                        table_name.to_token_stream()
                    }
                });
                let fields = pattern.token_fields().map(|prod_field| {
                    let field_name = format_ident!(
                        "{}",
                        from_sleigh(prod_field.token_field().name())
                    );
                    produced_token_fields
                        .entry(prod_field.token_field().element_ptr())
                        .and_modify(|_| unreachable!())
                        .or_insert_with(|| field_name.clone());
                    field_name
                });
                let sub_pattern_name =
                    format_ident!("sub_pattern_c{}", location.column);
                Some(quote! {
                    let mut #sub_pattern_name = #sub_func;
                    let (
                        (#(mut #tables),*),
                        (#(#fields),*),
                        sub_len
                     ) = #sub_pattern_name(#tokens, &mut #context_instance)?;
                     #block_len = #block_len.max(sub_len);
                })
            }
        })
        .collect();

    let fields = sleigh_token_fields
        .iter()
        .filter(|prod_field| prod_field.is_local())
        .map(|prod_field| {
            let token_field_name = format_ident!(
                "{}",
                from_sleigh(prod_field.token_field().name())
            );
            let (token_field, _token_field_type) =
                disassembler.token_parser.gen_read(
                    token_parser_name.as_ref().unwrap(),
                    prod_field.token_field().element_ptr(),
                );
            produced_token_fields
                .entry(prod_field.token_field().element_ptr())
                .and_modify(|_| unreachable!())
                .or_insert_with(|| token_field_name.clone());
            quote! { let #token_field_name = #token_field; }
        });
    quote! {
        #verifications
        #(#fields)*
    }
}

fn block_or_closure(
    constructor: &ConstructorStruct,
    _disassembly: Option<&sleigh_rs::Disassembly>,
    _len: &sleigh_rs::PatternLen,
    sleigh_token_fields: &[ProducedTokenField],
    sleigh_tables: &[ProducedTable],
    branches: &[Verification],
    _block_len: &Ident,
    inst_start: &Ident,
    root_tables: &HashMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &HashMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let tokens_param = format_ident!("tokens_param");
    let context_instance = format_ident!("context_param");
    let branches = branches.iter().map(|verification| {
        match verification {
            Verification::ContextCheck { context, op, value } => {
                let value = BlockParserValuesDisassembly {
                    disassembler: &disassembler,
                    context_instance: &context_instance,
                    inst_start,
                    produced_fields: root_token_fields,
                    token_parser: None,
                }
                .expr(value.expr());
                let cons_op = pattern_cmp_token(op);
                let context = context.element();
                let field = disassembler
                    .memory
                    .spaces_trait
                    .build_context_disassembly_read_call(
                        &context_instance,
                        &context,
                    );
                let inst_work_type = disassembler.inst_work_type;
                let tables = sleigh_tables.iter().map(|table_field| {
                    if table_field.always() {
                        unreachable!()
                    } else {
                        quote! {None}
                    }
                });
                quote! {
                    if #field #cons_op #value {
                        return Some((
                            (#(#tables),*),
                            (/*no fields*/),
                            0 as #inst_work_type,
                        ));
                    }
                }
            }
            Verification::TokenFieldCheck { field, op, value } => {
                //generate the token parser, if any
                let token_field = field.element();
                let token_len = token_field.token().len_bytes();
                let token_new = disassembler
                    .token_parser
                    .gen_new_call(token_len, &tokens_param);
                let token_parser_name = format_ident!("token_parser");
                let token_parser_creation = quote! {
                    let #token_parser_name = #token_new?;
                };

                let value = BlockParserValuesDisassembly {
                    disassembler: &disassembler,
                    context_instance: &context_instance,
                    inst_start,
                    produced_fields: root_token_fields,
                    token_parser: Some(&token_parser_name),
                }
                .expr(value.expr());
                let cons_op = pattern_cmp_token_neg(op);
                let token_field =
                    disassembler.token_parser.gen_disassembly_read_call(
                        &token_parser_name,
                        field.element_ptr(),
                    );
                let inst_work_type = disassembler.inst_work_type;
                //token fields is only exported if in the produced list
                let token_field_export = match sleigh_token_fields {
                    [prod] => {
                        if prod.token_field().element_ptr()
                            == field.element_ptr()
                        {
                            quote! { #token_field }
                        } else {
                            unreachable!()
                        }
                    }
                    [] => quote! {},
                    [..] => {
                        todo!("implicit field production in or pattern")
                    }
                };
                let tables = sleigh_tables.iter().map(|table_field| {
                    if table_field.always() {
                        unreachable!()
                    } else {
                        quote! {None}
                    }
                });
                let token_len = token_len.get();
                quote! {
                    #token_parser_creation
                    //verify the value, if true, return it
                    if #token_field #cons_op #value {
                        return Some((
                            (#(#tables),*),
                            (#token_field_export),
                            #inst_work_type::try_from(#token_len).unwrap(),
                        ));
                    }
                }
            }
            Verification::SubPattern { location, pattern } => {
                let sub_func = sub_pattern_closure(
                    constructor,
                    pattern,
                    inst_start,
                    root_tables,
                    root_token_fields,
                );
                let sub_pattern_name =
                    format_ident!("sub_pattern_c{}", location.column);
                let tables: HashMap<_, _> = pattern
                    .tables()
                    .map(|field_table| {
                        let table = field_table.table();
                        let table_struct = disassembler
                            .tables
                            .get(&table.element_ptr())
                            .unwrap();
                        let ptr = table.element_ptr();
                        let field = TableField {
                            name: format_ident!(
                                "{}",
                                from_sleigh(table.name())
                            ),
                            table: table_struct.clone(),
                            always: field_table.always(),
                            recursive: field_table.recursive(),
                        };
                        (ptr, field)
                    })
                    .collect();
                let fields: HashMap<_, _> = pattern
                    .token_fields()
                    .map(|field| {
                        let ptr = field.token_field().element_ptr();
                        let name = format_ident!(
                            "{}",
                            from_sleigh(field.token_field().name())
                        );
                        (ptr, name)
                    })
                    .collect();
                let sub_tables = tables.values().map(|table| &table.name);
                let sub_fields = fields.values();
                let tables = sleigh_tables.iter().map(|table_produced| {
                    //TODO what if the table have multiple levels of Option?
                    if let Some(table_sub) =
                        tables.get(&table_produced.table().element_ptr())
                    {
                        produced_table(
                            &table_sub.name,
                            table_sub.recursive,
                            table_sub.always,
                            table_produced.recursive(),
                            table_produced.always(),
                        )
                    } else {
                        if table_produced.always() {
                            unreachable!()
                        } else {
                            return quote! {None};
                        }
                    }
                });
                let fields = sleigh_token_fields.iter().map(|field| {
                    fields.get(&field.token_field().element_ptr()).unwrap()
                });
                quote! {
                    let mut #sub_pattern_name = #sub_func;
                    let mut context_current = #context_instance.clone();
                    if let Some((
                        (#(#sub_tables),*),
                        (#(#sub_fields),*),
                        sub_pattern_len,
                    )) = #sub_pattern_name(#tokens_param, &mut context_current) {
                        *#context_instance = context_current;
                        return Some((
                            (#(#tables),*),
                            (#(#fields),*),
                            sub_pattern_len,
                        ));
                    }
                }
            }
            Verification::TableBuild { .. } => {
                todo!("Table production in Or pattern directly")
            }
        }
    });
    quote! {
        |#tokens_param: &[u8], #context_instance: &mut T| {
            //used to calculate the current block len
            #(#branches)*
            None
        }
    }
}

fn produced_table(
    value: impl ToTokens,
    produced_recursive: bool,
    produced_always: bool,
    output_recursive: bool,
    output_always: bool,
) -> TokenStream {
    //if the source is recursive, the output is also recursive
    if produced_recursive != output_recursive {
        unreachable!();
    }
    let mut output = value.to_token_stream();
    if produced_recursive {
        //recursive need to go inside a box
        output = quote! {Box::new(#output)};
    }
    match (produced_always, output_always) {
        //source and output both always/sometimes produce
        (false, false) | (true, true) => output,
        //the source always produce, output sometimes produce
        (true, false) => quote! {Some(#output)},
        //the source sometimes produces, output always produce
        (false, true) => unreachable!(),
    }
}

fn pattern_cmp_token(
    value: &sleigh_rs::semantic::pattern::CmpOp,
) -> TokenStream {
    match value {
        sleigh_rs::semantic::pattern::CmpOp::Eq => quote!(==),
        sleigh_rs::semantic::pattern::CmpOp::Ne => quote!(!=),
        sleigh_rs::semantic::pattern::CmpOp::Lt => quote!(<),
        sleigh_rs::semantic::pattern::CmpOp::Gt => quote!(>),
        sleigh_rs::semantic::pattern::CmpOp::Le => quote!(<=),
        sleigh_rs::semantic::pattern::CmpOp::Ge => quote!(>=),
    }
}
fn pattern_cmp_token_neg(
    value: &sleigh_rs::semantic::pattern::CmpOp,
) -> TokenStream {
    match value {
        sleigh_rs::semantic::pattern::CmpOp::Eq => quote!(!=),
        sleigh_rs::semantic::pattern::CmpOp::Ne => quote!(==),
        sleigh_rs::semantic::pattern::CmpOp::Lt => quote!(>=),
        sleigh_rs::semantic::pattern::CmpOp::Gt => quote!(<=),
        sleigh_rs::semantic::pattern::CmpOp::Le => quote!(>),
        sleigh_rs::semantic::pattern::CmpOp::Ge => quote!(<),
    }
}

fn gen_table_parser(
    disassembler: &Disassembler,
    context_instance: &Ident,
    tokens: &Ident,
    inst_start: &Ident,
    table: &sleigh_rs::Table,
) -> TokenStream {
    let table_ptr: *const _ = table;
    let table = disassembler.tables.get(&table_ptr).unwrap();
    let table_enum = &table.enum_name;
    let table_parser = &table.parse_fun;
    quote! {
        #table_enum::#table_parser(
            #tokens,
            &mut #context_instance,
            #inst_start,
        )
    }
}

struct BlockParserValuesDisassembly<'a> {
    disassembler: &'a Disassembler,
    context_instance: &'a Ident,
    inst_start: &'a Ident,
    produced_fields: &'a HashMap<*const sleigh_rs::TokenField, Ident>,
    token_parser: Option<&'a Ident>,
}
//Block parser only use Disassembly for the pattern value, so only value is used
impl<'a, 'b> DisassemblyGenerator<'a> for BlockParserValuesDisassembly<'b> {
    fn global_set(&self, _global_set: &GlobalSet) -> TokenStream {
        unreachable!()
    }
    fn value(
        &self,
        value: &'a sleigh_rs::semantic::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::semantic::disassembly::*;
        match value {
            ReadScope::Integer(value) => {
                proc_macro2::Literal::u64_unsuffixed(*value).into_token_stream()
            }
            ReadScope::Context(context) => {
                let context = context.element();
                self.disassembler
                    .memory
                    .spaces_trait
                    .build_context_disassembly_read_call(
                        self.context_instance,
                        &context,
                    )
            }
            ReadScope::TokenField(ass) => {
                if let Some(token_field_name) =
                    self.produced_fields.get(&ass.element_ptr())
                {
                    let token_field_struct = self
                        .disassembler
                        .token_parser
                        .token_field_struct(ass.element_ptr());
                    let disassembly_fun = &token_field_struct.disassembly_fun;
                    quote! { #token_field_name.#disassembly_fun() }
                } else {
                    let token_parser =
                        self.token_parser.expect("Logical errror");
                    self.disassembler.token_parser.gen_disassembly_read_call(
                        token_parser,
                        ass.element_ptr(),
                    )
                }
            }
            ReadScope::InstStart(_) => self.inst_start.to_token_stream(),
            ReadScope::InstNext(_) | ReadScope::Local(_) => unreachable!(),
        }
    }
    fn set_context(
        &self,
        _context: &GlobalAnonReference<sleigh_rs::Context>,
        _value: TokenStream,
    ) -> TokenStream {
        unreachable!()
    }
    fn new_variable(&self, _var: &Rc<Variable>) -> TokenStream {
        unreachable!()
    }
    fn var_name(&self, _var: &Variable) -> TokenStream {
        unreachable!()
    }
}
