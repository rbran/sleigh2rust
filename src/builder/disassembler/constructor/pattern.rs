use indexmap::IndexMap;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::disassembly::{self, Assertation, GlobalSet, Variable};
use sleigh_rs::pattern::{
    ConstraintValue, ProducedTable, ProducedTokenField, Verification,
};
use sleigh_rs::GlobalAnonReference;

use crate::builder::formater::from_sleigh;
use crate::builder::{
    Disassembler, DisassemblyGenerator, ToLiteral, DISASSEMBLY_WORK_TYPE,
};
use crate::IntTypeU;

use super::{ConstructorStruct, DisassemblyPattern, ParsedField, TableField};

pub fn root_pattern_function(
    parse_fun: &Ident,
    constructor: &ConstructorStruct,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let context_memory = &disassembler.context().name;
    let addr_type = &disassembler.addr_type();
    let inst_start = format_ident!("inst_start");
    let pattern_len = format_ident!("pattern_len");
    let tokens_current = format_ident!("tokens_current");
    let context_instance = format_ident!("context_instance");
    let mut disassembly_vars = IndexMap::new();
    let mut root_tables = IndexMap::new();
    let mut root_token_fields = IndexMap::new();

    //TODO remove this
    let mut disassembly = DisassemblyPattern {
        disassembler: disassembler.as_ref(),
        context_instance: &context_instance,
        inst_start: &inst_start,
        root_tables: &root_tables,
        root_token_fields: &root_token_fields,
        vars: &mut disassembly_vars,
        tokens: &tokens_current,
    };
    let variables: TokenStream = constructor
        .sleigh
        .pattern
        .disassembly_vars()
        .iter()
        .map(|var| disassembly.new_variable(var))
        .collect();

    // dissable flat pattern if in debug mode
    let mut part_of_flat = !disassembler.debug;
    let blocks_parse: TokenStream = constructor
        .sleigh
        .pattern
        .blocks()
        .iter()
        // mark blocks that are part of the calculated flat pattern
        .map(move |block| {
            let this_is_flat = part_of_flat;
            part_of_flat &= block.len().single_len().is_some();
            (block, this_is_flat)
        })
        .enumerate()
        .map(|(index, (block, part_of_flat))| {
            body_block(
                constructor,
                index,
                part_of_flat,
                block,
                &pattern_len,
                &inst_start,
                &context_instance,
                &tokens_current,
                &IndexMap::new(),
                &IndexMap::new(),
                &mut disassembly_vars,
                &mut root_tables,
                &mut root_token_fields,
            )
        })
        .collect();

    let table_fields = root_tables.iter().map(|(ptr, gen_name)| {
        let struct_field = constructor
            .table_fields
            .get(ptr)
            .expect("LOGIC_ERROR: Produced table is not part of the struct");
        let struct_field_name = &struct_field.name;
        (struct_field_name, gen_name)
    });
    let token_fields =
        root_token_fields
            .iter()
            .filter_map(|(ptr, gen_field_name)| {
                let struct_field = constructor.ass_fields.get(ptr)?;
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
                    quote! { #struct_field: #gen_field.try_into().unwrap() }
                }
            });
    quote! {
        fn #parse_fun(
            mut #tokens_current: &[u8],
            context: &mut #context_memory,
            #inst_start: #addr_type,
        ) -> Option<(#addr_type, Self)> {
            //each block will increment this value by its size
            let mut #pattern_len = 0;
            let mut #context_instance = context.clone();

            //disassembly_vars
            #variables

            //the current_token will be increseased by each block, so the
            //next block knows when to start parsing
            #blocks_parse
            //only on instruction table, otherwise this is on a function
            *context = #context_instance;
            Some((
                #pattern_len,
                Self{ #(#fields),* },
            ))
        }
    }
}

fn sub_pattern_closure(
    constructor: &ConstructorStruct,
    pattern: &sleigh_rs::Pattern,
    inst_start: &Ident,
    disassembly_vars: &mut IndexMap<
        *const disassembly::Variable,
        ParsedField<Rc<Variable>>,
    >,
    root_tables: &IndexMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let tokens_current = format_ident!("tokens");
    let pattern_len = format_ident!("pattern_len");
    let context_instance = format_ident!("context_instance");
    let mut produced_tables = IndexMap::new();
    let mut produced_token_fields = IndexMap::new();

    let blocks_parse: TokenStream = pattern
        .blocks()
        .iter()
        .enumerate()
        .map(|(index, block)| {
            body_block(
                constructor,
                index,
                false,
                block,
                &pattern_len,
                &inst_start,
                &context_instance,
                &tokens_current,
                root_tables,
                root_token_fields,
                disassembly_vars,
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
    let context_struct = &disassembler.context().name;
    quote! {
        |tokens: &[u8], context_param: &mut #context_struct| {
            //each block will increment this value by its size
            let mut #pattern_len = 0;
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

fn body_block(
    constructor: &ConstructorStruct,
    block_index: usize,
    part_of_flat: bool,
    block: &sleigh_rs::Block,
    pattern_len: &Ident,
    inst_start: &Ident,
    context_param: &Ident,
    tokens: &Ident,
    root_tables: &IndexMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
    disassembly_vars: &mut IndexMap<
        *const disassembly::Variable,
        ParsedField<Rc<Variable>>,
    >,
    produced_tables: &mut IndexMap<*const sleigh_rs::Table, Ident>,
    produced_fields: &mut IndexMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    match block {
        sleigh_rs::Block::And {
            len,
            token_len,
            token_fields,
            tables,
            verifications,
            pre,
            pos,
            variants_prior: _,
            variants_number: _,
        } => body_block_and(
            constructor,
            block_index,
            part_of_flat,
            len,
            *token_len,
            token_fields,
            tables,
            verifications,
            pre,
            pos,
            pattern_len,
            inst_start,
            context_param,
            tokens,
            root_tables,
            root_token_fields,
            disassembly_vars,
            produced_tables,
            produced_fields,
        ),
        sleigh_rs::Block::Or {
            len,
            token_fields,
            tables,
            branches,
            pos,
            variants_prior: _,
            variants_number: _,
        } => {
            let block_closure = block_or_closure(
                constructor,
                len,
                token_fields,
                tables,
                branches,
                pattern_len,
                inst_start,
                disassembly_vars,
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
            let disassembler = constructor.disassembler.upgrade().unwrap();
            let disassembly = DisassemblyPattern {
                disassembler: disassembler.as_ref(),
                context_instance: &context_param,
                inst_start,
                root_tables,
                root_token_fields,
                vars: disassembly_vars,
                tokens,
            };
            let disassembly = disassembly.disassembly(&mut pos.iter());
            quote! {
                let #block_name = #block_closure;
                let (
                    (#(#tables),*),
                    (#(#fields),*),
                    #block_len,
                ) = #block_name(#tokens, &mut #context_param)?;
                #disassembly
                #pattern_len += #block_len;
                #tokens = &#tokens[usize::try_from(#block_len).unwrap()..];
            }
        }
    }
}

fn body_block_and(
    constructor: &ConstructorStruct,
    block_index: usize,
    part_of_flat: bool,
    len: &sleigh_rs::PatternLen,
    token_len: IntTypeU,
    sleigh_token_fields: &[ProducedTokenField],
    sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    pre: &[Assertation],
    pos: &[Assertation],
    pattern_len: &Ident,
    inst_start: &Ident,
    context: &Ident,
    tokens: &Ident,
    root_tables: &IndexMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
    disassembly_vars: &mut IndexMap<
        *const disassembly::Variable,
        ParsedField<Rc<Variable>>,
    >,
    produced_tables: &mut IndexMap<*const sleigh_rs::Table, Ident>,
    produced_token_fields: &mut IndexMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let block_len = format_ident!("block_{}_len", block_index);

    let code_pre = body_block_and_pre(
        constructor,
        len,
        part_of_flat,
        token_len,
        sleigh_token_fields,
        sleigh_tables,
        verifications,
        pre,
        &block_len,
        inst_start,
        root_tables,
        root_token_fields,
        disassembly_vars,
        &tokens,
        context,
    );
    let code_pos = body_block_and_pos(
        constructor,
        len,
        token_len,
        sleigh_token_fields,
        sleigh_tables,
        verifications,
        pos,
        &block_len,
        inst_start,
        root_tables,
        root_token_fields,
        disassembly_vars,
        &tokens,
        context,
        produced_tables,
        produced_token_fields,
    );
    let token_len = token_len.unsuffixed();
    quote! {
        let mut #block_len = #token_len;
        #code_pre
        #code_pos
        #pattern_len += #block_len;
        #tokens = &#tokens[usize::try_from(#block_len).unwrap()..];
    }
}

fn body_block_and_pre(
    constructor: &ConstructorStruct,
    _len: &sleigh_rs::PatternLen,
    part_of_flat: bool,
    _token_len: IntTypeU,
    _sleigh_token_fields: &[ProducedTokenField],
    _sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    pre: &[Assertation],
    _block_len: &Ident,
    inst_start: &Ident,
    root_tables: &IndexMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
    vars: &mut IndexMap<
        *const disassembly::Variable,
        ParsedField<Rc<Variable>>,
    >,
    tokens: &Ident,
    context_instance: &Ident,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();

    //verify all the values and build all the tables
    let verifications = verifications.iter().filter_map(|ver| match ver {
        // simple eq verifications can be ignored if part of the flat pattern
        Verification::TokenFieldCheck {
            field: _,
            op: sleigh_rs::pattern::CmpOp::Eq,
            value,
        }
        | Verification::ContextCheck {
            context: _,
            op: sleigh_rs::pattern::CmpOp::Eq,
            value,
        } if part_of_flat
            && matches!(
                value.expr().elements(),
                [sleigh_rs::disassembly::ExprElement::Value(
                    sleigh_rs::disassembly::ReadScope::Integer(_number)
                )]
            ) =>
        {
            None
        }
        Verification::ContextCheck { context, op, value } => {
            Some(context_verification(
                true,
                &context.element(),
                op,
                value,
                &*disassembler,
                tokens,
                context_instance,
                inst_start,
                root_token_fields,
            ))
        }
        Verification::TokenFieldCheck { field, op, value } => {
            Some(field_verification(
                true,
                &field.element(),
                op,
                value,
                &*disassembler,
                tokens,
                context_instance,
                inst_start,
                root_token_fields,
            ))
        }
        //table and sub_pattern is done on pos
        Verification::TableBuild { .. } | Verification::SubPattern { .. } => {
            None
        }
    });

    let disassembly = DisassemblyPattern {
        disassembler: &*disassembler,
        context_instance,
        inst_start,
        root_tables,
        root_token_fields,
        vars,
        tokens,
    };
    let disassembly = disassembly.disassembly(&mut pre.iter());

    let code = quote! {
        #(if #verifications {
            return None;
        })*
        #disassembly
    };
    code
}

fn body_block_and_pos(
    constructor: &ConstructorStruct,
    _len: &sleigh_rs::PatternLen,
    _token_len: IntTypeU,
    sleigh_token_fields: &[ProducedTokenField],
    _sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    pos: &[Assertation],
    block_len: &Ident,
    inst_start: &Ident,
    root_tables: &IndexMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
    vars: &mut IndexMap<
        *const disassembly::Variable,
        ParsedField<Rc<Variable>>,
    >,
    tokens: &Ident,
    context_instance: &Ident,
    produced_tables: &mut IndexMap<*const sleigh_rs::Table, Ident>,
    produced_token_fields: &mut IndexMap<*const sleigh_rs::TokenField, Ident>,
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
                    &*disassembler,
                    &context_instance,
                    &tokens,
                    &inst_start,
                    &table,
                );
                let addr_type = disassembler.addr_type();
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
                        #block_len = #block_len.max(len as #addr_type);
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
                    vars,
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
                    format_ident!("sub_pattern_c{}", location.start_column());
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
            let token_field = disassembler
                .token_field(prod_field.token_field().element_ptr())
                .unwrap();
            let token_field = token_field.inline_value(tokens);
            produced_token_fields
                .entry(prod_field.token_field().element_ptr())
                .and_modify(|_| unreachable!())
                .or_insert_with(|| token_field_name.clone());
            quote! { let #token_field_name = #token_field; }
        });

    let disassembly = DisassemblyPattern {
        disassembler: &*disassembler,
        context_instance,
        inst_start,
        root_tables,
        root_token_fields,
        vars,
        tokens,
    };
    let disassembly = disassembly.disassembly(&mut pos.iter());

    quote! {
        #verifications
        #(#fields)*
        #disassembly
    }
}

fn block_or_closure(
    constructor: &ConstructorStruct,
    _len: &sleigh_rs::PatternLen,
    sleigh_token_fields: &[ProducedTokenField],
    sleigh_tables: &[ProducedTable],
    branches: &[Verification],
    _block_len: &Ident,
    inst_start: &Ident,
    vars: &mut IndexMap<
        *const disassembly::Variable,
        ParsedField<Rc<Variable>>,
    >,
    root_tables: &IndexMap<*const sleigh_rs::Table, Ident>,
    root_token_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let disassembler = constructor.disassembler.upgrade().unwrap();
    let tokens_param = format_ident!("tokens_param");
    let context_instance = format_ident!("context_param");
    let branches = branches.iter().map(|verification| {
        match verification {
            Verification::ContextCheck { context, op, value } => {
                let context = context.element();
                let verification = context_verification(
                    false,
                    &context,
                    op,
                    value,
                    &*disassembler,
                    &tokens_param,
                    &context_instance,
                    inst_start,
                    root_token_fields,
                );
                let tables = sleigh_tables.iter().map(|table_field| {
                    if table_field.always() {
                        unreachable!()
                    } else {
                        quote! {None}
                    }
                });
                quote! {
                    if #verification {
                        return Some((
                            (#(#tables),*),
                            (/*no fields*/),
                            0,
                        ));
                    }
                }
            }
            Verification::TokenFieldCheck { field, op, value } => {
                //generate the token parser, if any
                let token_field = field.element();
                let token_len = token_field.token().len_bytes();

                let verification = field_verification(
                    false,
                    &token_field,
                    op,
                    value,
                    &*disassembler,
                    &tokens_param,
                    &context_instance,
                    inst_start,
                    root_token_fields,
                );
                //token fields is only exported if in the produced list
                let token_field_export = match sleigh_token_fields {
                    [prod] => {
                        if prod.token_field().element_ptr()
                            == field.element_ptr()
                        {
                            let token_field = disassembler
                                .token_field(field.element_ptr())
                                .unwrap();
                            let token_field = token_field.inline_value(&tokens_param);
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
                let token_len = token_len.get().unsuffixed();
                quote! {
                    //verify the value, if true, return it
                    if #verification {
                        return Some((
                            (#(#tables),*),
                            (#token_field_export),
                            #token_len,
                        ));
                    }
                }
            }
            Verification::SubPattern { location, pattern } => {
                let sub_func = sub_pattern_closure(
                    constructor,
                    pattern,
                    inst_start,
                    vars,
                    root_tables,
                    root_token_fields,
                );
                let sub_pattern_name =
                    format_ident!("sub_pattern_c{}", location.start_column());
                let tables: IndexMap<_, _> = pattern
                    .tables()
                    .map(|field_table| {
                        let table = field_table.table();
                        let table_struct = disassembler
                            .table(table.element_ptr())
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
                let fields: IndexMap<_, _> = pattern
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
    let context_struct = &disassembler.context().name;
    quote! {
        |#tokens_param: &[u8], #context_instance: &mut #context_struct| {
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

fn pattern_cmp_token(value: &sleigh_rs::pattern::CmpOp) -> TokenStream {
    match value {
        sleigh_rs::pattern::CmpOp::Eq => quote!(==),
        sleigh_rs::pattern::CmpOp::Ne => quote!(!=),
        sleigh_rs::pattern::CmpOp::Lt => quote!(<),
        sleigh_rs::pattern::CmpOp::Gt => quote!(>),
        sleigh_rs::pattern::CmpOp::Le => quote!(<=),
        sleigh_rs::pattern::CmpOp::Ge => quote!(>=),
    }
}
fn pattern_cmp_token_neg(value: &sleigh_rs::pattern::CmpOp) -> TokenStream {
    match value {
        sleigh_rs::pattern::CmpOp::Eq => quote!(!=),
        sleigh_rs::pattern::CmpOp::Ne => quote!(==),
        sleigh_rs::pattern::CmpOp::Lt => quote!(>=),
        sleigh_rs::pattern::CmpOp::Gt => quote!(<=),
        sleigh_rs::pattern::CmpOp::Le => quote!(>),
        sleigh_rs::pattern::CmpOp::Ge => quote!(<),
    }
}

fn gen_table_parser(
    disassembler: &Disassembler,
    context_instance: &Ident,
    tokens: &Ident,
    inst_start: &Ident,
    table: &sleigh_rs::Table,
) -> TokenStream {
    let table = disassembler.table(table).unwrap();
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
    tokens: &'a Ident,
    context_instance: &'a Ident,
    inst_start: &'a Ident,
    produced_fields: &'a IndexMap<*const sleigh_rs::TokenField, Ident>,
}
//Block parser only use Disassembly for the pattern value, so only value is used
impl<'a, 'b> DisassemblyGenerator<'a> for BlockParserValuesDisassembly<'b> {
    fn global_set(&self, _global_set: &GlobalSet) -> TokenStream {
        unreachable!()
    }
    fn value(
        &self,
        value: &'a sleigh_rs::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::disassembly::*;
        match value {
            ReadScope::Integer(value) => {
                value.signed_super().suffixed().into_token_stream()
            }
            ReadScope::Context(context) => {
                let context = context.element();
                let read_call = self
                    .disassembler
                    .context()
                    .read_call(&context, self.context_instance);
                quote! { #DISASSEMBLY_WORK_TYPE::try_from(#read_call).unwrap()}
            }
            ReadScope::TokenField(ass) => {
                if let Some(token_field_name) =
                    self.produced_fields.get(&ass.element_ptr())
                {
                    quote! {#DISASSEMBLY_WORK_TYPE::try_from(#token_field_name).unwrap()}
                } else {
                    let token_field = self
                        .disassembler
                        .token_field(ass.element_ptr())
                        .unwrap();
                    let value = token_field.inline_value(self.tokens);
                    quote! { #DISASSEMBLY_WORK_TYPE::try_from(#value).unwrap() }
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
    fn new_variable(&mut self, _var: &Rc<Variable>) -> TokenStream {
        unreachable!()
    }
    fn var_name(&self, _var: &Variable) -> TokenStream {
        unreachable!()
    }
}

fn value_disassembly(
    value: &ConstraintValue,
    disassembler: &Disassembler,
    tokens: &Ident,
    context_instance: &Ident,
    inst_start: &Ident,
    produced_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    BlockParserValuesDisassembly {
        disassembler,
        context_instance,
        inst_start,
        produced_fields,
        tokens,
    }
    .expr(value.expr())
}

fn verification(
    inverted: bool,
    field: TokenStream,
    op: &sleigh_rs::pattern::CmpOp,
    value: &ConstraintValue,
    disassembler: &Disassembler,
    tokens: &Ident,
    context_instance: &Ident,
    inst_start: &Ident,
    produced_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let cons_op = if inverted {
        pattern_cmp_token_neg(op)
    } else {
        pattern_cmp_token(op)
    };
    match value.expr().elements() {
        // if the value is a simple number, just compare it directly
        [disassembly::ExprElement::Value(disassembly::ReadScope::Integer(
            number,
        ))] => {
            let value = number.unsuffixed();
            quote! {#field #cons_op #value}
        }
        //otherwise make a full disassembly and compare it
        _ => {
            let value = value_disassembly(
                value,
                disassembler,
                tokens,
                context_instance,
                inst_start,
                produced_fields,
            );
            quote! { #DISASSEMBLY_WORK_TYPE::try_from(#field).unwrap() #cons_op #value }
        }
    }
}

fn field_verification(
    inverted: bool,
    field: &sleigh_rs::TokenField,
    op: &sleigh_rs::pattern::CmpOp,
    value: &ConstraintValue,
    disassembler: &Disassembler,
    tokens: &Ident,
    context_instance: &Ident,
    inst_start: &Ident,
    produced_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let token_field = disassembler.token_field(field).unwrap();
    let field = token_field.inline_value(tokens);
    verification(
        inverted,
        field,
        op,
        value,
        disassembler,
        tokens,
        context_instance,
        inst_start,
        produced_fields,
    )
}

fn context_verification(
    inverted: bool,
    context: &sleigh_rs::Context,
    op: &sleigh_rs::pattern::CmpOp,
    value: &ConstraintValue,
    disassembler: &Disassembler,
    tokens: &Ident,
    context_instance: &Ident,
    inst_start: &Ident,
    produced_fields: &IndexMap<*const sleigh_rs::TokenField, Ident>,
) -> TokenStream {
    let field = disassembler.context().read_call(context, context_instance);
    verification(
        inverted,
        field,
        op,
        value,
        disassembler,
        tokens,
        context_instance,
        inst_start,
        produced_fields,
    )
}
