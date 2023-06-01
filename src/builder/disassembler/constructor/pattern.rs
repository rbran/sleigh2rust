use indexmap::IndexMap;

use crate::builder::formater::from_sleigh;
use crate::builder::{
    Disassembler, DisassemblyGenerator, ToLiteral, DISASSEMBLY_WORK_TYPE,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::disassembly::{self, Assertation, GlobalSet};
use sleigh_rs::pattern::{
    ConstraintValue, ProducedTable, ProducedTokenField, Verification,
};

use super::{ConstructorStruct, DisassemblyPattern};

pub fn root_pattern_function(
    parse_fun: &Ident,
    constructor_struct: &ConstructorStruct,
    disassembler: &Disassembler,
) -> TokenStream {
    let context_memory = &disassembler.context.name;
    let addr_type = &disassembler.addr_type;
    let inst_start = format_ident!("inst_start");
    let pattern_len = format_ident!("pattern_len");
    let tokens_current = format_ident!("tokens_current");
    let context_instance = format_ident!("context_instance");
    let mut disassembly_vars = IndexMap::new();
    let mut root_tables = IndexMap::new();
    let mut root_token_fields = IndexMap::new();

    //TODO remove this
    let mut disassembly = DisassemblyPattern {
        disassembler,
        context_instance: &context_instance,
        inst_start: &inst_start,
        root_tables: &root_tables,
        root_token_fields: &root_token_fields,
        vars: &mut disassembly_vars,
        tokens: &tokens_current,
    };
    let constructor = disassembler
        .sleigh
        .table(constructor_struct.table_id)
        .constructor(constructor_struct.constructor_id);
    let variables: TokenStream = constructor
        .pattern
        .disassembly_vars()
        .iter()
        .enumerate()
        .map(|(i, var)| {
            disassembly
                .new_variable(&sleigh_rs::disassembly::VariableId(i), var)
        })
        .collect();

    // dissable flat pattern if in debug mode
    let mut part_of_flat = !disassembler.debug;
    let blocks_parse: TokenStream = constructor
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
                constructor_struct,
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
                disassembler,
            )
        })
        .collect();

    //all tables produced are stored, always
    let table_fields = constructor_struct.table_fields.values();
    //only pass the token fields that need to be stored
    let token_fields = constructor_struct.ass_fields.values();
    let fields = table_fields.chain(token_fields);
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
    pattern: &sleigh_rs::pattern::Pattern,
    inst_start: &Ident,
    disassembly_vars: &mut IndexMap<disassembly::VariableId, Ident>,
    root_tables: &IndexMap<sleigh_rs::TableId, Ident>,
    root_token_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
    disassembler: &Disassembler,
) -> TokenStream {
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
                inst_start,
                &context_instance,
                &tokens_current,
                root_tables,
                root_token_fields,
                disassembly_vars,
                &mut produced_tables,
                &mut produced_token_fields,
                disassembler,
            )
        })
        .collect();
    let table_fields = pattern.produced_tables().map(|table_field| {
        produced_tables
            .get(&table_field.table)
            .expect("LOGIC_EROR: Table not produced")
    });
    let token_fields = pattern.produced_token_fields().map(|token_field| {
        produced_token_fields
            .get(&token_field.field)
            .expect("LOGIC_EROR: TokenField not produced")
    });
    let context_struct = &disassembler.context.name;
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
    block: &sleigh_rs::pattern::Block,
    pattern_len: &Ident,
    inst_start: &Ident,
    context_param: &Ident,
    tokens: &Ident,
    root_tables: &IndexMap<sleigh_rs::TableId, Ident>,
    root_token_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
    disassembly_vars: &mut IndexMap<disassembly::VariableId, Ident>,
    produced_tables: &mut IndexMap<sleigh_rs::TableId, Ident>,
    produced_fields: &mut IndexMap<sleigh_rs::TokenFieldId, Ident>,
    disassembler: &Disassembler,
) -> TokenStream {
    match block {
        sleigh_rs::pattern::Block::And {
            len,
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
            disassembler,
        ),
        sleigh_rs::pattern::Block::Or {
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
                disassembler,
            );
            //create the fields produced by this closure
            let fields = block.token_fields().iter().map(|field| {
                let token_field = disassembler.sleigh.token_field(field.field);
                let name = format_ident!("{}", from_sleigh(token_field.name()));
                produced_fields
                    .entry(field.field)
                    .and_modify(|_| unreachable!())
                    .or_insert_with(|| name.clone());
                name
            });
            let tables = block.tables().iter().map(|field| {
                let table = disassembler.sleigh.table(field.table);
                let name = format_ident!("{}", from_sleigh(table.name()));
                produced_tables
                    .entry(field.table)
                    .and_modify(|_| unreachable!())
                    .or_insert_with(|| name.clone());
                name
            });
            let block_name = format_ident!("block_{}", block_index);
            let block_len = format_ident!("block_{}_len", block_index);
            let disassembly = DisassemblyPattern {
                disassembler,
                context_instance: context_param,
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
    len: &sleigh_rs::pattern::PatternLen,
    sleigh_token_fields: &[ProducedTokenField],
    sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    pre: &[Assertation],
    pos: &[Assertation],
    pattern_len: &Ident,
    inst_start: &Ident,
    context: &Ident,
    tokens: &Ident,
    root_tables: &IndexMap<sleigh_rs::TableId, Ident>,
    root_token_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
    disassembly_vars: &mut IndexMap<disassembly::VariableId, Ident>,
    produced_tables: &mut IndexMap<sleigh_rs::TableId, Ident>,
    produced_token_fields: &mut IndexMap<sleigh_rs::TokenFieldId, Ident>,
    disassembler: &Disassembler,
) -> TokenStream {
    let block_len = format_ident!("block_{}_len", block_index);

    let code_pre = body_block_and_pre(
        constructor,
        len,
        part_of_flat,
        sleigh_token_fields,
        sleigh_tables,
        verifications,
        pre,
        &block_len,
        inst_start,
        root_tables,
        root_token_fields,
        disassembly_vars,
        tokens,
        context,
        disassembler,
    );
    let code_pos = body_block_and_pos(
        constructor,
        len,
        sleigh_token_fields,
        sleigh_tables,
        verifications,
        pos,
        &block_len,
        inst_start,
        root_tables,
        root_token_fields,
        disassembly_vars,
        tokens,
        context,
        produced_tables,
        produced_token_fields,
        disassembler,
    );
    let this_block_min_len = len.min().unsuffixed();
    quote! {
        let mut #block_len = #this_block_min_len;
        #code_pre
        #code_pos
        #pattern_len += #block_len;
        #tokens = &#tokens[usize::try_from(#block_len).unwrap()..];
    }
}

fn body_block_and_pre(
    _constructor: &ConstructorStruct,
    _len: &sleigh_rs::pattern::PatternLen,
    part_of_flat: bool,
    _sleigh_token_fields: &[ProducedTokenField],
    _sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    pre: &[Assertation],
    _block_len: &Ident,
    inst_start: &Ident,
    root_tables: &IndexMap<sleigh_rs::TableId, Ident>,
    root_token_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
    vars: &mut IndexMap<disassembly::VariableId, Ident>,
    tokens: &Ident,
    context_instance: &Ident,
    disassembler: &Disassembler,
) -> TokenStream {
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
                [sleigh_rs::disassembly::ExprElement::Value {
                    value: sleigh_rs::disassembly::ReadScope::Integer(_number),
                    location: _,
                }]
            ) =>
        {
            None
        }
        Verification::ContextCheck { context, op, value } => {
            Some(context_verification(
                true,
                *context,
                op,
                value,
                disassembler,
                tokens,
                context_instance,
                inst_start,
                root_token_fields,
            ))
        }
        Verification::TokenFieldCheck { field, op, value } => {
            Some(field_verification(
                true,
                *field,
                op,
                value,
                disassembler,
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
        disassembler,
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
    _len: &sleigh_rs::pattern::PatternLen,
    sleigh_token_fields: &[ProducedTokenField],
    _sleigh_tables: &[ProducedTable],
    verifications: &[Verification],
    pos: &[Assertation],
    block_len: &Ident,
    inst_start: &Ident,
    root_tables: &IndexMap<sleigh_rs::TableId, Ident>,
    root_token_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
    vars: &mut IndexMap<disassembly::VariableId, Ident>,
    tokens: &Ident,
    context_instance: &Ident,
    produced_tables: &mut IndexMap<sleigh_rs::TableId, Ident>,
    produced_token_fields: &mut IndexMap<sleigh_rs::TokenFieldId, Ident>,
    disassembler: &Disassembler,
) -> TokenStream {
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
                let table = disassembler.sleigh.table(produced_table.table);
                let table_name = format_ident!("{}", from_sleigh(table.name()));
                produced_tables
                    .entry(produced_table.table)
                    .and_modify(|_| unreachable!())
                    .or_insert_with(|| table_name.clone());
                let table_parsing = call_parser(
                    disassembler,
                    context_instance,
                    tokens,
                    inst_start,
                    produced_table.table,
                );
                let addr_type = &disassembler.addr_type;
                let table_inner = format_ident!("table");
                let table_inner_output = if produced_table.recursive {
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
                    disassembler,
                );
                let tables = pattern.produced_tables().map(|table_field| {
                    let table = disassembler.sleigh.table(table_field.table);
                    let table_name =
                        format_ident!("{}", from_sleigh(table.name()));
                    produced_tables
                        .entry(table_field.table)
                        .and_modify(|_| unreachable!())
                        .or_insert_with(|| table_name.clone());

                    if table_field.recursive {
                        //recursive need to go inside a box
                        quote! {Box::new(#table_name)}
                    } else {
                        table_name.to_token_stream()
                    }
                });
                let fields =
                    pattern.produced_token_fields().map(|prod_field| {
                        let token_field =
                            disassembler.sleigh.token_field(prod_field.field);
                        let field_name = format_ident!(
                            "{}",
                            from_sleigh(token_field.name())
                        );
                        produced_token_fields
                            .entry(prod_field.field)
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
        .filter(|prod_field| prod_field.local)
        .map(|prod_field| {
            let token_field = disassembler.sleigh.token_field(prod_field.field);
            let token_field_name =
                format_ident!("{}", from_sleigh(token_field.name()));
            let token_field =
                &disassembler.token_field_function(prod_field.field).read;
            produced_token_fields
                .entry(prod_field.field)
                .and_modify(|_| unreachable!())
                .or_insert_with(|| token_field_name.clone());
            quote! { let #token_field_name = #token_field(#tokens); }
        });

    let disassembly = DisassemblyPattern {
        disassembler,
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
    _len: &sleigh_rs::pattern::PatternLen,
    sleigh_token_fields: &[ProducedTokenField],
    sleigh_tables: &[ProducedTable],
    branches: &[Verification],
    _block_len: &Ident,
    inst_start: &Ident,
    vars: &mut IndexMap<disassembly::VariableId, Ident>,
    root_tables: &IndexMap<sleigh_rs::TableId, Ident>,
    root_token_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
    disassembler: &Disassembler,
) -> TokenStream {
    let tokens_param = format_ident!("tokens_param");
    let context_instance = format_ident!("context_param");
    let branches = branches.iter().map(|verification| {
        match verification {
            Verification::ContextCheck { context, op, value } => {
                let verification = context_verification(
                    false,
                    *context,
                    op,
                    value,
                    disassembler,
                    &tokens_param,
                    &context_instance,
                    inst_start,
                    root_token_fields,
                );
                let tables = sleigh_tables.iter().map(|table_field| {
                    if table_field.always {
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
                let token_field = disassembler.sleigh.token_field(*field);
                let token = disassembler.sleigh.token(token_field.token);
                let token_len = token.len_bytes;

                let verification = field_verification(
                    false,
                    *field,
                    op,
                    value,
                    disassembler,
                    &tokens_param,
                    &context_instance,
                    inst_start,
                    root_token_fields,
                );
                //token fields is only exported if in the produced list
                let token_field_export = match sleigh_token_fields {
                    [prod] => {
                        if prod.field == *field
                        {
                            let token_field_new = &disassembler
                                .token_field_function(*field).read;
                            quote! { #token_field_new(#tokens_param) }
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
                    if table_field.always {
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
                    disassembler,
                );
                let sub_pattern_name =
                    format_ident!("sub_pattern_c{}", location.start_column());
                let tables: IndexMap<_, _> = pattern
                    .produced_tables()
                    .map(|field_table| {
                        let table = field_table.table;
                        let table_name = disassembler.sleigh.table(table).name();
                         let table_name =   format_ident!(
                                "{}",
                                from_sleigh(table_name)
                            );
                        (field_table.table, (field_table, table_name))
                    })
                    .collect();
                let fields: IndexMap<_, _> = pattern
                    .produced_token_fields()
                    .map(|field| {
                        let token_field = disassembler.sleigh.token_field(field.field);
                        let name = format_ident!(
                            "{}",
                            from_sleigh(token_field.name())
                        );
                        (field.field, name)
                    })
                    .collect();
                let sub_tables = tables.values().map(|(_, name)| name);
                let sub_fields = fields.values();
                let tables = sleigh_tables.iter().map(|table_produced| {
                    //TODO what if the table have multiple levels of Option?
                    if let Some((table_sub, table_sub_name)) =
                        tables.get(&table_produced.table)
                    {
                        produced_table(
                            table_sub_name,
                            table_sub.recursive,
                            table_sub.always,
                            table_produced.recursive,
                            table_produced.always,
                        )
                    } else if table_produced.always {
                        unreachable!()
                    } else {
                        quote! {None}
                    }
                });
                let fields = sleigh_token_fields.iter().map(|field| {
                    fields.get(&field.field).unwrap()
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
    let context_struct = &disassembler.context.name;
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

fn call_parser(
    disassembler: &Disassembler,
    context_instance: &Ident,
    tokens: &Ident,
    inst_start: &Ident,
    table: sleigh_rs::TableId,
) -> TokenStream {
    let table = disassembler.table_struct(table);
    let table_enum = &table.name;
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
    produced_fields: &'a IndexMap<sleigh_rs::TokenFieldId, Ident>,
}
//Block parser only use Disassembly for the pattern value, so only value is used
impl DisassemblyGenerator for BlockParserValuesDisassembly<'_> {
    fn global_set(&self, _global_set: &GlobalSet) -> TokenStream {
        unreachable!()
    }
    fn value(&self, value: &sleigh_rs::disassembly::ReadScope) -> TokenStream {
        use sleigh_rs::disassembly::*;
        match value {
            ReadScope::Integer(value) => {
                value.signed_super().suffixed().into_token_stream()
            }
            ReadScope::Context(context) => {
                let read_call = self
                    .disassembler
                    .context
                    .read_call(*context, self.context_instance);
                quote! { #DISASSEMBLY_WORK_TYPE::try_from(#read_call).unwrap()}
            }
            ReadScope::TokenField(ass) => {
                let token_field_value = if let Some(token_field_name) =
                    self.produced_fields.get(ass)
                {
                    token_field_name.to_token_stream()
                } else {
                    let token_field_new =
                        &self.disassembler.token_field_function(*ass).read;
                    let tokens = self.tokens;
                    quote! { #token_field_new(#tokens) }
                };
                let token_field = self.disassembler.sleigh.token_field(*ass);
                let token_field_value =
                    self.disassembler.meanings.disassembly_function_call(
                        token_field.bits.len().get().try_into().unwrap(),
                        token_field_value,
                        token_field.meaning(),
                    );
                quote! {#DISASSEMBLY_WORK_TYPE::try_from(#token_field_value).unwrap()}
            }
            ReadScope::InstStart(_) => self.inst_start.to_token_stream(),
            ReadScope::InstNext(_) | ReadScope::Local(_) => unreachable!(),
        }
    }
    fn set_context(
        &self,
        _context: &sleigh_rs::ContextId,
        _value: TokenStream,
    ) -> TokenStream {
        unreachable!()
    }
    fn new_variable(
        &mut self,
        _var_id: &disassembly::VariableId,
        _var: &disassembly::Variable,
    ) -> TokenStream {
        unreachable!()
    }
    fn var_name(&self, _var: &disassembly::VariableId) -> TokenStream {
        unreachable!()
    }
}

fn value_disassembly(
    value: &ConstraintValue,
    disassembler: &Disassembler,
    tokens: &Ident,
    context_instance: &Ident,
    inst_start: &Ident,
    produced_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
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
    produced_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
) -> TokenStream {
    let cons_op = if inverted {
        pattern_cmp_token_neg(op)
    } else {
        pattern_cmp_token(op)
    };
    match value.expr().elements() {
        // if the value is a simple number, just compare it directly
        [disassembly::ExprElement::Value {
            value: disassembly::ReadScope::Integer(number),
            location: _,
        }] => {
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
    field: sleigh_rs::TokenFieldId,
    op: &sleigh_rs::pattern::CmpOp,
    value: &ConstraintValue,
    disassembler: &Disassembler,
    tokens: &Ident,
    context_instance: &Ident,
    inst_start: &Ident,
    produced_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
) -> TokenStream {
    let token_field_new = &disassembler.token_field_function(field).read;
    let field = quote! {#token_field_new(#tokens)};
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
    context: sleigh_rs::ContextId,
    op: &sleigh_rs::pattern::CmpOp,
    value: &ConstraintValue,
    disassembler: &Disassembler,
    tokens: &Ident,
    context_instance: &Ident,
    inst_start: &Ident,
    produced_fields: &IndexMap<sleigh_rs::TokenFieldId, Ident>,
) -> TokenStream {
    let field = disassembler.context.read_call(context, context_instance);
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
