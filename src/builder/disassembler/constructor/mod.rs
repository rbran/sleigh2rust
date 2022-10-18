use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::semantic::disassembly::{GlobalSet, Variable};
use sleigh_rs::semantic::pattern::{
    FieldAnd, FieldOr, FieldProductTable, FieldProducts,
};
use sleigh_rs::semantic::{Meaning, PrintFmt};
use sleigh_rs::{Assembly, Block, IntTypeS, NonZeroTypeU, Varnode};

use crate::builder::formater::from_sleigh;
use crate::builder::{DisassemblyGenerator, TokenParser, WorkType};

use super::{Disassembler, Table};

mod disassembly;
pub use disassembly::*;

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

#[derive(Debug, Clone)]
pub struct ParsedField<T> {
    name: Ident,
    sleigh: T,
}
impl<T> ParsedField<T> {
    pub fn new(name: Ident, sleigh: T) -> Self {
        Self { name, sleigh }
    }
    pub fn value_type(&self) -> &T {
        &self.sleigh
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
}

impl<T> std::ops::Deref for ParsedField<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.value_type()
    }
}

#[derive(Debug, Clone)]
pub struct Constructor<'a> {
    sleigh: &'a sleigh_rs::Constructor,
    //struct name
    struct_name: Ident,
    //variant name in the enum
    variant_name: Ident,
    //parse function
    parse: Ident,
    //display function
    display: Ident,
    disassembly: Option<Ident>,
    table_fields:
        HashMap<*const sleigh_rs::Table, ParsedField<FieldProductTable>>,
    pub ass_fields:
        HashMap<*const Assembly, ParsedField<&'a sleigh_rs::Assembly>>,
    //TODO instead of having calc fields, have a constructor phase 1/2, with
    //the display/execution values in phase2 and values required by the
    //disassembly in phase1
    //Alternativally, having the having display call execute all the disassembly
    //as this will never be in the disassembly_pre
    pub calc_fields: HashMap<*const Variable, ParsedField<&'a Variable>>,
}
impl<'a> Constructor<'a> {
    pub fn new(
        sleigh: &'a sleigh_rs::Constructor,
        table: &'_ Table,
        number: usize,
    ) -> Self {
        let mut table_fields = HashMap::new();
        let mut calc_fields = HashMap::new();
        let mut ass_fields: HashMap<*const Assembly, _> = HashMap::new();
        let mut add_ass = |ass: &'a Assembly| {
            ass_fields.entry(ass).or_insert_with(|| {
                let name = format_ident!("{}", from_sleigh(ass.name.as_ref()));
                ParsedField::new(name, ass)
            });
        };
        //include on the enum all the required fields from the display
        for display in sleigh.display.elements().iter() {
            use sleigh_rs::semantic::display::Element::*;
            match display {
                Varnode(_) | Literal(_) => (),
                Assembly(ass) => {
                    add_ass(ass);
                }
                Disassembly(var) => {
                    let ptr = Rc::as_ptr(var);
                    calc_fields.entry(ptr).or_insert_with(|| {
                        let name =
                            format_ident!("{}", from_sleigh(var.name.as_ref()));
                        ParsedField::new(name, var.as_ref())
                    });
                }
                Table(display_table) => {
                    let ptr = Rc::as_ptr(display_table);
                    table_fields.entry(ptr).or_insert_with(|| {
                        let name = format_ident!(
                            "{}",
                            from_sleigh(display_table.name.as_ref())
                        );
                        let field = sleigh
                            .pattern
                            .produced()
                            .tables()
                            .iter()
                            .find(|table| {
                                Rc::as_ptr(table.table())
                                    == Rc::as_ptr(display_table)
                            })
                            .expect("Source of display field is unknown");
                        ParsedField::new(name, field.clone())
                    });
                }
            }
        }
        //also include all the fields requied by the pos disassembly
        for field in sleigh
            .disassembly
            .pos
            .then(|| sleigh.disassembly.assertations.iter())
            .unwrap_or_else(|| [].iter())
        {
            use sleigh_rs::semantic::disassembly;
            match field {
                disassembly::Assertation::GlobalSet(GlobalSet {
                    address,
                    context: _,
                }) => {
                    use sleigh_rs::semantic::assembly::AssemblyType::*;
                    use sleigh_rs::semantic::disassembly::AddrScope::*;
                    match address {
                        Int(_) | Table(_) | Varnode(_) | Local(_) => (),
                        Assembly(ass)
                            if matches!(ass.assembly_type, Next(_)) =>
                        {
                            ()
                        }
                        Assembly(ass) => {
                            todo!("How globalset ass works?: {:?}", ass)
                        }
                    }
                }
                disassembly::Assertation::Assignment(
                    disassembly::Assignment { left: _, right },
                ) => {
                    for ele in right.rpn.iter() {
                        use sleigh_rs::semantic::assembly::AssemblyType::*;
                        use sleigh_rs::semantic::disassembly::ExprElement::*;
                        use sleigh_rs::semantic::disassembly::ReadScope::*;
                        match ele {
                            Value(value) => match value {
                                Integer(_) | Varnode(_) | Local(_) => (),
                                Assembly(ass)
                                    if matches!(ass.assembly_type, Next(_)) =>
                                {
                                    ()
                                }
                                Assembly(ass) => add_ass(ass),
                            },
                            Op(_) | OpUnary(_) => (),
                        }
                    }
                }
            }
        }
        let variant_name = format_ident!("Var{}", number);
        let struct_name = format_ident!("{}Var{}", table.name(), number);
        let parse = format_ident!("parse");
        let display = format_ident!("display_extend");
        let disassembly =
            (!table.sleigh().is_root()).then(|| format_ident!("disassembly"));
        Self {
            sleigh,
            variant_name,
            struct_name,
            parse,
            display,
            disassembly,
            table_fields,
            ass_fields,
            calc_fields,
        }
    }
    pub fn is_root(&self) -> bool {
        //only root and constructors table don't have disassembly
        self.disassembly.is_none()
    }
    pub fn sleigh(&self) -> &'a sleigh_rs::Constructor {
        self.sleigh
    }
    pub fn variant_name(&self) -> &Ident {
        &self.variant_name
    }
    pub fn struct_name(&self) -> &Ident {
        &self.struct_name
    }
    pub fn parse(&self) -> &Ident {
        &self.parse
    }
    pub fn display(&self) -> &Ident {
        &self.display
    }
    pub fn disassembly(&self) -> Option<&Ident> {
        self.disassembly.as_ref()
    }
    pub fn table_fields(
        &self,
    ) -> impl Iterator<Item = &ParsedField<FieldProductTable>> {
        self.table_fields.values()
    }
    pub fn calc_fields(
        &self,
    ) -> impl Iterator<Item = &ParsedField<&'a Variable>> {
        self.calc_fields.values()
    }
    pub fn ass_fields(
        &self,
    ) -> impl Iterator<Item = &ParsedField<&'a Assembly>> {
        self.ass_fields.values()
    }
    pub fn gen_match_fields<'b>(&'b self) -> impl Iterator<Item = &Ident> + 'b {
        let tables = self.table_fields().map(|field| field.name());
        let calc = self.calc_fields().map(|field| field.name());
        let ass = self.ass_fields().map(|field| field.name());
        tables.chain(calc).chain(ass)
    }
}
fn token_len_from_fields_and<'a>(
    fields: &'a [FieldAnd],
) -> Option<NonZeroTypeU> {
    use sleigh_rs::semantic::pattern::{ConstraintVariable, Reference};
    fields
        .iter()
        .filter_map(|field| match field {
            FieldAnd::Constraint {
                field,
                constraint: _,
            } => match field {
                ConstraintVariable::Assembly { src: _, assembly } => {
                    Some(assembly.token_len())
                }
                ConstraintVariable::Varnode { .. } => None,
            },
            FieldAnd::Field(field) => match field {
                Reference::Assembly { src: _, assembly } => {
                    Some(assembly.token_len())
                }
                Reference::Varnode { .. } | Reference::Table { .. } => None,
            },
            FieldAnd::SubPattern { src: _, sub: _ } => None,
        })
        .max()
        .map(|len| NonZeroTypeU::new(len / 8))
        .flatten()
}

#[derive(Clone, Debug)]
pub enum PatternProduct<'a> {
    Ass(&'a Assembly),
    Table(FieldProductTable),
}

struct BlockParser<'a, 'b> {
    disassembler: &'b Disassembler<'a>,
    block_len: Ident,

    token_param: Ident,

    global_set_param: &'b Ident,
    context_param: Ident,
    context_current: Ident,
    inst_start: &'b Ident,

    constructor: &'b Constructor<'a>,

    block: &'a Block,
    //table and ass variables can't be duplicated, and they will be later used
    //by disassembly/execution
    //table_vars:
    //    HashMap<*const sleigh_rs::Table, ParsedField<FieldProductTable>>,
    //context variables need to be read for each block, because a table
    //sub parser could modify the context.
    //context_vars: HashMap<*const Varnode, ParsedField<&'a Varnode>>,

    //variables containing the produced values from the pattern
    produced_tables: HashMap<*const sleigh_rs::Table, Ident>,
    produced_fields: HashMap<*const Assembly, Ident>,
    //value_extraction: TokenStream,
}
impl<'a, 'b> BlockParser<'a, 'b> {
    pub fn generate_block(
        disassembler: &'b Disassembler<'a>,
        global_set_param: &'b Ident,
        inst_start: &'b Ident,
        constructor: &'b Constructor<'a>,
        block: &'a Block,
    ) -> TokenStream {
        let mut helper = BlockParser::new(
            disassembler,
            global_set_param,
            inst_start,
            constructor,
            block,
        );
        helper.process()
    }
    pub fn new(
        disassembler: &'b Disassembler<'a>,
        global_set_param: &'b Ident,
        inst_start: &'b Ident,
        constructor: &'b Constructor<'a>,
        block: &'a Block,
    ) -> Self {
        let produced_tables = block
            .produced()
            .tables()
            .iter()
            .map(|field_table| {
                let table = field_table.table().as_ref();
                (
                    table as *const _,
                    format_ident!("{}", from_sleigh(&table.name)),
                )
            })
            .collect();
        let produced_fields = block
            .produced()
            .fields()
            .iter()
            .map(|field| {
                (
                    field.as_ref() as *const _,
                    format_ident!("{}", from_sleigh(&field.name)),
                )
            })
            .collect();
        BlockParser {
            disassembler,
            block_len: format_ident!("block_len"),
            global_set_param,
            token_param: format_ident!("tokens"),
            context_param: format_ident!("context"),
            context_current: format_ident!("context_current"),
            inst_start,
            constructor,
            block,
            produced_tables,
            produced_fields,
        }
    }
    fn token_parser(
        &self,
        len_bytes: NonZeroTypeU,
    ) -> Option<&'b TokenParser<'a>> {
        self.disassembler.token_parsers.get(&len_bytes)
    }
    fn gen_new_token_parser(
        &self,
        token_parser: &'b TokenParser<'a>,
    ) -> TokenStream {
        let creator = token_parser.creator();

        let token_struct = token_parser.name();
        let token_param = &self.token_param;
        quote! {
            #token_struct::#creator(#token_param)?
        }
    }
    //create the variable (if necessary) and return its name
    fn gen_ass_field<'c>(
        &'c self,
        ass: &'a Assembly,
        token_parser: Option<&'b TokenParser<'a>>,
        token_parser_name: &Ident,
    ) -> Option<TokenStream> {
        use sleigh_rs::semantic::assembly::AssemblyType::*;
        let ass_ptr: *const _ = ass;
        match &ass.assembly_type {
            Epsilon => None,
            Start(_) => None,
            Next(_) => unreachable!("inst_next before the pattern match?"),
            Field(_) => {
                //if this fields is produced, create a var for it, otherwise
                //do nothing
                self.produced_fields.get(&ass_ptr).map(|name| {
                    let token_field = token_parser.unwrap().field(ass);
                    let token_read_name = token_field.read();
                    quote! {
                        let #name = #token_parser_name.#token_read_name();
                    }
                })
            }
        }
    }
    fn get_ass_field_value(
        &self,
        ass: &'a Assembly,
        token_parser: Option<&'b TokenParser<'a>>,
        token_parser_name: &Ident,
    ) -> TokenStream {
        match &ass.assembly_type {
            sleigh_rs::semantic::assembly::AssemblyType::Epsilon => {
                todo!("epsilon?")
            }
            sleigh_rs::semantic::assembly::AssemblyType::Start(_) => {
                self.inst_start.to_token_stream()
            }
            sleigh_rs::semantic::assembly::AssemblyType::Next(_) => {
                todo!("inst_next before the pattern match?")
            }
            sleigh_rs::semantic::assembly::AssemblyType::Field(_) => {
                //check if this field was already created, otherwise create it
                let ass_ptr: *const _ = ass;
                match self.produced_fields.get(&ass_ptr) {
                    None => {
                        let token_field = token_parser.unwrap().field(ass);
                        let token_read_name = token_field.read();
                        quote! {
                            #token_parser_name.#token_read_name()
                        }
                    }
                    Some(name) => name.to_token_stream(),
                }
            }
        }
    }
    //return the function call that return the value
    fn gen_context_field_read(&self, varnode: &'a Varnode) -> TokenStream {
        let context = self.disassembler.context_trait.varnode(varnode);
        let context_func = context.read();
        let context_param = &self.context_param;
        quote! {
            #context_param.#context_func()
        }
    }
    fn gen_table_field(&self, table: &'a sleigh_rs::Table) -> TokenStream {
        let table_ptr: *const _ = table;
        let table = self.disassembler.tables.get(&table_ptr).unwrap();
        let context_current = &self.context_current;
        let table_enum = table.name();
        let table_parser = table.parse_name();
        let tokens = &self.token_param;
        let inst_start = self.inst_start;
        let global_set = self.global_set_param;
        quote! {
            #table_enum::#table_parser(
                #tokens,
                &mut #context_current,
                #inst_start,
                #global_set,
            )
        }
    }
    fn get_table_field_name(&self, table: &'a sleigh_rs::Table) -> TokenStream {
        let table_ptr: *const _ = table;
        match self.produced_tables.get(&table_ptr) {
            None => quote! {_},
            Some(name) => name.to_token_stream(),
        }
    }
    fn process(&mut self) -> TokenStream {
        match self.block {
            Block::Or {
                fields,
                len: _,
                products,
            } => {
                let fields = fields
                    .iter()
                    .map(|field| self.gen_field_or(field, products));
                quote! {
                    |tokens, context: &mut T| {
                        #(#fields)*
                        None
                    }
                }
            }
            Block::And {
                left,
                left_len: _,
                right,
                right_len: _,
                products,
            } => self.gen_field_and(left, right, products),
        }
    }
    fn gen_field_and(
        &self,
        left: &'a [FieldAnd],
        right: &'a [FieldAnd],
        products: &'a FieldProducts,
    ) -> TokenStream {
        use sleigh_rs::semantic::pattern::ConstraintVariable::*;
        use sleigh_rs::semantic::pattern::Reference;
        //TODO use left_len and right_len
        //TODO match the right expression
        if !right.is_empty() {
            unimplemented!("right match pattern is not implemented yet")
        }
        //generate the token parser, if any
        let token_parser = token_len_from_fields_and(left)
            .map(|len| self.token_parser(len).unwrap());
        let token_parser_name = format_ident!("token_parser");
        let token_parser_creation = token_parser.map(|token_parser| {
            let token_new = self.gen_new_token_parser(token_parser);
            let inst_work_type = &self.disassembler.inst_work_type;
            let len_bytes = token_parser.token_len().get();
            let block_len = &self.block_len;
            quote! {
                let #token_parser_name = #token_new;
                #block_len = #len_bytes as #inst_work_type;
            }
        });

        //generate variables, don't verify or parse anything
        let ass_variables = left
            .iter()
            //only assembly need to be parsed into variables
            .filter_map(|field| match field {
                FieldAnd::Constraint {
                    field: Assembly { src: _, assembly },
                    constraint: _,
                } => Some(assembly),
                FieldAnd::Field(Reference::Assembly { assembly, src: _ }) => {
                    Some(assembly)
                }
                _ => None,
            })
            .map(|ass| {
                self.gen_ass_field(ass, token_parser, &token_parser_name)
            });

        //verify all the values and build all the tables
        let verifications = left.iter().filter_map(|field| match field {
            FieldAnd::Constraint { field, constraint } => {
                let value = BlockParserValuesDisassembly(self)
                    .expr(&constraint.value().expr);
                let cons_op = pattern_cmp_token_neg(constraint.op());
                match field {
                    Assembly { src: _, assembly } => {
                        let field = self.get_ass_field_value(
                            assembly,
                            token_parser,
                            &token_parser_name,
                        );
                        let field_type = WorkType::from_ass(assembly);
                        Some(quote! {
                            if #field #cons_op #value as #field_type {
                                return None;
                            }
                        })
                    }
                    Varnode { src: _, varnode } => {
                        let field = self.gen_context_field_read(varnode);
                        let field_type = WorkType::from_varnode(varnode);
                        Some(quote! {
                            if #field #cons_op #value as #field_type {
                                return None;
                            }
                        })
                    }
                }
            }
            FieldAnd::Field(
                sleigh_rs::semantic::pattern::Reference::Table {
                    table,
                    self_ref: _,
                    src: _,
                },
            ) => {
                let table_name = self.get_table_field_name(&table);
                let table_parsing = self.gen_table_field(table);
                let block_len = &self.block_len;
                let inst_work_type = &self.disassembler.inst_work_type;
                Some(quote! {
                    let #table_name = if let Some((len, table)) =
                            #table_parsing {
                        #block_len = len as #inst_work_type;
                        table
                    } else {
                        return None;
                    };
                })
            }
            FieldAnd::Field(_) => None,
            FieldAnd::SubPattern { src, sub } => {
                //TODO check recursive/always for table
                let sub_func = self.gen_sub_pattern(sub);
                let tables =
                    sub.produced().tables().iter().map(|table_field| {
                        self.produced_tables
                            .get(&Rc::as_ptr(table_field.table()))
                            .map(|x| x.into_token_stream())
                            .unwrap_or_else(|| unreachable!())
                    });
                let fields = sub.produced().fields().iter().map(|field| {
                    self.produced_fields
                        .get(&Rc::as_ptr(field))
                        .map(|x| x.to_token_stream())
                        .unwrap_or(quote! {_})
                });
                let tokens = &self.token_param;
                let context_current = &self.context_current;
                let block_len = &self.block_len;
                let sub_pattern_name =
                    format_ident!("sub_pattern_c{}", src.column);
                Some(quote! {
                    let mut #sub_pattern_name = #sub_func;
                    let (
                        (#(mut #tables),*),
                        (#(#fields),*),
                        sub_len
                     ) = #sub_pattern_name(#tokens, &mut #context_current)?;
                     #block_len = #block_len.max(sub_len);
                })
            }
        });

        let tables = products.tables().iter().map(|table_field| {
            //TODO what if the table have multiple levels of Option?
            if let Some(name) =
                self.produced_tables.get(&Rc::as_ptr(table_field.table()))
            {
                let return_table = if table_field.recursive() {
                    quote! {Box::new(#name)}
                } else {
                    name.to_token_stream()
                };
                if !table_field.always() {
                    quote! {Some(#return_table)}
                } else {
                    return_table
                }
            } else {
                if table_field.always() {
                    unreachable!()
                } else {
                    return quote! {None};
                }
            }
        });
        let fields = products
            .fields()
            .iter()
            .map(|field| self.produced_fields.get(&Rc::as_ptr(field)).unwrap());
        let inst_work_type = &self.disassembler.inst_work_type;
        let block_len = &self.block_len;
        let context_current = &self.context_current;
        quote! {
            |tokens, context: &mut T| {
                //used to calculate the current block len
                let mut #block_len = 0 as #inst_work_type;
                let mut #context_current = context.clone();
                #token_parser_creation
                #(#ass_variables)*
                #(#verifications)*
                *context = #context_current;
                Some((
                    (#(#tables),*),
                    (#(#fields),*),
                    #block_len
                ))
            }
        }
    }
    fn gen_field_or(
        &self,
        field: &'a FieldOr,
        products: &'a FieldProducts,
    ) -> TokenStream {
        use sleigh_rs::semantic::pattern::*;
        match field {
            FieldOr::Constraint {
                field: ConstraintVariable::Assembly { src: _, assembly },
                constraint,
                ..
            } => {
                //generate the token parser, if any
                let token_len = assembly.token_len() / 8;
                let token_parser = NonZeroTypeU::new(token_len)
                    .map(|len| self.token_parser(len).unwrap());
                let token_parser_name = format_ident!("token_parser");
                let token_parser_creation = token_parser.map(|token_parser| {
                    let token_new = self.gen_new_token_parser(token_parser);
                    let inst_work_type = &self.disassembler.inst_work_type;
                    let len_bytes = token_parser.token_len().get();
                    let block_len = &self.block_len;
                    quote! {
                        let #token_parser_name = #token_new;
                        #block_len = #len_bytes as #inst_work_type;
                    }
                });

                //generate a variable with the assembly that will be
                //extracted
                let field_var = self.gen_ass_field(
                    assembly,
                    token_parser,
                    &token_parser_name,
                );

                //verify the value, if true, return it
                let value = BlockParserValuesDisassembly(self)
                    .expr(&constraint.value().expr);
                let cons_op = pattern_cmp_token(constraint.op());
                let field = self.get_ass_field_value(
                    assembly,
                    token_parser,
                    &token_parser_name,
                );
                let field_type = WorkType::from_ass(assembly);
                let inst_work_type = self.disassembler.inst_work_type;
                let token_len_bytes = token_len / 8;
                let fields = products.fields().iter().map(|field_prod| {
                    assert_eq!(Rc::as_ptr(assembly), Rc::as_ptr(field_prod));
                    &field
                });
                let tables = products.tables().iter().map(|table_field| {
                    if table_field.always() {
                        unreachable!()
                    } else {
                        quote! {None}
                    }
                });
                quote! {
                    #token_parser_creation
                    #field_var
                    if #field #cons_op #value as #field_type {
                        return Some((
                            (#(#tables),*),
                            (#(#fields),*),
                            #inst_work_type::try_from(#token_len_bytes).unwrap(),
                        ));
                    }
                }
            }
            FieldOr::Constraint {
                field: ConstraintVariable::Varnode { src: _, varnode },
                constraint,
                ..
            } => {
                let value = BlockParserValuesDisassembly(self)
                    .expr(&constraint.value().expr);
                let cons_op = pattern_cmp_token(constraint.op());
                let context = self.gen_context_field_read(varnode);
                let context_type = WorkType::from_varnode(varnode);
                let inst_work_type = self.disassembler.inst_work_type;
                let tables = products.tables().iter().map(|table_field| {
                    if table_field.always() {
                        unreachable!()
                    } else {
                        quote! {None}
                    }
                });
                quote! {
                    if #context #cons_op #value as #context_type {
                        return Some((
                            (#(#tables),*),
                            (/*no fields*/),
                            0 as #inst_work_type,
                        ));
                    }
                }
            }
            FieldOr::SubPattern { sub, src } => {
                let sub_parser = self.gen_sub_pattern(sub);
                let tokens = &self.token_param;
                let context_param = &self.context_param;
                let sub_pattern_name =
                    format_ident!("sub_pattern_c{}", src.column);
                let tables: HashMap<_, _> = sub
                    .produced()
                    .tables()
                    .iter()
                    .map(|field_table| {
                        let table = field_table.table().as_ref();
                        let ptr: *const _ = table;
                        (ptr, format_ident!("{}", from_sleigh(&table.name)))
                    })
                    .collect();
                let fields: HashMap<_, _> = sub
                    .produced()
                    .fields()
                    .iter()
                    .map(|field| {
                        let ptr = Rc::as_ptr(field);
                        (ptr, format_ident!("{}", from_sleigh(&field.name)))
                    })
                    .collect();
                let sub_tables = tables.values();
                let sub_fields = fields.values();
                let tables = products.tables().iter().map(|table_field| {
                    //TODO what if the table have multiple levels of Option?
                    if let Some(name) =
                        tables.get(&Rc::as_ptr(table_field.table()))
                    {
                        let return_table = if table_field.recursive() {
                            quote! {Box::new(#name)}
                        } else {
                            name.to_token_stream()
                        };
                        if !table_field.always() {
                            quote! {Some(#return_table)}
                        } else {
                            return_table
                        }
                    } else {
                        if table_field.always() {
                            unreachable!()
                        } else {
                            return quote! {None};
                        }
                    }
                });
                let fields = products
                    .fields()
                    .iter()
                    .map(|field| fields.get(&Rc::as_ptr(field)).unwrap());
                quote! {
                    let mut #sub_pattern_name = #sub_parser;
                    let mut context_current = #context_param.clone();
                    if let Some((
                        (#(#sub_tables),*),
                        (#(#sub_fields),*),
                        sub_pattern_len,
                    )) = #sub_pattern_name(#tokens, &mut context_current) {
                        *#context_param = context_current;
                        return Some((
                            (#(#tables),*),
                            (#(#fields),*),
                            sub_pattern_len,
                        ));
                    }
                }
            }
        }
    }
    fn gen_sub_pattern(&self, pattern: &'a sleigh_rs::Pattern) -> TokenStream {
        let context_current = format_ident!("context_current");
        let block_len = format_ident!("block_len");
        let token_current = format_ident!("token_current");
        let inst_work_type = self.disassembler.inst_work_type;

        let pattern_produced_tables: HashMap<_, _> = pattern
            .produced()
            .tables()
            .iter()
            .map(|field_table| {
                let table = field_table.table().as_ref();
                let table_ptr: *const _ = table;
                (table_ptr, format_ident!("{}", from_sleigh(&table.name)))
            })
            .collect();
        let pattern_produced_fields: HashMap<_, _> = pattern
            .produced()
            .fields()
            .iter()
            .map(|field| {
                let ptr = Rc::as_ptr(field);
                (ptr, format_ident!("{}", from_sleigh(&field.name)))
            })
            .collect();
        //for each block:
        //* make the verifications
        //* parse any sub tables
        //* create variables (table/assembly)
        //variables created by the blocks
        let blocks = pattern.blocks().iter().enumerate();
        let blocks_parsing = blocks.map(|(block_index, block)| {
            let parse = Self::generate_block(
                self.disassembler,
                self.global_set_param,
                self.inst_start,
                self.constructor,
                block,
            );
            let block_produced_tables =
                block.produced().tables().iter().map(|field_table| {
                    pattern_produced_tables
                        .get(&Rc::as_ptr(field_table.table()))
                        .map(|table_name| table_name.to_token_stream())
                        .unwrap_or(quote! {_})
                });
            let block_produced_fields =
                block.produced().fields().iter().map(|field| {
                    pattern_produced_fields
                        .get(&Rc::as_ptr(field))
                        .map(|field_name| field_name.to_token_stream())
                        .unwrap_or(quote! {_})
                });
            let block_name = format_ident!("block_{}", block_index);
            quote! {
                let mut #block_name = #parse;
                let (
                    (#(mut #block_produced_tables),*),
                    (#(#block_produced_fields),*),
                    block_len
                ) = #block_name(#token_current, &mut #context_current)?;
                #token_current = &#token_current[
                    usize::try_from(block_len).unwrap()..
                ];
            }
        });
        let tables = pattern.produced().tables().iter().map(|table_field| {
            //TODO what if the table have multiple levels of Option?
            if let Some(name) =
                pattern_produced_tables.get(&Rc::as_ptr(table_field.table()))
            {
                let return_table = if table_field.recursive() {
                    quote! {Box::new(#name)}
                } else {
                    name.to_token_stream()
                };
                if !table_field.always() {
                    quote! {Some(#return_table)}
                } else {
                    return_table
                }
            } else {
                if table_field.always() {
                    unreachable!()
                } else {
                    return quote! {None};
                }
            }
        });
        let fields = pattern.produced().fields().iter().map(|field| {
            pattern_produced_fields.get(&Rc::as_ptr(field)).unwrap()
        });
        quote! {
            |token, context: &mut T| {
                //each block will increment this value by its size
                let mut #block_len = 0 as #inst_work_type;
                let mut #context_current = context.clone();
                //the current_token will be increseased by each block, so the next
                //block knows when to start parsing
                let mut #token_current = token;
                #(#blocks_parsing)*
                *context = #context_current;
                Some((
                    (#(#tables),*),
                    (#(#fields),*),
                    #block_len
                ))
            }
        }
    }
}

struct BlockParserValuesDisassembly<'a, 'b, 'c>(&'c BlockParser<'a, 'b>);
//Block parser only use Disassembly for the pattern value, so only value is used
impl<'a, 'b, 'c> DisassemblyGenerator<'a>
    for BlockParserValuesDisassembly<'a, 'b, 'c>
{
    fn global_set(&mut self, _global_set: &GlobalSet) -> TokenStream {
        unreachable!()
    }
    fn value(
        &mut self,
        value: &'a sleigh_rs::semantic::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::semantic::disassembly::*;
        match value {
            ReadScope::Integer(value) => quote! {(#value as i64)},
            ReadScope::Varnode(varnode) => {
                let value = self.0.gen_context_field_read(varnode);
                //TODO solve IntTypeS instead of i64
                quote! { i64::try_from(#value).unwrap() }
            }
            ReadScope::Assembly(ass) => {
                let value =
                    self.0.produced_fields.get(&Rc::as_ptr(ass)).unwrap();
                //TODO solve IntTypeS instead of i64
                quote! { i64::try_from(#value).unwrap() }
            }
            ReadScope::Local(_) => unreachable!(),
        }
    }
    fn set_context(
        &mut self,
        _context: &Varnode,
        _value: TokenStream,
    ) -> TokenStream {
        unreachable!()
    }
    fn new_variable(&mut self, _var: &Variable) {
        unreachable!()
    }
    fn var_name(&mut self, _var: &Variable) -> TokenStream {
        unreachable!()
    }
}

impl<'a> Disassembler<'a> {
    pub fn gen_constructor_display_extend(
        &self,
        constructor: &Constructor<'a>,
    ) -> TokenStream {
        let name = constructor.display();
        let display_param = format_ident!("display");
        let context_param = format_ident!("context");
        let display_struct = self.display.name();
        let context_trait = self.context_trait.name();
        let display = &self.display;
        let display_element = display.name();
        let var_literal = display.literal_var();
        let var_register = display.register_var();
        let var_signed = display.signed_var();
        let var_unsigned = display.unsigned_var();
        let register_enum = self.registers.name();

        let attach_solve = |attach: &RefCell<Option<Rc<Meaning>>>,
                            value: &TokenStream|
         -> Option<TokenStream> {
            let attach = attach.borrow();
            let attach = attach.as_ref()?;
            let function =
                self.meanings.get(&Rc::as_ptr(attach)).unwrap().name();
            Some(quote! {#function(#value)})
        };
        let sig_var = |signed| match signed {
            true => var_signed,
            false => var_unsigned,
        };

        use sleigh_rs::semantic::display::Element;
        let displays = constructor
            .sleigh()
            .display
            .elements()
            .split_inclusive(|ele| matches!(ele, Element::Table(_)))
            .map(|eles| {
                let (ele, table) = match eles {
                    [ele @ .., Element::Table(table)] => (ele, Some(table)),
                    _ => (eles, None),
                };
                let extend = (!ele.is_empty()).then(|| {
                    let display = ele.iter().map(|ele| match ele {
                        Element::Varnode(varnode) => {
                            use sleigh_rs::semantic::varnode::{
                                Context, VarnodeType,
                            };
                            match &varnode.varnode_type {
                                VarnodeType::Memory(_)
                                | VarnodeType::BitRange(_) => {
                                    let (reg_var, _) = self
                                        .registers
                                        .register(&varnode)
                                        .unwrap();
                                    quote! {
                                        #display_element::#var_register(
                                            #register_enum::#reg_var
                                        )
                                    }
                                }
                                //TODO solve context with and without meaning
                                VarnodeType::Context(
                                    Context {
                                        attach,
                                        fmt,
                                        signed,
                                        ..
                                    },
                                ) => {
                                    let context = self.context_trait.varnode(varnode.as_ref());
                                    let context_read_fun = context.read();
                                    let value = quote! {
                                        #context_param.#context_read_fun()
                                    };
                                    if let Some(val) =
                                        attach_solve(attach, &value)
                                    {
                                        val
                                    } else {
                                        let var = sig_var(*signed);
                                        let hex = matches!(fmt, PrintFmt::Hex);
                                        quote! {
                                            #display_element::#var(#hex, #value)
                                        }
                                    }
                                }
                            }
                        }
                        Element::Assembly(ass) => {
                            let field = ass.field().unwrap();
                            let var = constructor
                                .ass_fields
                                .get(&Rc::as_ptr(&ass))
                                .unwrap();
                            let var_name = var.name();
                            let value = quote! {
                                usize::try_from(*#var_name).unwrap()
                            };
                            if let Some(val) =
                                attach_solve(&field.attach, &value)
                            {
                                return val;
                            }
                            //TODO replace u64/i64 by IntTypeU/IntTypeS type
                            let (variant, var_name) = if field.signed {
                                (
                                    var_signed,
                                    quote! {i64::try_from(*#var_name).unwrap()},
                                )
                            } else {
                                (
                                    var_unsigned,
                                    quote! {u64::try_from(*#var_name).unwrap()},
                                )
                            };
                            let hex = matches!(
                                field.fmt,
                                sleigh_rs::semantic::PrintFmt::Hex
                            );
                            quote! {
                                #display_element::#variant(#hex, #var_name)
                            }
                        }
                        Element::Disassembly(var) => {
                            let var = constructor
                                .calc_fields
                                .get(&Rc::as_ptr(&var))
                                .unwrap();
                            let var_name = var.name();
                            //TODO the calculated value are printed as signed hex
                            //by default?
                            quote! {
                                #display_element::#var_signed(true, *#var_name)
                            }
                        }
                        Element::Literal(literal) => {
                            quote! {#display_element::#var_literal(#literal)}
                        }
                        Element::Table(_) => unreachable!(),
                    });
                    let display_out_len = ele.len();
                    quote! {
                        let extend: [#display_element; #display_out_len] = [
                            #(#display),*
                        ];
                        #display_param.extend_from_slice(&extend);
                    }
                });
                let build_table = table.map(|table_sleigh| {
                    let field = constructor
                        .table_fields
                        .get(&Rc::as_ptr(&table_sleigh))
                        .unwrap();
                    let field_name = field.name();
                    let table = self.tables.get(&Rc::as_ptr(field.table())).unwrap();
                    let display_fun = table.display_extend_name();
                    quote! {
                        #field_name.#display_fun(#display_param, #context_param);
                    }
                });
                quote! {
                    #extend
                    #build_table
                }
        });
        let pattern = constructor.gen_match_fields();
        quote! {
            pub fn #name<T>(
                &self,
                #display_param: &mut Vec<#display_struct>,
                #context_param: &T,
            ) where T: #context_trait + Clone {
                let Self { #(#pattern),* } = self;
                #(#displays)*
            }
        }
    }
    pub fn gen_constructor_disassembly(
        &self,
        constructor: &Constructor<'a>,
        ass_vars: &HashMap<*const Assembly, Ident>,
    ) -> Option<TokenStream> {
        let name = constructor.disassembly()?;
        let context_param = format_ident!("context_param");
        let global_set_param = format_ident!("global_set");
        let inst_start = format_ident!("inst_start");
        let inst_next = format_ident!("inst_next");
        //only non-instruction-tables that have pos disassembly to execute will
        //do something
        let body = (!constructor.is_root()
            && constructor.sleigh().disassembly.pos)
            .then(|| {
                //make it simpler to get the field value in DisassemblerConstructor
                let pattern = constructor.gen_match_fields();
                //disassembly code
                let disassembly = DisassemblyConstructor::disassembly(
                    &self.context_trait,
                    &self.global_set,
                    &inst_start,
                    &inst_next,
                    &self.inst_work_type,
                    &global_set_param,
                    &context_param,
                    true,
                    true,
                    &constructor,
                    &constructor.calc_fields,
                    ass_vars,
                    &constructor.sleigh().disassembly,
                );
                //call disassembly for all the sub tables
                let sub_disassembly = constructor
                    .table_fields()
                    .filter_map(|field| {
                        let name = field.name();
                        let table_ptr = Rc::as_ptr(field.table());
                        let table = self.tables.get(&table_ptr).unwrap();
                        let disassembly = table.disassembly_name()?;
                        Some(quote! {
                            #name.#disassembly(
                                #context_param,
                                #inst_start,
                                #inst_next,
                                #global_set_param,
                            );
                        })
                    })
                    .collect::<TokenStream>();
                quote! {
                        let Self { #(#pattern),* } = self;
                        #disassembly
                        #sub_disassembly
                }
            });
        let context_trait_name = &self.context_trait.name();
        let global_set_enum = &self.global_set.trait_name();
        let inst_work_type = &self.inst_work_type;
        Some(quote! {
            fn #name<'a, T>(
                &mut self,
                #context_param: &mut T,
                #inst_start: #inst_work_type,
                #inst_next: #inst_work_type,
                #global_set_param: &mut impl #global_set_enum,
            ) where T: #context_trait_name + Clone
            {
                #body
            }
        })
    }
    pub fn gen_constructor_parse(
        &self,
        constructor: &Constructor<'a>,
    ) -> (
        HashMap<*const sleigh_rs::Table, Ident>,
        HashMap<*const Assembly, Ident>,
        TokenStream,
    ) {
        let sleigh = constructor.sleigh();
        let context_current = format_ident!("context_current");
        let token_param = format_ident!("tokens");
        let global_set_param = format_ident!("global_set");
        let parse_name = constructor.parse();
        let inst_start = format_ident!("inst_start");
        let global_set_enum = self.global_set.trait_name();
        let context_trait_name = self.context_trait.name();

        let inst_next = format_ident!("inst_next");
        let inst_len = format_ident!("inst_len");
        let token_current = format_ident!("token_current");

        let inst_work_type = self.inst_work_type;

        //for each block:
        //* make the verifications
        //* parse any sub tables
        //* create variables (table/assembly)
        let mut blocks_parsing = TokenStream::new();
        //variables created by the blocks
        let mut table_vars: HashMap<*const sleigh_rs::Table, Ident> =
            HashMap::new();
        let mut ass_vars: HashMap<*const Assembly, Ident> = HashMap::new();
        for (block_index, block) in sleigh.pattern.blocks().iter().enumerate() {
            let parse = BlockParser::generate_block(
                self,
                &global_set_param,
                &inst_start,
                constructor,
                block,
            );
            let mut tables: Vec<(*const sleigh_rs::Table, Ident)> = block
                .produced()
                .tables()
                .iter()
                .map(|field_table| {
                    let table = field_table.table().as_ref();
                    (
                        table as *const _,
                        format_ident!("{}", from_sleigh(&table.name)),
                    )
                })
                .collect();
            let mut fields: Vec<(*const sleigh_rs::Assembly, Ident)> = block
                .produced()
                .fields()
                .iter()
                .map(|field| {
                    (
                        field.as_ref() as *const _,
                        format_ident!("{}", from_sleigh(&field.name)),
                    )
                })
                .collect();
            let table_var_print = tables.iter().map(|(_, name)| name);
            let field_var_print = fields.iter().map(|(_, name)| name);
            let parse_block_name = format_ident!("block_{}", block_index);
            blocks_parsing.extend(quote! {
                let mut #parse_block_name = #parse;
                let (
                    (#(mut #table_var_print),*),
                    (#(#field_var_print),*),
                    block_len
                ) = #parse_block_name(#token_current, &mut #context_current)?;
                #token_current = &#token_current[usize::try_from(block_len).unwrap()..];
                #inst_len += block_len;
            });
            table_vars.extend(tables.drain(..));
            ass_vars.extend(fields.drain(..));
        }

        //values that where created during the disassembly
        let mut calc_vars_local = HashMap::new();

        //if this table have any pre disassembly, it should be executed here
        let disassembly_pre = if !sleigh.disassembly.pos {
            DisassemblyConstructor::disassembly(
                &self.context_trait,
                &self.global_set,
                &inst_start,
                &inst_next,
                &inst_work_type,
                &global_set_param,
                &context_current,
                false,
                false,
                constructor,
                &mut calc_vars_local,
                &ass_vars,
                &sleigh.disassembly,
            )
        } else {
            quote! {}
        };

        //if root, any pos disassembly should also be executed localy
        let disassembly_pos = if constructor.is_root() {
            let dis = sleigh.disassembly.pos.then(|| {
                let context_helper = format_ident!("context_tmp");
                let dis = DisassemblyConstructor::disassembly(
                    &self.context_trait,
                    &self.global_set,
                    &inst_start,
                    &inst_next,
                    &inst_work_type,
                    &global_set_param,
                    &context_helper,
                    false,
                    false,
                    constructor,
                    &mut calc_vars_local,
                    &ass_vars,
                    &sleigh.disassembly,
                );
                quote! {
                    let #context_helper = &mut #context_current;
                    #dis
                }
            });
            let disassembly_inner =
                constructor.table_fields().filter_map(|field| {
                    let name = field.name();
                    let table_ptr = Rc::as_ptr(field.table());
                    let table = self.tables.get(&table_ptr).unwrap();
                    let disassembly = table.disassembly_name()?;
                    Some(quote! {
                        #name.#disassembly(
                            &mut #context_current,
                            #inst_start,
                            #inst_next,
                            #global_set_param,
                        );
                    })
                });
            quote! {
                #(#disassembly_inner)*
                #dis
            }
        } else {
            quote! {}
        };

        //if this constructor have a calculated field, that was not calculated
        //during the disassembly, create a zero value for it.
        //obs: this only happen in non-root tables, that contains disassembly
        //values, that are used in display. Normally relative jumps use them.
        let defaults = constructor
            .calc_fields()
            .filter(|var| {
                let ptr: *const _ = *var.value_type();
                !calc_vars_local.contains_key(&ptr)
            })
            .map(|var| {
                let zero: IntTypeS = 0;
                let name = var.name();
                quote! {
                    let #name = #zero;
                }
            });
        let fields = constructor.gen_match_fields();
        let inst_next = constructor.is_root().then(|| {
            quote! {
                let #inst_next = #inst_start + #inst_len;
            }
        });
        let code = quote! {
            pub fn #parse_name<'a, T>(
                #token_param: &'a [u8],
                context: &mut T,
                #inst_start: #inst_work_type,
                //TODO can global set be called in pre disassembly reliably?
                //it could cause problems if in table matches, but the following
                //one does not, as globalset can't be undone.
                #global_set_param: &mut impl #global_set_enum,
            ) -> Option<(#inst_work_type, Self)>
                where T: #context_trait_name + Clone
            {
                //each block will increment this value by its size
                let mut #inst_len = 0 as #inst_work_type;
                let mut #context_current = context.clone();
                //the current_token will be increseased by each block, so the next
                //block knows when to start parsing
                let mut #token_current = #token_param;
                #blocks_parsing
                #disassembly_pre
                #inst_next
                //only on instruction table, otherwise this is on a function
                #(#defaults)*
                #disassembly_pos
                *context = #context_current;
                Some((
                    #inst_len,
                    Self{ #(#fields),* },
                ))
            }
        };
        (table_vars, ass_vars, code)
    }
    pub fn gen_variant_struct(
        &self,
        constructor: &Constructor<'a>,
    ) -> TokenStream {
        let sleigh = constructor.sleigh();
        let name = constructor.struct_name();
        let doc = format!("Constructor at {}", &sleigh.src);

        let tables = constructor.table_fields().map(|field| {
            let table = self.tables.get(&Rc::as_ptr(field.table())).unwrap();
            let table_enum = table.name();
            let mut table_enum = quote! { #table_enum };
            if field.recursive() {
                table_enum = quote! { Box<#table_enum> };
            }
            if !field.always() {
                table_enum = quote! { Option<#table_enum> };
            }
            let name = field.name();
            quote! { #name: #table_enum }
        });
        let calc = constructor.calc_fields().map(|field| {
            //TODO IntTypeS resulved value and not i64
            let name = field.name();
            quote! { #name: i64 }
        });
        let ass = constructor.ass_fields().map(|field| {
            let name = field.name();
            let work_type = WorkType::from_ass(&field);
            quote! { #name: #work_type }
        });
        let fields = tables.chain(calc).chain(ass);
        quote! {
            #[doc = #doc]
            #[derive(Clone, Debug)]
            pub struct #name {
                #(#fields),*
            }
        }
    }
    pub fn gen_constructor_variant(
        &self,
        constructor: &Constructor<'a>,
    ) -> TokenStream {
        let name = constructor.struct_name();
        let data = self.gen_variant_struct(constructor);
        let display = self.gen_constructor_display_extend(constructor);
        let (_table_vars, ass_vars, parse) =
            self.gen_constructor_parse(constructor);
        let disassembly =
            self.gen_constructor_disassembly(constructor, &ass_vars);
        quote! {
            #data
            impl #name {
                #display
                #disassembly
                #parse
            }
        }
    }
}
