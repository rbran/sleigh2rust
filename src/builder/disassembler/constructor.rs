use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::semantic::disassembly::{GlobalSet, Variable};
use sleigh_rs::semantic::{Meaning, PrintFmt};
use sleigh_rs::{Assembly, Block, IntTypeS, NonZeroTypeU, Varnode};

use crate::builder::formater::{from_sleigh, snake_case};
use crate::builder::{
    ContextTrait, DisassemblyGenerator, DisplayElement, RegistersEnum,
    TokenField, TokenParser, WorkType,
};

use super::{Disassembler, Table};

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

#[derive(Debug, Clone)]
pub struct Constructor {
    //TODO get a reference to the constructor directly, for now use a Rc table
    //with the constructor offset
    table: Rc<Table>,
    index: usize,
    //variant name in the enum
    name: Ident,
    //parse function
    parse: Ident,
    //fields required to print the disassembled output
    table_fields: HashMap<*const sleigh_rs::Table, (Ident, Rc<Table>)>,
    pub ass_fields: HashMap<*const Assembly, (Ident, Rc<TokenField>)>,
    pub calc_fields: HashMap<*const Variable, (Ident, Rc<Variable>)>,
}
impl Constructor {
    pub fn new(
        sleigh: &sleigh_rs::Constructor,
        table: Rc<Table>,
        number: usize,
        disassembly: &Disassembler,
    ) -> Self {
        let name = format_ident!("Var{}", number);
        let parse = format_ident!("parse_var{}", number);
        let mut table_fields = HashMap::new();
        let mut calc_fields = HashMap::new();
        let mut ass_fields = HashMap::new();
        let mut add_ass = |ass: &Rc<Assembly>| {
            let ptr = Rc::as_ptr(ass);
            ass_fields.entry(ptr).or_insert_with(|| {
                let name = format_ident!(
                    "{}",
                    snake_case(from_sleigh(ass.name.as_ref()))
                );
                let ass = Rc::clone(disassembly.token.field(ass));
                (name, ass)
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
                        let name = format_ident!(
                            "{}",
                            snake_case(from_sleigh(var.name.as_ref()))
                        );
                        (name, Rc::clone(var))
                    });
                }
                Table(table) => {
                    let ptr = Rc::as_ptr(table);
                    table_fields.entry(ptr).or_insert_with(|| {
                        let name = format_ident!(
                            "{}",
                            snake_case(from_sleigh(table.name.as_ref()))
                        );
                        let table = Rc::clone(disassembly.pattern(table));
                        (name, table)
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
        Self {
            table,
            index: number,
            name,
            parse,
            table_fields,
            ass_fields,
            calc_fields,
        }
    }
    pub fn sleigh(&self) -> &sleigh_rs::Constructor {
        self.table.sleigh().constructors.get(self.index).unwrap()
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn parse(&self) -> &Ident {
        &self.parse
    }
    pub fn table_fields(&self) -> impl Iterator<Item = &(Ident, Rc<Table>)> {
        self.table_fields.values()
    }
    pub fn calc_fields(&self) -> impl Iterator<Item = &(Ident, Rc<Variable>)> {
        self.calc_fields.values()
    }
    pub fn ass_fields(&self) -> impl Iterator<Item = &(Ident, Rc<TokenField>)> {
        self.ass_fields.values()
    }
    pub fn gen_declare_fields<'a>(
        &'a self,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        let tables = self.table_fields().map(|(field, table)| {
            let table_enum = table.name();
            quote! { #field: #table_enum }
        });
        let calc = self.calc_fields().map(|(field, _val)| {
            //TODO IntTypeS resulved value and not i64
            quote! { #field: i64 }
        });
        let ass = self.ass_fields().map(|(field_name, field)| {
            let work_type = field.return_type();
            quote! { #field_name: #work_type }
        });
        tables.chain(calc).chain(ass)
    }
    pub fn gen_match_fields<'a>(&'a self) -> impl Iterator<Item = &Ident> + 'a {
        let tables = self.table_fields().map(|(field, _table)| field);
        let calc = self.calc_fields().map(|(field, _var)| field);
        let ass = self.ass_fields().map(|(field_name, _field)| field_name);
        tables.chain(calc).chain(ass)
    }

    pub fn gen_parse(
        &self,
        token_parser: &TokenParser,
        global_set: &crate::builder::GlobalSet,
        context_trait: &ContextTrait,
        tables: &HashMap<*const sleigh_rs::Table, Rc<Table>>,
        inst_work_type: &WorkType,
    ) -> TokenStream {
        let context_current = format_ident!("context_current");
        let token_param = format_ident!("tokens");
        let global_set_param = format_ident!("global_set");
        let constructor_src = format!("Constructor at {}", &self.sleigh().src);
        let parse_name = self.parse();
        let inst_start = format_ident!("inst_start");
        let global_set_enum = global_set.trait_name();
        let context_trait_name = context_trait.name();
        let parse = self.gen_parse_body(
            token_parser,
            global_set,
            context_trait,
            tables,
            inst_work_type,
            &context_current,
            &token_param,
            &inst_start,
            &global_set_param,
        );
        quote! {
            #[doc = #constructor_src]
            pub fn #parse_name<'a, T>(
                #token_param: &'a [u8],
                context: &mut T,
                #inst_start: #inst_work_type,
                #global_set_param: &mut impl #global_set_enum,
            ) -> Option<(&'a [u8], Self)>
                where T: #context_trait_name + Clone
            {
                //context can only be modified if the match is valid
                //TODO make the context a Cow
                let mut #context_current = context.clone();
                #parse
            }
        }
    }
    fn gen_parse_body(
        &self,
        token_parser: &TokenParser,
        global_set: &crate::builder::GlobalSet,
        context_trait: &ContextTrait,
        tables: &HashMap<*const sleigh_rs::Table, Rc<Table>>,
        inst_work_type: &WorkType,

        context_current: &Ident,
        token_param: &Ident,
        inst_start: &Ident,
        global_set_param: &Ident,
    ) -> TokenStream {
        let mut output = TokenStream::new();

        let mut token_current = token_param.clone();
        let mut token_next = None;

        //variables created by the blocks
        let mut table_vars = HashMap::new();
        let mut ass_vars = HashMap::new();
        let mut context_vars = HashMap::new();

        //create inst_next to be used by the disassembly
        //but only on the root (instruction) table
        let inst_next = self.table.sleigh().is_root().then(|| {
            let inst_next = format_ident!("inst_next");
            let inst_next_ref = &inst_next;
            output.extend(quote! {
                let mut #inst_next_ref = #inst_start;
            });
            inst_next
        });

        //for each block:
        //* make the verifications
        //* parse any sub tables
        //* create variables (table/assembly/context)
        for block in self.sleigh().pattern.blocks().iter() {
            output.extend(BlockParser::generate(
                token_parser,
                &token_current,
                &mut token_next,
                global_set_param,
                context_trait,
                context_current,
                inst_work_type,
                inst_start,
                &inst_next,
                self,
                tables,
                block,
                &mut table_vars,
                &mut ass_vars,
                &mut context_vars,
            ));
            if let Some(token_next) = token_next.take() {
                token_current = token_next;
            }
        }

        //values that where created during the disassembly
        let mut calc_vars_local = HashMap::new();

        //if this table have any pre disassembly, it should be executed here
        if !self.sleigh().disassembly.pos {
            let sleigh = self.sleigh();
            output.extend(DisassemblerConstructor::disassembly(
                context_trait,
                global_set,
                inst_start,
                &inst_next,
                inst_work_type,
                global_set_param,
                context_current,
                false,
                &ass_vars,
                false,
                &HashMap::new(),
                &mut calc_vars_local,
                &sleigh.disassembly,
            ));
        }

        //if root, any pos disassembly should also be executed localy
        if self.table.sleigh().is_root() {
            let sleigh = self.sleigh();
            let context_helper = format_ident!("context_tmp");
            output.extend(quote! {
                let #context_helper = &mut #context_current;
            });
            if sleigh.disassembly.pos {
                output.extend(DisassemblerConstructor::disassembly(
                    context_trait,
                    global_set,
                    inst_start,
                    &inst_next,
                    inst_work_type,
                    global_set_param,
                    context_current,
                    false,
                    &ass_vars,
                    false,
                    &HashMap::new(),
                    &mut calc_vars_local,
                    &sleigh.disassembly,
                ));
            }
        }

        //if this constructor have a calculated field, that was not calculated
        //during the disassembly, create a zero value for it.
        //obs: this only happen in non-root tables, that contains disassembly
        //values, that are used in display. Normally relative jumps use them.
        let defaults = self
            .calc_fields()
            .filter(|(_, var)| !calc_vars_local.contains_key(&Rc::as_ptr(var)))
            .map(|(name, _)| {
                let zero: IntTypeS = 0;
                quote! {
                    let #name = #zero;
                }
            });
        let fields = self.gen_match_fields();
        let variant = self.name();
        output.extend(quote! {
            #(#defaults)*
            return Some((#token_current, Self::#variant {#(#fields),*}));
        });

        output
    }
}

pub struct ConstructorDisplay<'a> {
    //disassembler: &'a Disassembler,
    display_param: &'a Ident,
    display: &'a DisplayElement,
    context_param: &'a Ident,
    context: &'a ContextTrait,
    register: &'a RegistersEnum,
    meanings: &'a HashMap<
        *const sleigh_rs::semantic::Meaning,
        crate::builder::Meaning,
    >,
    //table: &'a Table,
    constructor: &'a Constructor,
    constructor_sleigh: &'a sleigh_rs::Constructor,
}

impl<'a> ConstructorDisplay<'a> {
    pub fn new(
        //disassembler: &'a Disassembler,
        display_param: &'a Ident,
        display: &'a DisplayElement,
        context_param: &'a Ident,
        context: &'a ContextTrait,
        register: &'a RegistersEnum,
        meanings: &'a HashMap<
            *const sleigh_rs::semantic::Meaning,
            crate::builder::Meaning,
        >,
        //table: &'a Table,
        constructor: &'a Constructor,
        constructor_sleigh: &'a sleigh_rs::Constructor,
    ) -> Self {
        Self {
            //disassembler,
            display_param,
            display,
            context_param,
            context,
            register,
            meanings,
            //table,
            constructor,
            constructor_sleigh,
        }
    }
    pub fn gen_display_match(&self) -> TokenStream {
        let var_name = self.constructor.name();
        let var_field = self.constructor.gen_match_fields();
        let display_param = self.display_param;
        let display_element = self.display.name();
        let var_literal = self.display.literal_var();
        let var_register = self.display.register_var();
        let var_signed = self.display.signed_var();
        let var_unsigned = self.display.unsigned_var();
        let register_enum = self.register.name();

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
        let display = self
            .constructor_sleigh
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
                                        .register
                                        .register(varnode)
                                        .unwrap();
                                    quote! {
                                        #display_element::#var_register(
                                            #register_enum::#reg_var
                                        )
                                    }
                                }
                                //TODO solve context with and without meaning
                                VarnodeType::Context(
                                    _context @ Context {
                                        attach,
                                        fmt,
                                        signed,
                                        ..
                                    },
                                ) => {
                                    let context = self.context.varnode(varnode);
                                    let context_param = self.context_param;
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
                            let (var_name, _) = self
                                .constructor
                                .ass_fields
                                .get(&Rc::as_ptr(ass))
                                .unwrap();
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
                            let (var_name, _) = self
                                .constructor
                                .calc_fields
                                .get(&Rc::as_ptr(var))
                                .unwrap();
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
                    let (table_name, table) = self
                        .constructor
                        .table_fields
                        .get(&Rc::as_ptr(table_sleigh))
                        .unwrap();
                    let display_fun = table.display_extend_name();
                    let context_param = self.context_param;
                    quote! {
                        #table_name.#display_fun(#display_param, #context_param);
                    }
                });
                quote! {
                    #extend
                    #build_table
                }
            });
        quote! {
            Self::#var_name { #(#var_field),* } => {
                #(#display)*
            },
        }
    }
}

struct BlockParser<'a> {
    token_parser: &'a TokenParser,
    token_current: &'a Ident,
    token_next: &'a mut Option<Ident>,

    token_parser_current: Option<Ident>,

    global_set_param: &'a Ident,

    context_trait: &'a ContextTrait,
    context_param: &'a Ident,

    inst_work_type: &'a WorkType,
    inst_start: &'a Ident,
    inst_next: &'a Option<Ident>,

    constructor: &'a Constructor,
    tables: &'a HashMap<*const sleigh_rs::Table, Rc<Table>>,

    block: &'a Block,

    table_vars: &'a mut HashMap<*const sleigh_rs::Table, (Ident, Rc<Table>)>,
    ass_vars: &'a mut HashMap<*const Assembly, (Ident, Rc<TokenField>)>,
    context_vars:
        &'a mut HashMap<*const Varnode, (Ident, WorkType, Rc<Varnode>)>,

    parser_build: TokenStream,
    var_creation: TokenStream,
    table_parse: TokenStream,
    check: TokenStream,
}
impl<'a> BlockParser<'a> {
    pub fn generate(
        token_parser: &'a TokenParser,
        token_current: &'a Ident,
        token_next: &'a mut Option<Ident>,

        global_set_param: &'a Ident,

        context_trait: &'a ContextTrait,
        context_param: &'a Ident,

        inst_work_type: &'a WorkType,
        inst_start: &'a Ident,
        inst_next: &'a Option<Ident>,

        constructor: &'a Constructor,
        tables: &'a HashMap<*const sleigh_rs::Table, Rc<Table>>,

        block: &'a Block,

        table_vars: &'a mut HashMap<
            *const sleigh_rs::Table,
            (Ident, Rc<Table>),
        >,
        ass_vars: &'a mut HashMap<*const Assembly, (Ident, Rc<TokenField>)>,
        context_vars: &'a mut HashMap<
            *const Varnode,
            (Ident, WorkType, Rc<Varnode>),
        >,
    ) -> TokenStream {
        let mut helper = Self {
            token_parser,
            token_current,
            token_next,
            token_parser_current: None,
            global_set_param,
            context_trait,
            context_param,
            inst_work_type,
            inst_start,
            inst_next,
            constructor,
            tables,
            block,
            table_vars,
            ass_vars,
            context_vars,
            parser_build: TokenStream::new(),
            var_creation: TokenStream::new(),
            table_parse: TokenStream::new(),
            check: TokenStream::new(),
        };
        helper.process();

        let parser_build = helper.parser_build;
        let var_creation = helper.var_creation;
        let check = helper.check;
        let table_parse = helper.table_parse;
        quote! {
            #parser_build
            #var_creation
            #check
            #table_parse
        }
    }
    fn process(&mut self) {
        use sleigh_rs::semantic::pattern::*;
        let mut checks = TokenStream::new();
        let mut add_check = |value: TokenStream, op| {
            if checks.is_empty() {
                checks = value;
            } else {
                checks.extend(quote! {#op #value});
            }
        };
        let mut build_token_parser = |token_next, token_parser, len| {
            if let Some(len) = NonZeroTypeU::new(len) {
                let creator = self.token_parser.creator(len);

                let token_struct = self.token_parser.name();
                let token_current = &self.token_current;
                self.parser_build.extend(quote! {
                    let (#token_next, #token_parser) =
                        #token_struct::#creator(#token_current)?;
                });
            }
        };

        match self.block {
            Block::Or { fields, len } => {
                //create the token parser for this size and set the next token
                //offset
                let token_parser = format_ident!("token_parser");
                let token_next = format_ident!("token_next");
                build_token_parser(&token_next, &token_parser, *len);
                self.token_parser_current = Some(token_parser);
                *self.token_next = Some(token_next);

                //update the the inst_next address to be after this block
                if let Some(inst_next) = self.inst_next {
                    let inst_type = self.inst_work_type;
                    self.var_creation.extend(quote! {
                        #inst_next += (#len as #inst_type) * 8;
                    });
                }

                let op = quote! {||};
                for field in fields {
                    match field {
                        FieldOr::Constraint {
                            assembly,
                            constraint,
                            ..
                        } => {
                            let value = self.expr(&constraint.value().expr);
                            let (name, token_type) = self.ass_field(assembly);
                            let cons_op = pattern_cmp_token(constraint.op());
                            let check =
                                quote! {#name #cons_op #value as #token_type };
                            add_check(check, &op);
                        }
                        FieldOr::SubPattern(sub_pattern) => {
                            let sub = self.gen_sub_constraints(sub_pattern);
                            add_check(sub, &op)
                        }
                    }
                }
            }
            Block::And { fields, len } => {
                //create the token parser for this size and set the next token
                //offset
                let token_parser = format_ident!("token_parser");
                let token_next = format_ident!("token_next");
                build_token_parser(&token_next, &token_parser, *len);
                self.token_parser_current = Some(token_parser);
                *self.token_next = Some(token_next);

                //update the the inst_next address to be after this block
                if let Some(inst_next) = self.inst_next {
                    let inst_type = self.inst_work_type;
                    self.var_creation.extend(quote! {
                        #inst_next += (#len as #inst_type) * 8;
                    });
                }

                let op = quote! {&&};
                for field in fields {
                    match field {
                        FieldAnd::Constraint { field, constraint } => {
                            let value = self.expr(&constraint.value().expr);
                            let (name, return_type) = match field {
                                ConstraintVariable::Assembly {
                                    assembly,
                                    ..
                                } => self.ass_field(assembly),
                                ConstraintVariable::Varnode {
                                    varnode, ..
                                } => self.context_field(varnode),
                            };
                            let cons_op = pattern_cmp_token(constraint.op());
                            let check = quote! {
                                #name #cons_op #value as #return_type
                            };
                            add_check(check, &op);
                        }
                        FieldAnd::SubPattern(sub_pattern) => {
                            let sub = self.gen_sub_constraints(sub_pattern);
                            add_check(sub, &op);
                        }
                        FieldAnd::Field(Reference::Table { table, .. }) => {
                            let table_ptr = Rc::as_ptr(table);
                            let table = self.tables.get(&table_ptr).unwrap();
                            let table_ident = if let Some((table, _table)) =
                                self.constructor.table_fields.get(&table_ptr)
                            {
                                table.clone()
                            } else {
                                format_ident!("_")
                            };
                            let context = &self.context_param;
                            let table_enum = &table.name();
                            let table_creator = &table.parse_name();
                            let tokens = &self.token_current;
                            let context = quote! {&mut #context};
                            let inst_start = self.inst_start;
                            let global_set = self.global_set_param;
                            self.table_parse.extend(
                                quote! {let (_, #table_ident) =
                                #table_enum::#table_creator(
                                    #tokens,
                                    #context,
                                    #inst_start,
                                    #global_set,
                                )?;},
                            );
                            self.table_vars
                                .insert(
                                    table_ptr,
                                    (table_ident, Rc::clone(table)),
                                )
                                .map(|_| unreachable!());
                        }
                        FieldAnd::Field(Reference::Assembly {
                            assembly,
                            ..
                        }) => {
                            use sleigh_rs::semantic::assembly::AssemblyType::*;
                            if matches!(&assembly.assembly_type, Field(_)) {
                                //the value may or not be used, create the var
                                //just in case
                                self.ass_field(assembly);
                            }
                        }
                        FieldAnd::Field(Reference::Varnode { .. }) => (),
                    };
                }
            }
            Block::Expansive { .. } => todo!(),
        }
        if !checks.is_empty() {
            self.check = quote! {
                if !(#checks) {
                    return None;
                }
            };
        }
    }
    pub fn gen_sub_constraints(
        &mut self,
        pattern: &'a sleigh_rs::SubPattern,
    ) -> TokenStream {
        let check = pattern.blocks().iter().filter_map(|block| {
            use sleigh_rs::semantic::pattern::*;
            //TODO how to merge multiple blocks?
            match block {
                SubBlock::Or { fields, .. } => {
                    let op = quote! {||};
                    fields
                        .iter()
                        .map(|field| match field {
                            FieldOr::Constraint {
                                assembly,
                                constraint,
                                ..
                            } => {
                                let value = self.expr(&constraint.value().expr);
                                let cons_op = pattern_cmp_token(constraint.op());
                                let (token_name, token_type) =
                                    self.ass_field(assembly);
                                quote! { #token_name #cons_op #value as #token_type }
                            }
                            FieldOr::SubPattern(sub_pattern) => {
                                self.gen_sub_constraints(sub_pattern)
                            }
                        })
                        .reduce(|acc, x| quote! {#acc #op #x})
                }
                SubBlock::And { fields, .. } => {
                    let op = quote! {&&};
                    fields
                        .iter()
                        .filter_map(|field| match field {
                            FieldAnd::Constraint { field, constraint } => {
                                let value = self.expr(&constraint.value().expr);
                                let (name, return_type) = match field {
                                    ConstraintVariable::Assembly {
                                        assembly,
                                        ..
                                    } => self.ass_field(assembly),
                                    ConstraintVariable::Varnode {
                                        varnode,
                                        ..
                                    } => self.context_field(varnode),
                                };
                                let cons_op = pattern_cmp_token(constraint.op());
                                Some(
                                    quote! { #name #cons_op #value as #return_type },
                                )
                            }
                            FieldAnd::SubPattern(sub_pattern) => {
                                Some(self.gen_sub_constraints(sub_pattern))
                            }
                            FieldAnd::Field(Reference::Table { .. }) => {
                                //TODO remove this from the struct
                                unreachable!()
                            }
                            FieldAnd::Field(
                                Reference::Assembly { .. }
                                | Reference::Varnode { .. },
                            ) => None,
                        })
                        .reduce(|acc, x| quote! {#acc #op #x})
                }
            }
        });
        quote! {
            (#(#check)*)
        }
    }
    //the block will use this assebly, so create a variable for it, if it does
    //not already exists
    fn ass_field(&mut self, ass: &Rc<Assembly>) -> (&Ident, &WorkType) {
        match &ass.assembly_type {
            sleigh_rs::semantic::assembly::AssemblyType::Epsilon => {
                todo!("epsilon?")
            }
            sleigh_rs::semantic::assembly::AssemblyType::Start(_) => {
                //TODO: is inst_start check this even possible?
                (self.inst_start, self.inst_work_type)
            }
            sleigh_rs::semantic::assembly::AssemblyType::Next(_) => {
                todo!("inst_next before the pattern match?")
            }
            sleigh_rs::semantic::assembly::AssemblyType::Field(_) => {
                //if this variable was previuslly created, return it,
                //otherwise create it
                let ptr = Rc::as_ptr(ass);
                if self.ass_vars.contains_key(&ptr) {
                    let (ident, token) = self.ass_vars.get(&ptr).unwrap();
                    return (ident, token.return_type());
                }
                let token_parser_current =
                    self.token_parser_current.as_ref().unwrap();
                let token_field = self.token_parser.field(ass);
                let token_read_name = token_field.read();
                let token_name =
                    format_ident!("{}", snake_case(from_sleigh(&ass.name)));

                self.var_creation.extend(quote! {
                    let #token_name = #token_parser_current.#token_read_name();
                });

                self.ass_vars
                    .insert(ptr, (token_name, Rc::clone(token_field)))
                    .map(|_| unreachable!());
                let (ident, token) = self.ass_vars.get(&ptr).unwrap();
                (ident, token.return_type())
            }
        }
    }
    //get var name on that contains the this context value
    fn context_field(&mut self, varnode: &Rc<Varnode>) -> (&Ident, &WorkType) {
        let ptr = Rc::as_ptr(varnode);
        //if the variable exists, return it
        if self.context_vars.contains_key(&ptr) {
            let (ident, context_type, _) = self.context_vars.get(&ptr).unwrap();
            return (ident, context_type);
        }
        //otherwise create it
        let context = self.context_trait.varnode(varnode);
        let context_func = context.read();
        let context_name =
            format_ident!("{}", snake_case(from_sleigh(&varnode.name)));
        let context_param = self.context_param;
        let context_type = *context.return_type();

        //the mut is in case we want to modify this value on the disassembly
        let pre_build = quote! {
            let mut #context_name = #context_param.#context_func();
        };
        self.var_creation.extend(pre_build);
        self.context_vars
            .insert(ptr, (context_name, context_type, Rc::clone(varnode)))
            .map(|_| unreachable!());
        let (ident, context_type, _) = self.context_vars.get(&ptr).unwrap();
        (ident, context_type)
    }
}

//Block parser only use Disassembly for the pattern value, so only value is used
impl<'a> DisassemblyGenerator for BlockParser<'a> {
    fn global_set(&mut self, _global_set: &GlobalSet) -> TokenStream {
        unreachable!()
    }
    fn value(
        &mut self,
        value: &sleigh_rs::semantic::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::semantic::disassembly::*;
        match value {
            ReadScope::Integer(value) => quote! {(#value as i64)},
            ReadScope::Varnode(varnode) => {
                let (name, _var_type) = self.context_field(varnode);
                //TODO solve IntTypeS instead of i64
                quote! { i64::try_from(#name).unwrap() }
            }
            ReadScope::Assembly(ass) => {
                let (name, _ass_type) = self.ass_field(ass);
                //TODO solve IntTypeS instead of i64
                quote! { i64::try_from(#name).unwrap() }
            }
            ReadScope::Local(_) => unreachable!(),
        }
    }
    fn set_context(
        &mut self,
        _context: &Rc<Varnode>,
        _value: TokenStream,
    ) -> TokenStream {
        unreachable!()
    }
    fn new_variable(&mut self, _var: &Rc<Variable>) {
        unreachable!()
    }
    fn var_name(&mut self, _var: &Rc<Variable>) -> TokenStream {
        unreachable!()
    }
}

pub struct DisassemblerConstructor<'a> {
    context_trait: &'a ContextTrait,
    global_set: &'a crate::builder::GlobalSet,

    inst_start: &'a Ident,
    inst_next: &'a Option<Ident>,
    inst_work_type: &'a WorkType,
    global_set_param: &'a Ident,
    context_param: &'a Ident,

    //things that need to be done before the disassembly
    build_pre_disassembly: Vec<TokenStream>,
    //things that need to be done after the disassembly
    build_pos_disassembly: Vec<TokenStream>,

    //vars that where declared outside the disassembly and can be used without
    //declaration
    deref_ass: bool,
    existing_ass:
        &'a HashMap<*const sleigh_rs::Assembly, (Ident, Rc<TokenField>)>,
    deref_var: bool,
    existing_var: &'a HashMap<*const Variable, (Ident, Rc<Variable>)>,

    //variables created during the execution of this disassembly
    calc_fields: &'a mut HashMap<*const Variable, (Ident, Rc<Variable>)>,
    context_vars: HashMap<*const Varnode, (Ident, WorkType, Rc<Varnode>)>,
}

impl<'a> DisassemblerConstructor<'a> {
    pub fn disassembly(
        context_trait: &'a ContextTrait,
        global_set: &'a crate::builder::GlobalSet,

        inst_start: &'a Ident,
        inst_next: &'a Option<Ident>,
        inst_work_type: &'a WorkType,
        global_set_param: &'a Ident,
        context_param: &'a Ident,

        deref_ass: bool,
        existing_ass: &'a HashMap<
            *const sleigh_rs::Assembly,
            (Ident, Rc<TokenField>),
        >,
        deref_var: bool,
        existing_var: &'a HashMap<*const Variable, (Ident, Rc<Variable>)>,

        calc_fields: &'a mut HashMap<*const Variable, (Ident, Rc<Variable>)>,
        disassembly: &sleigh_rs::Disassembly,
    ) -> TokenStream {
        let mut helper = Self {
            context_trait,
            global_set,

            inst_start,
            inst_next,
            inst_work_type,
            global_set_param,
            context_param,

            //things that need to be done before the disassembly
            build_pre_disassembly: vec![],
            //things that need to be done after the disassembly
            build_pos_disassembly: vec![],

            deref_ass,
            existing_ass,
            deref_var,
            existing_var,

            calc_fields,
            context_vars: HashMap::new(),
        };
        helper.generate(disassembly)
    }

    pub fn generate(
        &mut self,
        disassembly: &sleigh_rs::Disassembly,
    ) -> TokenStream {
        //non pos disassembly is done on the pattern itself
        if !disassembly.pos {
            return quote! {};
        }
        let disassembly =
            self.disassembly(&disassembly.vars, &disassembly.assertations);

        let pre_build_disassembly =
            std::mem::take(&mut self.build_pre_disassembly);
        let pos_build_disassembly =
            std::mem::take(&mut self.build_pos_disassembly);
        quote! {
            #(#pre_build_disassembly)*
            #disassembly
            #(#pos_build_disassembly)*
        }
    }
    //get var name on that contains the this assembly field value
    fn ass_field(
        &mut self,
        ass: &Rc<sleigh_rs::Assembly>,
    ) -> (TokenStream, WorkType) {
        match &ass.assembly_type {
            sleigh_rs::semantic::assembly::AssemblyType::Epsilon => {
                unreachable!("epsilon field?")
            }
            sleigh_rs::semantic::assembly::AssemblyType::Start(_) => {
                let inst_start = &self.inst_start;
                (quote! {#inst_start}, *self.inst_work_type)
            }
            sleigh_rs::semantic::assembly::AssemblyType::Next(_) => {
                let inst_next = self.inst_next.as_ref().unwrap();
                (quote! {#inst_next}, *self.inst_work_type)
            }
            sleigh_rs::semantic::assembly::AssemblyType::Field(_) => {
                //can't create new ass fields during disassembly, all
                //fields used need to be declared on the pattern
                let ptr = Rc::as_ptr(ass);
                let (ident, field) = self.existing_ass.get(&ptr).unwrap();
                let deref = self.deref_ass.then(|| quote! {*});
                return (quote! {#deref #ident}, *field.return_type());
            }
        }
    }
    //get var name on that contains the this context value
    fn context_field(&mut self, varnode: &Rc<Varnode>) -> (&Ident, &WorkType) {
        let ptr = Rc::as_ptr(varnode);
        //if the variable exists, return it
        if self.context_vars.contains_key(&ptr) {
            let (ident, context_type, _) = self.context_vars.get(&ptr).unwrap();
            return (ident, context_type);
        }
        //otherwise create it
        let context = self.context_trait.varnode(varnode);
        let context_func = context.read();
        let context_name =
            format_ident!("{}", snake_case(from_sleigh(&varnode.name)));
        let context_param = &self.context_param;
        let context_type = *context.return_type();

        self.build_pre_disassembly.push(quote! {
            let mut #context_name = #context_param.#context_func();
        });
        self.context_vars
            .insert(ptr, (context_name, context_type, Rc::clone(varnode)))
            .map(|_| unreachable!());
        let (ident, context_type, _) = self.context_vars.get(&ptr).unwrap();
        (ident, context_type)
    }
}

impl<'a> DisassemblyGenerator for DisassemblerConstructor<'a> {
    fn global_set(&mut self, global_set: &GlobalSet) -> TokenStream {
        let context = self.global_set.context(&global_set.context);
        let addr_type = self.inst_work_type;
        use sleigh_rs::semantic::disassembly::AddrScope::*;
        let address = match &global_set.address {
            Int(value) => quote! {(#value as #addr_type)},
            Varnode(varnode) => {
                let (name, _var_type) = self.context_field(varnode);
                //TODO solve IntTypeS instead of i64
                quote! { #addr_type::try_from(#name).unwrap() }
            }
            Assembly(ass) => {
                let (name, _ass_type) = self.ass_field(ass);
                quote! { #addr_type::try_from(#name).unwrap() }
            }
            Local(var) => {
                let name = self.var_name(var);
                quote! {#addr_type::try_from(#name).unwrap()}
            }
            Table(_) => todo!(),
        };
        let set_function = context.function();
        let value_type = context.value_type();
        let global_set_param = self.global_set_param;
        let (value, _value_type) = self.context_field(&global_set.context);
        quote! {
            #global_set_param.#set_function(
                #address,
                #value_type::try_into(#value).unwrap()
            );
        }
    }
    fn value(
        &mut self,
        value: &sleigh_rs::semantic::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::semantic::disassembly::ReadScope;
        match value {
            ReadScope::Integer(value) => quote! {(#value as i64)},
            ReadScope::Varnode(varnode) => {
                let (name, _var_type) = self.context_field(varnode);
                //TODO solve IntTypeS instead of i64
                quote! { i64::try_from(#name).unwrap() }
            }
            ReadScope::Assembly(ass) => {
                let (name, _ass_type) = self.ass_field(ass);
                //TODO solve IntTypeS instead of i64
                quote! { i64::try_from(#name).unwrap() }
            }
            ReadScope::Local(var) => self.var_name(var),
        }
    }

    fn set_context(
        &mut self,
        varnode: &Rc<sleigh_rs::Varnode>,
        value: TokenStream,
    ) -> TokenStream {
        let context_write_func =
            self.context_trait.varnode(varnode).write().unwrap();
        let context_param = self.context_param;
        //this context variable need to be written back into the context trait
        //after the disassembly is finished
        let (name, context_type) = self.context_field(varnode);
        let (name, context_type) = (name.clone(), *context_type);
        self.build_pos_disassembly.push(quote! {
            #context_param.#context_write_func(#name);
        });
        //update the variable during the disassembly
        quote! { #name = #context_type::try_from(#value).unwrap(); }
    }

    fn new_variable(&mut self, var: &Rc<Variable>) {
        let ptr = Rc::as_ptr(var);
        //if this variable is already declare, does nothing
        if self.existing_var.contains_key(&ptr) {
            return;
        }
        let var_name =
            format_ident!("{}", snake_case(from_sleigh(var.name.as_ref())));
        let zero: IntTypeS = 0;
        //variable used used during the disassembly, initialized it with zero
        self.build_pre_disassembly.push(quote! {
            let mut #var_name = #zero;
        });
        self.calc_fields
            .insert(ptr, (var_name, Rc::clone(var)))
            .map(|_| unreachable!("Variable duplicated"));
    }

    fn var_name(&mut self, var: &Rc<Variable>) -> TokenStream {
        let ptr = Rc::as_ptr(var);
        self.existing_var
            .get(&ptr)
            .map(|(name, _)| {
                let deref = self.deref_var.then(|| quote! {*});
                quote! {#deref #name}
            })
            .or_else(|| {
                self.calc_fields.get(&ptr).map(|(name, _)| quote! {#name})
            })
            .unwrap()
    }
}
