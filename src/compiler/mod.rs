use std::{collections::HashMap, error::Error};

pub mod ast;
pub mod lexer;
pub mod macro_expander;
pub mod parser;
pub mod type_inference;

#[derive(Debug, Clone)]
pub enum Atom {
    List(Vec<Atom>),
    ValueList(Vec<Atom>),
    String(String),
    Symbol(String),
    Number(f64),
}

impl Atom {
    pub fn add(&mut self, new_atom: Atom) -> Result<(), Box<dyn Error>> {
        match self {
            Atom::List(vec) | Atom::ValueList(vec) => {
                vec.push(new_atom);

                Ok(())
            }

            _ => Err(format!("Expected on of (List ValueList), got: {:#?}", self).into()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Program { body: Vec<Node> },

    // Conditional { branches: Vec<Node> },
    // Branch { pred: Box<Node>, body: Vec<Node> },
    DoBlock { body: Vec<Node> },

    LambdaFunction { params: Vec<Node>, body: Vec<Node> },
    LetBinding { name: String, value: Box<Node> },
    FunctionCall { name: String, params: Vec<Node> },
    Variable { name: String },

    List { value: Vec<Node> },
    String { value: String },
    Number { value: f64 },
    Boolean { value: bool },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit {
        t: Option<TypeForm>,
    },

    Number {
        t: Option<TypeForm>,
        value: f64,
    },
    String {
        t: Option<TypeForm>,
        value: String,
    },
    Boolean {
        t: Option<TypeForm>,
        value: bool,
    },

    DoBlock {
        t: Option<TypeForm>,
        body: Box<Expr>,
    },

    Variable {
        t: Option<TypeForm>,
        var_name: String,
    },
    FunctionCall {
        t: Option<TypeForm>,
        fn_name: Box<Expr>,
        arg: Box<Expr>,
    },

    LambdaFunction {
        t: Option<TypeForm>,
        arg_name: Option<String>,
        body: Box<Expr>,
    },
    LetBinding {
        t: Option<TypeForm>,
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },

    List {
        t: Option<TypeForm>,
        value: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Number,
    String,
    Boolean,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeForm {
    Constant {
        t: Type,
    },
    Variable {
        name: String,
    },
    Arrow {
        arg: Box<TypeForm>,
        ret: Box<TypeForm>,
    },
    Record {
        fields: HashMap<String, TypeForm>,
    },
    List {
        items: Box<TypeForm>,
    },
}
