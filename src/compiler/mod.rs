use std::error::Error;

pub mod ast;
pub mod lexer;
pub mod macro_expander;
pub mod parser;

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

    Conditional { branches: Vec<Node> },
    Branch { pred: Box<Node>, body: Vec<Node> },

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
