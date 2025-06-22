use std::{collections::VecDeque, error::Error};

use super::Atom;

pub struct Parser {
    tokens: VecDeque<String>,
    stack: VecDeque<Atom>,
    current: Atom,
}

fn to_atom(token: String) -> Atom {
    match token.parse::<f64>() {
        Ok(number) => Atom::Number(number),
        Err(_) => Atom::Symbol(token),
    }
}

impl Default for Parser {
    fn default() -> Self {
        Parser {
            tokens: VecDeque::new(),
            stack: VecDeque::new(),
            current: Atom::List(vec![]),
        }
    }
}

impl Parser {
    fn categorize_string(&mut self) -> Result<(), Box<dyn Error>> {
        match Vec::from(self.tokens.clone()).as_slice() {
            [string_content, end_token, new_tokens @ ..] if end_token == "\"" => {
                let new_atom = Atom::String(string_content.to_string());

                self.current.add(new_atom)?;
                self.tokens = VecDeque::from(new_tokens.to_vec());
                Ok(())
            }

            _ => Err(format!("Unexpected atom in string: {:#?}", self.tokens).into()),
        }
    }

    fn categorize_closing(&mut self, str_rep: &str) -> Result<(), Box<dyn Error>> {
        match self.stack.pop_back() {
            Some(mut new_current) => {
                new_current.add(self.current.clone())?;

                self.current = new_current;
                Ok(())
            }

            None => Err(format!("Unexpected atom in closing: {}", str_rep).into()),
        }
    }

    fn categorize_opening(&mut self, new_current: Atom) {
        self.stack.push_back(self.current.clone());
        self.current = new_current;
    }

    fn categorize_else(&mut self, token: String) -> Result<(), Box<dyn Error>> {
        let new_atom = to_atom(token);

        self.current.add(new_atom)?;
        Ok(())
    }

    fn categorize_token(&mut self, token: String) -> Result<(), Box<dyn Error>> {
        match token.as_str() {
            "\"" => self.categorize_string()?,
            ")" => self.categorize_closing(")")?,
            "]" => self.categorize_closing("]")?,
            "(" => self.categorize_opening(Atom::List(vec![])),
            "[" => self.categorize_opening(Atom::ValueList(vec![])),
            _ => self.categorize_else(token)?,
        }

        Ok(())
    }

    pub fn categorize(&mut self, tokens: &[String]) -> Result<Atom, Box<dyn Error>> {
        self.tokens = VecDeque::from(tokens.to_owned());

        while let Some(token) = self.tokens.pop_front() {
            self.categorize_token(token)?
        }

        Ok(self.current.clone())
    }
}
