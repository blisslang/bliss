use super::Node;
use crate::utils::drop_while_inclusive;
use std::collections::VecDeque;

fn to_atom(x: &str) -> Node {
    match x.parse::<f32>() {
        Ok(f) => Node::Number(f),
        Err(_) => Node::Symbol(String::from(x)),
    }
}

fn to_string(x: &str) -> Node {
    Node::String(String::from(x))
}

#[derive(Default)]
pub struct Categorizer {
    stack: VecDeque<Node>,
    current: Node,
}

impl Categorizer {
    fn categorize_closing(&mut self, str_repr: &str) -> Result<(), String> {
        if let Some(mut new_current) = self.stack.pop_front() {
            new_current.add(self.current.to_owned())?;
            self.current = new_current;
            return Ok(());
        }
        Err(format!("Unexpected '{}'", str_repr))
    }

    fn categorize_else(
        &mut self,
        token: &str,
        type_fun: &dyn Fn(&str) -> Node,
    ) -> Result<(), String> {
        self.current.add(type_fun(token))?;
        Ok(())
    }

    fn categorize_opening(&mut self, new_current: Node) {
        self.stack.push_back(self.current.to_owned());
        self.current = new_current;
    }

    fn categorize_comment(&mut self, rest: &mut VecDeque<String>) {
        drop_while_inclusive(rest, |elem| elem != ";");
    }

    pub fn categorize(&mut self, tokens: Vec<String>) -> Result<Node, String> {
        let mut tokens_deque = VecDeque::from(tokens);

        while let Some(token) = tokens_deque.pop_front() {
            println!(
                "TOKEN: {}\nREST: {:?}\n BEFORE STACK: {:?}\nBEFORE CURRENT: {:?}",
                token, tokens_deque, self.stack, self.current
            );

            match self.current {
                Node::String(ref string_value) => match token.as_str() {
                    "\"" if string_value.ends_with("\\") => self.categorize_closing("\"")?,

                    _ => self.categorize_else(&token, &to_string)?,
                },

                _ => match token.as_str() {
                    ";" => self.categorize_comment(&mut tokens_deque),

                    "(" => self.categorize_opening(Node::List(vec![])),
                    "[" => self.categorize_opening(Node::ValueList(vec![])),
                    "\"" => self.categorize_opening(Node::String(String::from(""))),

                    ")" => self.categorize_closing(")")?,
                    "]" => self.categorize_closing("]")?,

                    _ => self.categorize_else(&token, &to_atom)?,
                },
            }

            println!(
                "AFTER STACK: {:?}\nAFTER CURRENT: {:?}\n",
                self.stack, self.current
            );
        }

        Ok(self.current.to_owned())
    }
}
