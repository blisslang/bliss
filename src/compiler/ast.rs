use std::error::Error;

use super::{Atom, Node};

fn construct_branch(branch: Atom) -> Result<Node, Box<dyn Error>> {
    match branch {
        Atom::List(items) => match items.as_slice() {
            [pred, body @ ..] => {
                let pred = Box::new(handle_atom(pred)?);
                let body = handle_atom_list(body)?;

                Ok(Node::Branch { pred, body })
            }

            _ => Err(format!("Invalid conditional branch: {:#?}", items).into()),
        },
        _ => Err(format!("Expected one of (List), got: {:#?}", branch).into()),
    }
}

fn construct_conditional(atoms: &[Atom]) -> Result<Node, Box<dyn Error>> {
    if atoms.is_empty() {
        return Err(format!("Invalid conditional branches: {:#?}", atoms).into());
    }

    let branches = atoms
        .iter()
        .map(|a| construct_branch(a.clone()))
        .collect::<Result<Vec<Node>, Box<dyn Error>>>()?;

    Ok(Node::Conditional { branches })
}

fn construct_do_block(atoms: &[Atom]) -> Result<Node, Box<dyn Error>> {
    let body = handle_atom_list(atoms)?;

    Ok(Node::DoBlock { body })
}

fn construct_lambda_function(atoms: &[Atom]) -> Result<Node, Box<dyn Error>> {
    match atoms {
        [Atom::ValueList(params), body @ ..] => {
            if !params.iter().all(|p| matches!(p, Atom::Symbol(_))) {
                return Err(format!("Invalid lambda function parameters: {:#?}", params).into());
            }

            let params = handle_atom_list(params)?;
            let body = handle_atom_list(body)?;

            Ok(Node::LambdaFunction { params, body })
        }

        _ => Err(format!("Malformed lambda function: {:#?}", atoms).into()),
    }
}

fn construct_let_binding(atoms: &[Atom]) -> Result<Node, Box<dyn Error>> {
    match atoms {
        [Atom::Symbol(name), value] => {
            let value = Box::new(handle_atom(value)?);

            Ok(Node::LetBinding {
                name: name.clone(),
                value,
            })
        }

        _ => Err(format!("Malformed let binding: {:#?}", atoms).into()),
    }
}

fn construct_function_call(name: &str, atoms: &[Atom]) -> Result<Node, Box<dyn Error>> {
    let params = handle_atom_list(atoms)?;

    Ok(Node::FunctionCall {
        name: name.to_string(),
        params,
    })
}

fn handle_list(atoms: &Vec<Atom>) -> Result<Node, Box<dyn Error>> {
    match atoms.as_slice() {
        [Atom::Symbol(s), atoms @ ..] if s == "cond" => construct_conditional(atoms),
        [Atom::Symbol(s), atoms @ ..] if s == "do" => construct_do_block(atoms),
        [Atom::Symbol(s), atoms @ ..] if s == "fn" => construct_lambda_function(atoms),
        [Atom::Symbol(s), atoms @ ..] if s == "let" => construct_let_binding(atoms),
        [Atom::Symbol(name), atoms @ ..] => construct_function_call(name, atoms),

        _ => Err(format!("Malformed expression: {:#?}", atoms).into()),
    }
}

fn handle_value_list(atoms: &[Atom]) -> Result<Node, Box<dyn Error>> {
    let items = handle_atom_list(atoms)?;

    Ok(Node::List { value: items })
}

fn handle_string(string: &str) -> Node {
    Node::String {
        value: string.to_string(),
    }
}

fn handle_symbol(name: &str) -> Node {
    match name {
        "true" => Node::Boolean { value: true },
        "false" => Node::Boolean { value: false },
        _ => Node::Variable {
            name: name.to_string(),
        },
    }
}

fn handle_number(number: &f64) -> Node {
    Node::Number { value: *number }
}

fn handle_atom(atom: &Atom) -> Result<Node, Box<dyn Error>> {
    let new_node = match atom {
        Atom::List(atoms) => handle_list(atoms)?,
        Atom::ValueList(atoms) => handle_value_list(atoms)?,
        Atom::String(string) => handle_string(string),
        Atom::Symbol(name) => handle_symbol(name),
        Atom::Number(number) => handle_number(number),
    };

    Ok(new_node)
}

fn handle_atom_list(atoms: &[Atom]) -> Result<Vec<Node>, Box<dyn Error>> {
    atoms.iter().map(handle_atom).collect()
}

pub fn construct(atom: &Atom) -> Result<Node, Box<dyn Error>> {
    if let Atom::List(atoms) = atom.clone() {
        let body = handle_atom_list(&atoms)?;

        Ok(Node::Program { body })
    } else {
        Err("Program has to be wrapped in a List".into())
    }
}
