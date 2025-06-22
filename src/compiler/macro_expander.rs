use std::{collections::HashMap, error::Error};

use clap::error::Result;
use lit_vek::vek;

use super::Atom;

#[derive(Debug)]
struct MacroDef {
    params: Vec<String>,
    body: Vec<Atom>,
}

type RegisteredMacros = HashMap<String, MacroDef>;

type SubstitutionArgs = HashMap<String, Atom>;

// TODO: Macros are right now only registered in the top-level scope
fn register_macro_defs(atoms: Vec<Atom>) -> Result<(RegisteredMacros, Vec<Atom>), Box<dyn Error>> {
    let mut registered_macros = RegisteredMacros::new();
    let mut rest_atoms = vec![];

    for atom in atoms {
        match &atom {
            // (defmacro name [param1 ...] body...)
            Atom::List(items) => match items.as_slice() {
                [
                    Atom::Symbol(s),
                    Atom::Symbol(name),
                    Atom::ValueList(params),
                    body @ ..,
                ] if s == "defmacro" => {
                    let string_params = params
                        .iter()
                        .map_while(|p| match p {
                            Atom::Symbol(name) => Some(name.clone()),
                            _ => None,
                        })
                        .collect::<Vec<String>>();

                    if string_params.len() != params.len() {
                        return Err(format!("Invalid macro parameters: {:#?}", params).into());
                    }

                    let new_macro_def = MacroDef {
                        params: string_params,
                        body: body.to_vec(),
                    };

                    registered_macros.insert(name.clone(), new_macro_def);
                }
                // Any other atom is kept as is
                _ => rest_atoms.push(atom),
            },
            _ => rest_atoms.push(atom),
        }
    }

    Ok((registered_macros, rest_atoms))
}

fn substitute_macro_arg(atom: &Atom, substitution_args: &SubstitutionArgs) -> Atom {
    match atom {
        // Variadic arg
        Atom::Symbol(name) if name.starts_with("&") => {
            let (macro_params, macro_args): (Vec<&String>, Vec<&Atom>) =
                substitution_args.iter().unzip();

            match macro_params.iter().position(|p| p == &name) {
                Some(param_idx) => {
                    let (_, rest_args) = macro_args.split_at(param_idx);

                    let owned_rest_args =
                        rest_args.iter().map(|&a| a.clone()).collect::<Vec<Atom>>();

                    // Wrap in a do block just in case
                    Atom::List(vek![Atom::Symbol("do".to_string()), ...owned_rest_args])
                }
                None => atom.clone(),
            }
        }
        // Regular arg
        Atom::Symbol(name) => substitution_args.get(name).unwrap_or(atom).clone(),
        // Continue down in children
        Atom::List(children) => Atom::List(
            children
                .iter()
                .map(|a| substitute_macro_arg(a, substitution_args))
                .collect(),
        ),
        Atom::ValueList(children) => Atom::ValueList(
            children
                .iter()
                .map(|a| substitute_macro_arg(a, substitution_args))
                .collect(),
        ),
        // Anything else do nothing
        _ => atom.clone(),
    }
}

fn create_substitution_args(
    macro_def_params: &[String],
    macro_args: &[Atom],
) -> Result<SubstitutionArgs, Box<dyn Error>> {
    let mut substitution_args = SubstitutionArgs::new();

    let variadic_args_amount = macro_def_params
        .iter()
        .filter(|p| p.starts_with("&"))
        .collect::<Vec<&String>>()
        .len();

    match variadic_args_amount {
        // Variadic args
        1 => match macro_def_params.last() {
            Some(last_param) if last_param.starts_with("&") => {
                let variadic_arg_idx = macro_def_params.len() - 1;

                let (regular_macro_def_params, variadic_macro_def_params) =
                    macro_def_params.split_at(variadic_arg_idx);

                if variadic_macro_def_params.len() > 1 {
                    return Err("Macro definition can only have one variadic parameter".into());
                }
                let variadic_macro_def_param = &variadic_macro_def_params[0];

                let (regular_args, variadic_args) = macro_args.split_at(variadic_arg_idx);

                regular_macro_def_params
                    .iter()
                    .zip(regular_args)
                    .for_each(|(k, v)| {
                        substitution_args.insert(k.clone(), v.clone());
                    });

                substitution_args.insert(
                    variadic_macro_def_param.clone(),
                    Atom::List(vek![Atom::Symbol("do".to_string()), ...variadic_args.to_vec()]),
                );
            }

            _ => return Err("Variadic macro argument has to be in the last position".into()),
        },
        // Regular args
        0 => {
            macro_def_params.iter().zip(macro_args).for_each(|(k, v)| {
                substitution_args.insert(k.clone(), v.clone());
            });
        }

        _ => return Err("Macros can only have one variadic argument".into()),
    }

    Ok(substitution_args)
}

fn substitute_macro_args(
    macros: RegisteredMacros,
    atoms: Vec<Atom>,
) -> Result<Vec<Atom>, Box<dyn Error>> {
    let substituted_macro_args = atoms
        .iter()
        .map(|atom| match atom {
            // check if a list is a macro call (macro-name arg1 arg2 ...)
            Atom::List(items) => match items.as_slice() {
                [Atom::Symbol(name), args @ ..] => match macros.get(name) {
                    Some(macro_def) => {
                        let substitution_args = create_substitution_args(&macro_def.params, args)?;

                        let substituted_body = macro_def
                            .body
                            .iter()
                            .map(|body_atom| substitute_macro_arg(body_atom, &substitution_args))
                            .collect::<Vec<Atom>>();

                        // Wrap in a do block just in case
                        Ok(Atom::List(
                            vek![Atom::Symbol("do".to_string()), ...substituted_body],
                        ))
                    }
                    // not a macro call, do nothing
                    None => Ok(atom.clone()),
                },
                _ => Ok(atom.clone()),
            },
            _ => Ok(atom.clone()),
        })
        .collect::<Result<Vec<Atom>, Box<dyn Error>>>()?;

    Ok(substituted_macro_args)
}

pub fn expand(atom: &Atom) -> Result<Atom, Box<dyn Error>> {
    if let Atom::List(atoms) = atom.clone() {
        let (macros, rest_atoms) = register_macro_defs(atoms)?;

        // println!("MACROS: {:#?}", macros);
        // println!("REST ATOMS: {:#?}", rest_atoms);

        let substituted_atoms = substitute_macro_args(macros, rest_atoms)?;

        Ok(Atom::List(substituted_atoms))
    } else {
        Err("Program has to be wrapped in a List".into())
    }
}
