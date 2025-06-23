use spinners::{Spinner, Spinners};

use crate::{
    bliss_lib::prelude::PRELUDE,
    compiler::{ast, lexer, macro_expander, parser::Parser, type_inference::TypeInference},
};
use std::{error::Error, fs};

pub fn generate(input_filename: &str, debug: bool, no_prelude: bool) -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string(input_filename)?;

    let full_contents = if no_prelude {
        input
    } else {
        PRELUDE.to_string() + &input
    };

    println!("==> Generating: {} -> {}", input_filename, input_filename);

    let mut sp = Spinner::new(Spinners::Dots, "Lexing source".to_string());

    let tokens = lexer::tokenize(&full_contents);

    sp.stop_with_symbol("✅");

    if debug {
        println!("TOKENS:");
        println!("{:#?}", tokens);
    }

    let mut sp = Spinner::new(Spinners::Dots, "Parsing tokens".to_string());

    let atom_tree = Parser::default().categorize(&tokens)?;

    sp.stop_with_symbol("✅");

    if debug {
        println!("ATOM TREE:");
        println!("{:#?}", atom_tree);
    }

    let mut sp = Spinner::new(Spinners::Dots, "Expanding macros".to_string());

    let expanded_atom_tree = macro_expander::expand(&atom_tree)?;

    sp.stop_with_symbol("✅");

    if debug {
        println!("EXPANDED ATOM TREE:");
        println!("{:#?}", expanded_atom_tree);
    }

    let mut sp = Spinner::new(Spinners::Dots, "Constructing AST".to_string());

    let ast = ast::construct(&expanded_atom_tree)?;

    sp.stop_with_symbol("✅");

    if debug {
        println!("AST:");
        println!("{:#?}", ast);
    }

    let mut sp = Spinner::new(Spinners::Dots, "Inferring types".to_string());

    let typed_ast = TypeInference::default().infer(&ast)?;

    sp.stop_with_symbol("✅");

    if debug {
        println!("TYPED AST:");
        println!("{:#?}", typed_ast);
    }

    let mut sp = Spinner::new(Spinners::Dots, "Emitting IR".to_string());

    // let ir = codegen::emit(&typed_ast)?;

    sp.stop_with_symbol("✅");

    // if debug {
    //     println!("IR:");
    //     println!("{:#?}", ir);
    // }

    println!("Generated IR: {}", input_filename);

    Ok(())
}
