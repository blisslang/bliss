use compiler::{categorizer::Categorizer, tokenizer::tokenize};

mod compiler;
mod utils;

fn main() {
    let code = "
(
  IO/puts   (String/of-float

   ( Math/fib      8)
  ) ) ";

    let tokens = tokenize(code);
    println!("Tokens: {:#?}", tokens);

    let mut categorizer = Categorizer::default();

    match categorizer.categorize(tokens) {
        Ok(ast) => println!("AST: {:#?}", ast),
        Err(reason) => println!("Categorize error: {}", reason),
    }
}
