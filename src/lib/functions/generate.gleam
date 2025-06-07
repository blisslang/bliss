import gleam/io
import lib/compiler/codegen
import lib/compiler/lexer
import lib/compiler/parser
import pprint
import simplifile

pub fn generate(input_filename: String) {
  let assert Ok(contents) = simplifile.read(from: input_filename)

  io.println("==> Generating: " <> input_filename <> " -> " <> input_filename)

  io.println("    * Lexing source")

  let tokens = lexer.tokenize(contents)
  io.println("TOKENS:")
  pprint.debug(tokens)

  io.println("    * Parsing tokens")

  let ast = parser.categorize(tokens)
  io.println("AST:")
  pprint.debug(ast)

  io.println("    * Emitting IR")

  let ir = codegen.emit(ast)
  io.println("IR:")
  pprint.debug(ir)

  io.println("    * Generated IR: " <> input_filename)
}
