import gleam/io
import lib/bliss/prelude
import lib/compiler/ast
import lib/compiler/lexer
import lib/compiler/macro_expander
import lib/compiler/parser
import lib/utils
import simplifile

pub fn generate(
  input_filename: String,
  debug debug: Bool,
  no_prelude no_prelude: Bool,
) {
  let assert Ok(input) = simplifile.read(from: input_filename)

  let full_contents = case no_prelude {
    True -> input
    False -> prelude.self <> input
  }

  io.println("==> Generating: " <> input_filename <> " -> " <> input_filename)

  io.println("    * Lexing source")

  let tokens = lexer.tokenize(full_contents)

  utils.do_if(debug, fn() {
    io.println("TOKENS:")
    io.println(utils.styled(tokens))
  })

  io.println("    * Parsing tokens")

  let atom_tree = parser.categorize(tokens)

  utils.do_if(debug, fn() {
    io.println("ATOM TREE:")
    io.println(utils.styled(atom_tree))
  })

  io.println("    * Expanding macros")

  let expanded_atom_tree = macro_expander.expand(atom_tree)

  utils.do_if(debug, fn() {
    io.println("EXPANDED ATOM TREE:")
    io.println(utils.styled(expanded_atom_tree))
  })

  io.println("    * Constructing AST")

  let ast = ast.construct(expanded_atom_tree)

  utils.do_if(debug, fn() {
    io.println("AST:")
    io.println(utils.styled(ast))
  })

  io.println("    * Emitting IR")

  // let ir = codegen.emit(ast)
  //
  // utils.do_if(debug, fn() {
  //   io.println("IR:")
  //   io.println(utils.pprint(ir))
  // })

  io.println("    * Generated IR: " <> input_filename)

  io.println("\nResult:")
  io.println(utils.styled(ast))
}
