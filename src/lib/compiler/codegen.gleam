import lib/compiler/macro_expander
import lib/compiler/mod.{type Atom}

pub fn emit(ast: Atom) -> String {
  let expanded_ast = macro_expander.expand(ast)
  let _ = expanded_ast

  ""
}
