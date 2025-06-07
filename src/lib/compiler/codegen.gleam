import lib/compiler/macro_expander
import lib/compiler/node.{type Node}

pub fn emit(ast: Node) -> String {
  let expanded_ast = macro_expander.expand([ast])
  let _ = expanded_ast

  ""
}
