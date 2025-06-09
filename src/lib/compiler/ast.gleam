import gleam/list
import lib/compiler/mod.{
  type Atom, type Node, ConditionalBranchNode, ConditionalNode, DoBlockNode,
  LambdaFunctionNode, LetBindingNode, ListAtom, NumberAtom, NumberNode,
  ProgramNode, StringAtom, StringNode, SymbolAtom, SymbolNode, ValueListAtom,
  ValueListNode,
}
import lib/utils

fn construct_conditional_branch(branch: Atom) {
  case branch {
    ListAtom([pred, ..body]) -> {
      let pred = handle_atom(pred)
      let body = handle_atom_list(body)

      ConditionalBranchNode(pred:, body:)
    }
    atom -> panic as { "Invalid conditional branch: " <> utils.pprint(atom) }
  }
}

fn construct_conditional(atoms: List(Atom)) -> Node {
  let len = list.length(atoms)

  case len > 0 && len % 2 == 0 {
    False ->
      panic as { "Imbalanced conditional branches: " <> utils.pprint(atoms) }
    True -> {
      let branches = list.map(atoms, construct_conditional_branch)

      ConditionalNode(branches:)
    }
  }
}

fn construct_do_block(atoms: List(Atom)) -> Node {
  // let len = list.length(atoms)
  //
  // case len > 1 {
  //   False -> panic as "Empty or single-element do block"
  //   True -> {
  let body = handle_atom_list(atoms)

  DoBlockNode(body:)
  //   }
  // }
}

fn construct_macro_definition(atoms: List(Atom)) {
  case atoms {
    [SymbolAtom(name), ValueListAtom(params), ..body] -> {
      case list.all(params, mod.is_symbol_atom) {
        False ->
          panic as {
            "Invalid macro definition parameters" <> utils.pprint(params)
          }
        True -> {
          let params = handle_atom_list(params)
          let body = handle_atom_list(body)

          mod.MacroDefinitionNode(name:, params:, body:)
        }
      }
    }
    atoms -> panic as { "Malformed macro definition: " <> utils.pprint(atoms) }
  }
}

fn construct_lambda_function(atoms: List(Atom)) -> Node {
  case atoms {
    [ValueListAtom(params), ..body] -> {
      case list.all(params, mod.is_symbol_atom) {
        False ->
          panic as {
            "Invalid lambda function parameters" <> utils.pprint(params)
          }
        True -> {
          let params = handle_atom_list(params)
          let body = handle_atom_list(body)

          LambdaFunctionNode(params:, body:)
        }
      }
    }
    _ -> panic as { "Malformed lambda function" <> utils.pprint(atoms) }
  }
}

fn construct_let_binding(atoms: List(Atom)) -> Node {
  case atoms {
    [SymbolAtom(name), value] -> {
      let value = handle_atom(value)

      LetBindingNode(name:, value:)
    }
    _ -> panic as { "Malformed let binding" <> utils.pprint(atoms) }
  }
}

fn construct_function_call(name: String, atoms: List(Atom)) -> Node {
  let params = handle_atom_list(atoms)

  mod.FunctionCallNode(name:, params:)
}

fn handle_list(atoms: List(Atom)) -> Node {
  case atoms {
    [SymbolAtom("cond"), ..atoms] -> construct_conditional(atoms)
    [SymbolAtom("do"), ..atoms] -> construct_do_block(atoms)
    [SymbolAtom("defmacro"), ..atoms] -> construct_macro_definition(atoms)
    [SymbolAtom("fn"), ..atoms] -> construct_lambda_function(atoms)
    [SymbolAtom("let"), ..atoms] -> construct_let_binding(atoms)
    [SymbolAtom(name), ..atoms] -> construct_function_call(name, atoms)
    _ -> panic as { "Malformed expression" <> utils.pprint(atoms) }
  }
}

fn handle_value_list(atoms: List(Atom)) -> Node {
  let items = handle_atom_list(atoms)

  ValueListNode(items:)
}

fn handle_number(num: Float) -> Node {
  NumberNode(value: num)
}

fn handle_string(str: String) -> Node {
  StringNode(value: str)
}

fn handle_symbol(sym: String) -> Node {
  SymbolNode(name: sym)
}

fn handle_atom_list(atoms: List(Atom)) -> List(Node) {
  list.map(atoms, handle_atom)
}

fn handle_atom(atom: Atom) -> Node {
  case atom {
    ValueListAtom(atoms) -> handle_value_list(atoms)
    NumberAtom(num) -> handle_number(num)
    StringAtom(str) -> handle_string(str)
    SymbolAtom(sym) -> handle_symbol(sym)
    ListAtom(atoms) -> handle_list(atoms)
  }
}

pub fn construct(atom: Atom) -> Node {
  let assert ListAtom(atoms) = atom as "Program has to be wrapped in a ListAtom"

  let body = handle_atom_list(atoms)

  ProgramNode(body:)
}
