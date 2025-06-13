import gleam/list
import lib/compiler/mod.{
  type Atom, type Node, BranchNode, ConditionalNode, DoBlockNode, FloatType,
  FunctionCallNode, LambdaFunctionNode, LetBindingNode, ListAtom,
  ListNode as ValueListNode, NumberAtom, NumberNode, ProgramNode, StringAtom,
  StringNode, SymbolAtom, UntypedType, ValueListAtom, VariableNode,
}
import lib/utils

fn construct_branch(branch: Atom) {
  case branch {
    ListAtom([pred, ..body]) -> {
      let pred = handle_atom(pred)
      let body = handle_atom_list(body)

      BranchNode(pred:, body:)
    }
    atom -> panic as { "Invalid conditional branch: " <> utils.styled(atom) }
  }
}

fn construct_conditional(atoms: List(Atom)) -> Node {
  case list.length(atoms) > 0 {
    False ->
      panic as { "Invalid conditional branches: " <> utils.styled(atoms) }
    True -> {
      let branches = list.map(atoms, construct_branch)

      ConditionalNode(branches:)
    }
  }
}

fn construct_do_block(atoms: List(Atom)) -> Node {
  let body = handle_atom_list(atoms)

  DoBlockNode(body:)
}

fn construct_lambda_function(atoms: List(Atom)) -> Node {
  case atoms {
    [ValueListAtom(params), ..body] -> {
      case list.all(params, mod.is_symbol_atom) {
        False ->
          panic as {
            "Invalid lambda function parameters: " <> utils.styled(params)
          }
        True -> {
          let params = handle_atom_list(params)
          let body = handle_atom_list(body)

          LambdaFunctionNode(params:, body:)
        }
      }
    }
    _ -> panic as { "Malformed lambda function: " <> utils.styled(atoms) }
  }
}

fn construct_let_binding(atoms: List(Atom)) -> Node {
  case atoms {
    [SymbolAtom(name), value] -> {
      let value = handle_atom(value)

      LetBindingNode(name:, value:)
    }
    _ -> panic as { "Malformed let binding: " <> utils.styled(atoms) }
  }
}

fn construct_function_call(name: String, atoms: List(Atom)) -> Node {
  let params = handle_atom_list(atoms)

  FunctionCallNode(name:, params:)
}

fn handle_list(atoms: List(Atom)) -> Node {
  case atoms {
    [SymbolAtom("cond"), ..atoms] -> construct_conditional(atoms)
    [SymbolAtom("do"), ..atoms] -> construct_do_block(atoms)
    [SymbolAtom("fn"), ..atoms] -> construct_lambda_function(atoms)
    [SymbolAtom("let"), ..atoms] -> construct_let_binding(atoms)
    [SymbolAtom(name), ..atoms] -> construct_function_call(name, atoms)
    _ -> panic as { "Malformed expression: " <> utils.styled(atoms) }
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

fn handle_variable(name: String) -> Node {
  VariableNode(name:, typ: UntypedType)
}

fn handle_atom_list(atoms: List(Atom)) -> List(Node) {
  list.map(atoms, handle_atom)
}

fn handle_atom(atom: Atom) -> Node {
  case atom {
    ValueListAtom(atoms) -> handle_value_list(atoms)
    NumberAtom(num) -> handle_number(num)
    StringAtom(str) -> handle_string(str)
    ListAtom(atoms) -> handle_list(atoms)
    SymbolAtom(name) -> handle_variable(name)
  }
}

pub fn construct(atom: Atom) -> Node {
  let assert ListAtom(atoms) = atom as "Program has to be wrapped in a ListAtom"

  let body = handle_atom_list(atoms)

  ProgramNode(body:)
}
