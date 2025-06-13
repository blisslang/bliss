import gleam/list
import lib/utils

pub type Atom {
  ListAtom(List(Atom))
  ValueListAtom(List(Atom))
  StringAtom(String)
  SymbolAtom(String)
  NumberAtom(Float)
}

pub type VariableType {
  FloatType
  StringType
  UntypedType
}

pub type Node {
  ProgramNode(body: List(Node))

  ConditionalNode(branches: List(Node))
  BranchNode(pred: Node, body: List(Node))
  DoBlockNode(body: List(Node))
  LambdaFunctionNode(params: List(Node), body: List(Node))
  LetBindingNode(name: String, value: Node)
  FunctionCallNode(name: String, params: List(Node))
  VariableNode(name: String, typ: VariableType)

  ListNode(items: List(Node))
  StringNode(value: String)
  NumberNode(value: Float)
}

pub fn add_to_atom(atom: Atom, new_atom: Atom) -> Atom {
  case atom {
    ListAtom(contents) -> ListAtom(list.append(contents, [new_atom]))
    ValueListAtom(contents) -> ValueListAtom(list.append(contents, [new_atom]))
    _ ->
      panic as {
        "Expected one of: (ListAtom ValueListAtom), got: "
        <> utils.styled(new_atom)
      }
  }
}

pub fn is_list_atom(atom: Atom) {
  case atom {
    ListAtom(_) -> True
    _ -> False
  }
}

pub fn is_value_list_atom(atom: Atom) {
  case atom {
    ValueListAtom(_) -> True
    _ -> False
  }
}

pub fn is_string_atom(atom: Atom) {
  case atom {
    StringAtom(_) -> True
    _ -> False
  }
}

pub fn is_symbol_atom(atom: Atom) {
  case atom {
    SymbolAtom(_) -> True
    _ -> False
  }
}

pub fn is_number_atom(atom: Atom) {
  case atom {
    NumberAtom(_) -> True
    _ -> False
  }
}
