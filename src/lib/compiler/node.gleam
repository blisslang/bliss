import gleam/list
import pprint

pub type Node {
  ListNode(List(Node))
  ValueListNode(List(Node))
  StringNode(String)
  SymbolNode(String)
  NumberNode(Float)
}

pub fn add_to_node(node: Node, new_node: Node) -> Node {
  case node {
    ListNode(contents) -> ListNode(list.append(contents, [new_node]))
    ValueListNode(contents) -> ValueListNode(list.append(contents, [new_node]))
    _ ->
      panic as {
        "Expected one of: (ListNode ValueListNode), got: "
        <> pprint.styled(new_node)
      }
  }
}

pub fn is_list_node(node: Node) {
  case node {
    ListNode(_) -> True
    _ -> False
  }
}

pub fn is_value_list_node(node: Node) {
  case node {
    ValueListNode(_) -> True
    _ -> False
  }
}

pub fn is_string_node(node: Node) {
  case node {
    StringNode(_) -> True
    _ -> False
  }
}

pub fn is_symbol_node(node: Node) {
  case node {
    SymbolNode(_) -> True
    _ -> False
  }
}

pub fn is_number_node(node: Node) {
  case node {
    NumberNode(_) -> True
    _ -> False
  }
}
