import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import lib/compiler/node.{
  type Node, ListNode, NumberNode, StringNode, SymbolNode, ValueListNode,
}
import lib/utils

type State {
  State(tokens: List(String), stack: List(Node), current: Node)
}

fn to_node(_state, token token: String) -> Node {
  let try_to_float = fn() { float.parse(token) |> option.from_result() }

  let try_to_int_as_float = fn() {
    int.parse(token) |> option.from_result() |> option.map(int.to_float)
  }

  case option.or(try_to_float(), try_to_int_as_float()) {
    Some(number) -> NumberNode(number)
    None -> SymbolNode(token)
  }
}

fn categorize_string(state: State) -> State {
  case state.tokens {
    [content, "\"", ..new_tokens] -> {
      let new_current = node.add_to_node(state.current, StringNode(content))

      State(..state, tokens: new_tokens, current: new_current)
    }
    _ -> panic as "Unexpected node in string"
  }
}

fn categorize_comment(state: State) -> State {
  let new_tokens =
    state.tokens
    |> list.drop_while(fn(c) { c != ";" })
    |> list.drop(1)

  State(..state, tokens: new_tokens)
}

fn categorize_closing(state: State, str_rep str_rep: String) -> State {
  case utils.unsnoc(state.stack) {
    Some(#(new_stack, new_current)) -> {
      let new_current = node.add_to_node(new_current, state.current)

      State(..state, stack: new_stack, current: new_current)
    }
    None -> panic as { "Unexpected node in closing: " <> str_rep }
  }
}

fn categorize_opening(state: State, new_current new_current: Node) -> State {
  let new_stack = list.append(state.stack, [state.current])

  State(..state, stack: new_stack, current: new_current)
}

fn categorize_else(state: State, token token: String) -> State {
  let new_current = node.add_to_node(state.current, to_node(state, token))

  State(..state, current: new_current)
}

fn categorize_token(state: State, token token: String) -> State {
  case token {
    "\"" -> categorize_string(state)
    ";" -> categorize_comment(state)
    ")" -> categorize_closing(state, str_rep: ")")
    "]" -> categorize_closing(state, str_rep: "]")
    "(" -> categorize_opening(state, new_current: ListNode([]))
    "[" -> categorize_opening(state, new_current: ValueListNode([]))
    token -> categorize_else(state, token)
  }
}

fn do_categorize(state: State) -> Node {
  case state.tokens {
    [] -> state.current
    [token, ..rest] -> {
      State(..state, tokens: rest)
      |> categorize_token(token:)
      |> do_categorize()
    }
  }
}

pub fn categorize(tokens: List(String)) -> Node {
  let initial_state = State(tokens:, stack: [], current: ListNode([]))

  do_categorize(initial_state)
}
