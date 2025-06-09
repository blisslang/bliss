import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import lib/compiler/mod.{
  type Atom, ListAtom, NumberAtom, StringAtom, SymbolAtom, ValueListAtom,
}
import lib/utils

type State {
  State(tokens: List(String), stack: List(Atom), current: Atom)
}

fn to_atom(_state, token token: String) -> Atom {
  let try_to_float = fn() { float.parse(token) |> option.from_result() }

  let try_to_int_as_float = fn() {
    int.parse(token) |> option.from_result() |> option.map(int.to_float)
  }

  case option.or(try_to_float(), try_to_int_as_float()) {
    Some(number) -> NumberAtom(number)
    None -> SymbolAtom(token)
  }
}

fn categorize_string(state: State) -> State {
  case state.tokens {
    [content, "\"", ..new_tokens] -> {
      let new_current = mod.add_to_atom(state.current, StringAtom(content))

      State(..state, tokens: new_tokens, current: new_current)
    }
    _ -> panic as "Unexpected atom in string"
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
      let new_current = mod.add_to_atom(new_current, state.current)

      State(..state, stack: new_stack, current: new_current)
    }
    None -> panic as { "Unexpected atom in closing: " <> str_rep }
  }
}

fn categorize_opening(state: State, new_current new_current: Atom) -> State {
  let new_stack = list.append(state.stack, [state.current])

  State(..state, stack: new_stack, current: new_current)
}

fn categorize_else(state: State, token token: String) -> State {
  let new_current = mod.add_to_atom(state.current, to_atom(state, token))

  State(..state, current: new_current)
}

fn categorize_token(state: State, token token: String) -> State {
  case token {
    "\"" -> categorize_string(state)
    ";" -> categorize_comment(state)
    ")" -> categorize_closing(state, str_rep: ")")
    "]" -> categorize_closing(state, str_rep: "]")
    "(" -> categorize_opening(state, new_current: ListAtom([]))
    "[" -> categorize_opening(state, new_current: ValueListAtom([]))
    token -> categorize_else(state, token)
  }
}

fn do_categorize(state: State) -> Atom {
  case state.tokens {
    [] -> state.current
    [token, ..rest] -> {
      State(..state, tokens: rest)
      |> categorize_token(token:)
      |> do_categorize()
    }
  }
}

pub fn categorize(tokens: List(String)) -> Atom {
  let initial_state = State(tokens:, stack: [], current: ListAtom([]))

  do_categorize(initial_state)
}
