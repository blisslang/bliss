import gleam/list
import gleam/string
import lib/utils

fn pad_delims(str: String) -> String {
  str
  |> string.split(on: "")
  |> list.flat_map(fn(c) {
    case utils.is_delim(c) {
      True -> [" ", c, " "]
      False -> [c]
    }
  })
  |> string.join(with: "")
}

fn tokenize_word(rest: String, acc: List(List(String))) -> List(List(String)) {
  case list.drop_while(string.split(rest, on: ""), utils.is_space) {
    [] -> list.reverse(acc)
    // start of string
    ["\"", ..tail] as new_rest ->
      case list.split_while(tail, fn(c) { c != "\"" }) {
        #(_, []) -> list.reverse([new_rest, ..acc])
        #(content, ["\"", ..after_quote]) ->
          tokenize_word(string.join(after_quote, with: ""), [
            ["\""],
            content,
            ["\""],
            ..acc
          ])
        _ -> panic as "Unreachable"
      }
    // regular code
    new_rest ->
      case list.split_while(new_rest, fn(c) { !utils.is_space(c) }) {
        #(word, []) -> list.reverse([word, ..acc])
        #(word, after_space) ->
          tokenize_word(string.join(after_space, with: ""), [word, ..acc])
      }
  }
}

fn flatten_inner(nested_tokens: List(List(String))) -> List(String) {
  list.map(nested_tokens, with: fn(word_chars) {
    string.join(word_chars, with: "")
  })
}

pub fn tokenize(code: String) -> List(String) {
  code
  |> pad_delims()
  |> tokenize_word([])
  |> flatten_inner()
}
