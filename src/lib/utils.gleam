import gleam/list
import gleam/option.{type Option, None, Some}
import pprint

const delims = ["[", "]", "(", ")", ";"]

pub fn is_delim(c: String) -> Bool {
  list.contains(delims, c)
}

const whitespace = [" ", "\n", "\t"]

pub fn is_space(c: String) -> Bool {
  list.contains(whitespace, c)
}

pub fn unsnoc(list: List(a)) -> Option(#(List(a), a)) {
  case list.reverse(list) {
    [] -> None
    [last, ..rest] -> Some(#(list.reverse(rest), last))
  }
}

/// Converts the given option to a list.
///
/// ## Examples
///
/// ```gleam
/// option_to_list(Some(1))
/// // -> [1]
///
/// option_to_list(None)
/// // -> []
/// ```
pub fn option_to_list(option: Option(a)) -> List(a) {
  case option {
    Some(el) -> [el]
    None -> []
  }
}

/// Runs a function repeatedly until the result has not changed between the last run.
pub fn fixed_point(f: fn(a) -> a, x: a) -> a {
  let next_x = f(x)

  case next_x == x {
    True -> x
    False -> fixed_point(f, next_x)
  }
}

/// Gets the index of an element in a list.
///
/// Stops iterating over the list when the element is found.
///
/// ## Examples
///
/// ```gleam
/// list_index_of(1, in: [3, 2, 1])
/// // -> Some(2)
///
/// list_index_of("Yes", in: ["No", "Maybe"])
/// // -> None
/// ```
pub fn list_index_of(el: a, in lst: List(a)) -> Option(Int) {
  do_index_of(el, lst, 0)
}

fn do_index_of(el: a, lst: List(a), index: Int) -> Option(Int) {
  case lst {
    [] -> None
    [head, ..] if head == el -> Some(index)
    [_, ..tail] -> do_index_of(el, tail, index + 1)
  }
}

pub fn pprint(x: a) {
  pprint.with_config(
    x,
    pprint.Config(
      style_mode: pprint.Styled,
      bit_array_mode: pprint.KeepBitArrays,
      label_mode: pprint.Labels,
    ),
  )
}
