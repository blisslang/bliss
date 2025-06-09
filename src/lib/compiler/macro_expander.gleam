import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import lib/compiler/mod.{type Atom, ListAtom, SymbolAtom, ValueListAtom}
import lib/utils

type MacroDef {
  MacroDef(replacement_args: List(String), macro_nodes: List(Atom))
}

fn process_macro_node(node: Atom) -> #(Dict(String, MacroDef), Option(Atom)) {
  case node {
    // (defmacro name [...] ...)
    ListAtom([
      SymbolAtom("defmacro"),
      SymbolAtom(name),
      ValueListAtom(args),
      ..body_nodes
    ]) -> {
      case !list.is_empty(args) && list.all(args, mod.is_symbol_atom) {
        // invalid macro args
        False ->
          panic as {
            "Invalid macro definition arguments: " <> utils.pprint(args)
          }
        // valid macro args
        True -> {
          let str_args =
            list.map(args, fn(x) {
              case x {
                SymbolAtom(s) -> s
                _ -> panic as "Unreachable"
              }
            })

          let new_macro_def =
            dict.from_list([
              #(
                name,
                MacroDef(replacement_args: str_args, macro_nodes: body_nodes),
              ),
            ])

          #(new_macro_def, None)
        }
      }
    }
    // invalid macro
    ListAtom([SymbolAtom("defmacro"), ..]) ->
      panic as { "Invalid macro definition: " <> utils.pprint(node) }
    // continue downwards to process children
    ListAtom(child_nodes) -> {
      let #(macros_from_children, remaining_children) =
        register_macro_defs(child_nodes)

      #(macros_from_children, Some(ListAtom(remaining_children)))
    }
    // anything else
    node -> #(dict.new(), Some(node))
  }
}

fn register_macro_defs(
  atom_tree_nodes: List(Atom),
) -> #(Dict(String, MacroDef), List(Atom)) {
  case atom_tree_nodes {
    [] -> #(dict.new(), [])
    [node, ..rest] -> {
      let #(macros_from_node, maybe_node) = process_macro_node(node)
      let #(macros_from_rest, nodes_from_rest) = register_macro_defs(rest)

      #(
        dict.merge(macros_from_node, macros_from_rest),
        list.append(utils.option_to_list(maybe_node), nodes_from_rest),
      )
    }
  }
}

fn expand_macro_arg(
  macro_def: MacroDef,
  macro_args: List(Atom),
  in arg_node: Atom,
) -> Atom {
  case arg_node {
    // found variadic arg, expand it
    SymbolAtom("&" <> _ as arg_name) as node -> {
      let maybe_replacement_idx =
        utils.list_index_of(arg_name, in: macro_def.replacement_args)

      case maybe_replacement_idx {
        None -> node

        Some(replacement_idx) -> {
          let #(_, replacement_nodes) =
            list.split(macro_args, at: replacement_idx)

          // wrapped with (do ...) just in case
          ListAtom([SymbolAtom("do"), ..replacement_nodes])
        }
      }
    }
    // found arg, expand it
    SymbolAtom(arg_name) as node -> {
      let maybe_replacement_idx =
        utils.list_index_of(arg_name, in: macro_def.replacement_args)

      case maybe_replacement_idx {
        None -> node

        Some(replacement_idx) -> {
          let replacement_node =
            list.split(macro_args, at: replacement_idx)
            |> pair.second()
            |> list.first()
            |> result.unwrap(or: SymbolAtom("Unreachable"))

          replacement_node
        }
      }
    }
    // continue deeper to find more args to expand
    ListAtom(exprs) ->
      ListAtom(list.map(exprs, expand_macro_arg(macro_def, macro_args, in: _)))
    ValueListAtom(exprs) ->
      ValueListAtom(
        list.map(exprs, expand_macro_arg(macro_def, macro_args, in: _)),
      )
    // anything else, dont do anything
    node -> node
  }
}

fn process_macro_usage_node(
  macro_defs: Dict(String, MacroDef),
  in atom_tree_node: Atom,
) -> Atom {
  case atom_tree_node {
    ListAtom([SymbolAtom(name), ..args] as exprs) -> {
      case dict.get(macro_defs, name) {
        // found a macro usage, expand it node by node
        Ok(macro_def) -> {
          // wrapped with (do ...) just in case
          let new_node = ListAtom([SymbolAtom("do"), ..macro_def.macro_nodes])
          let expanded_node = expand_macro_arg(macro_def, args, in: new_node)

          let defined_args = macro_def.replacement_args
          let num_defined_args = list.length(defined_args)
          let num_provided_args = list.length(args)

          let is_variadic =
            defined_args
            |> list.last()
            |> result.unwrap(or: "")
            |> string.starts_with("&")

          case True {
            _
              if is_variadic
              && num_provided_args >= num_defined_args
              || !is_variadic
              && num_provided_args == num_defined_args
            -> {
              expanded_node
            }
            _ ->
              panic as {
                "Invalid macro usage: " <> name <> " " <> utils.pprint(args)
              }
          }
        }
        // false alarm, continue deeper
        Error(_) ->
          ListAtom(list.map(exprs, process_macro_usage_node(macro_defs, in: _)))
      }
    }
    // go deeper to find more macro usages
    ListAtom(exprs) ->
      ListAtom(list.map(exprs, process_macro_usage_node(macro_defs, in: _)))
    ValueListAtom(exprs) ->
      ValueListAtom(
        list.map(exprs, process_macro_usage_node(macro_defs, in: _)),
      )
    // anything else, do thing
    node -> node
  }
}

fn expand_macro_usages(
  macro_defs: Dict(String, MacroDef),
  rest_atom_tree_nodes: List(Atom),
) -> List(Atom) {
  let expand_pass = fn(current_atom_tree_nodes) {
    list.map(current_atom_tree_nodes, process_macro_usage_node(
      macro_defs,
      in: _,
    ))
  }

  utils.fixed_point(expand_pass, rest_atom_tree_nodes)
}

pub fn expand(atom: Atom) -> Atom {
  let assert ListAtom(atoms) = atom as "Program has to be wrapped in a ListAtom"

  let #(macro_defs, rest_atom_tree_nodes) = register_macro_defs(atoms)

  io.println("MACRO DEFS:")
  io.println(utils.pprint(macro_defs))
  io.println("REST ATOM TREE NODES:")
  io.println(utils.pprint(rest_atom_tree_nodes))

  let expanded_atom_tree = expand_macro_usages(macro_defs, rest_atom_tree_nodes)

  ListAtom(expanded_atom_tree)
}
