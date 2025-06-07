import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import lib/compiler/node.{type Node, ListNode, SymbolNode, ValueListNode}
import lib/utils
import pprint

type MacroDef {
  MacroDef(replacement_args: List(String), macro_nodes: List(Node))
}

fn process_macro_node(node: Node) -> #(Dict(String, MacroDef), Option(Node)) {
  case node {
    // (defmacro name [...] ...)
    ListNode([
      SymbolNode("defmacro"),
      SymbolNode(name),
      ValueListNode(args),
      ..body_nodes
    ]) -> {
      case !list.is_empty(args) && list.all(args, node.is_symbol_node) {
        // invalid macro args
        False ->
          panic as {
            "Invalid macro definition arguments: " <> pprint.styled(args)
          }
        // valid macro args
        True -> {
          let str_args =
            list.map(args, fn(x) {
              case x {
                SymbolNode(s) -> s
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
    ListNode([SymbolNode("defmacro"), ..]) ->
      panic as { "Invalid macro definition: " <> pprint.styled(node) }
    // continue downwards to process children
    ListNode(child_nodes) -> {
      let #(macros_from_children, remaining_children) =
        register_macro_defs(child_nodes)

      #(macros_from_children, Some(ListNode(remaining_children)))
    }
    // anything else
    node -> #(dict.new(), Some(node))
  }
}

fn register_macro_defs(
  ast_nodes: List(Node),
) -> #(Dict(String, MacroDef), List(Node)) {
  case ast_nodes {
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
  macro_args: List(Node),
  in arg_node: Node,
) -> Node {
  case arg_node {
    // found variadic arg, expand it
    SymbolNode("&" <> _ as arg_name) as node -> {
      let maybe_replacement_idx =
        utils.list_index_of(arg_name, in: macro_def.replacement_args)

      case maybe_replacement_idx {
        None -> node

        Some(replacement_idx) -> {
          let #(_, replacement_nodes) =
            list.split(macro_args, at: replacement_idx)

          // wrapped with (do ...) just in case
          ListNode([SymbolNode("do"), ..replacement_nodes])
        }
      }
    }
    // found arg, expand it
    SymbolNode(arg_name) as node -> {
      let maybe_replacement_idx =
        utils.list_index_of(arg_name, in: macro_def.replacement_args)

      case maybe_replacement_idx {
        None -> node

        Some(replacement_idx) -> {
          let replacement_node =
            list.split(macro_args, at: replacement_idx)
            |> pair.second()
            |> list.first()
            |> result.unwrap(or: SymbolNode("Unreachable"))

          replacement_node
        }
      }
    }
    // continue deeper to find more args to expand
    ListNode(exprs) ->
      ListNode(list.map(exprs, expand_macro_arg(macro_def, macro_args, in: _)))
    ValueListNode(exprs) ->
      ValueListNode(
        list.map(exprs, expand_macro_arg(macro_def, macro_args, in: _)),
      )
    // anything else, dont do anything
    node -> node
  }
}

fn process_macro_usage_node(
  macro_defs: Dict(String, MacroDef),
  in ast_node: Node,
) -> Node {
  case ast_node {
    ListNode([SymbolNode(name), ..args] as exprs) -> {
      case dict.get(macro_defs, name) {
        // found a macro usage, expand it node by node
        Ok(macro_def) -> {
          // wrapped with (do ...) just in case
          let new_node = ListNode([SymbolNode("do"), ..macro_def.macro_nodes])
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
                "Invalid macro usage: " <> name <> " " <> pprint.styled(args)
              }
          }
        }
        // false alarm, continue deeper
        Error(_) ->
          ListNode(list.map(exprs, process_macro_usage_node(macro_defs, in: _)))
      }
    }
    // go deeper to find more macro usages
    ListNode(exprs) ->
      ListNode(list.map(exprs, process_macro_usage_node(macro_defs, in: _)))
    ValueListNode(exprs) ->
      ValueListNode(
        list.map(exprs, process_macro_usage_node(macro_defs, in: _)),
      )
    // anything else, do thing
    node -> node
  }
}

fn expand_macro_usages(
  macro_defs: Dict(String, MacroDef),
  rest_ast_nodes: List(Node),
) -> List(Node) {
  let expand_pass = fn(current_ast_nodes: List(Node)) {
    list.map(current_ast_nodes, process_macro_usage_node(macro_defs, in: _))
  }

  utils.fixed_point(expand_pass, rest_ast_nodes)
}

pub fn expand(ast_nodes: List(Node)) -> List(Node) {
  let #(macro_defs, rest_ast_nodes) = register_macro_defs(ast_nodes)

  io.println("MACRO DEFS:")
  pprint.debug(macro_defs)
  io.println("REST AST NODES:")
  pprint.debug(rest_ast_nodes)

  let expanded_ast = expand_macro_usages(macro_defs, rest_ast_nodes)

  io.println("EXPANDED AST:")
  pprint.debug(expanded_ast)

  expanded_ast
}
