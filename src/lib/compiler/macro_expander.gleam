import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import lib/compiler/mod.{
  type Atom, ListAtom, NumberAtom, StringAtom, SymbolAtom, ValueListAtom,
}
import lib/utils

type RegistrationState {
  RegistrationState(macros: Dict(String, MacroDef), other_atoms: List(Atom))
}

type MacroDef {
  MacroDef(params: List(String), body: List(Atom))
}

type Visitor =
  fn(Atom) -> Atom

fn walk_atom(atom: Atom, visitor: Visitor) -> Atom {
  let new_atom = case atom {
    ListAtom(children) ->
      ListAtom(list.map(children, fn(a) { walk_atom(a, visitor) }))
    ValueListAtom(children) ->
      ValueListAtom(list.map(children, fn(a) { walk_atom(a, visitor) }))
    // leaf atoms: these have no children to walk_atom.
    StringAtom(..) | NumberAtom(..) | SymbolAtom(..) -> atom
  }

  visitor(new_atom)
}

fn do_register_macro_defs(
  atoms: List(Atom),
  state: RegistrationState,
) -> RegistrationState {
  list.fold(over: atoms, from: state, with: fn(acc, atom) {
    case atom {
      // (defmacro name [param1 ...] body...)
      ListAtom([
        SymbolAtom("defmacro"),
        SymbolAtom(name),
        ValueListAtom(params),
        ..body
      ]) -> {
        case list.all(params, mod.is_symbol_atom) {
          False -> panic as { "Invalid macro args: " <> utils.styled(params) }
          True -> {
            let param_names =
              list.map(params, fn(p) {
                let assert SymbolAtom(name) = p
                name
              })

            let new_macro_def = MacroDef(params: param_names, body:)

            let new_macros = dict.insert(acc.macros, name, new_macro_def)

            RegistrationState(..acc, macros: new_macros)
          }
        }
      }

      // any other atom is kept as is
      _ -> {
        let new_atoms = [atom, ..acc.other_atoms]

        RegistrationState(..acc, other_atoms: new_atoms)
      }
    }
  })
}

fn register_macro_defs(
  atoms: List(Atom),
) -> #(Dict(String, MacroDef), List(Atom)) {
  let initial_state = RegistrationState(macros: dict.new(), other_atoms: [])

  let final_state = do_register_macro_defs(atoms, initial_state)

  #(final_state.macros, list.reverse(final_state.other_atoms))
}

fn substitute_arg(atom: Atom, substitution_args: Dict(String, Atom)) -> Atom {
  let visitor = fn(atom) {
    case atom {
      // variadic arg
      SymbolAtom("&" <> _ as name) -> {
        let #(replacement_args, macro_args) =
          substitution_args |> dict.to_list() |> list.unzip()

        let maybe_replacement_idx =
          utils.list_index_of(name, in: replacement_args)

        case maybe_replacement_idx {
          None -> atom
          Some(replacement_idx) -> {
            let #(_, rest_args) = list.split(macro_args, at: replacement_idx)

            // wrap in a do block just in case
            ListAtom([SymbolAtom("do"), ..rest_args])
          }
        }
      }
      // regular arg
      SymbolAtom(name) ->
        dict.get(substitution_args, name)
        |> result.unwrap(or: atom)
      // anything else, dont touch it
      _ -> atom
    }
  }

  walk_atom(atom, visitor)
}

fn create_substitution_args(
  macro_def_params: List(String),
  args: List(Atom),
) -> Dict(String, Atom) {
  let variadic_args_amount =
    macro_def_params
    |> list.filter(fn(param) { string.starts_with(param, "&") })
    |> list.length()

  case variadic_args_amount {
    // variadic args
    1 -> {
      case list.last(macro_def_params) {
        Ok("&" <> _) -> {
          let variadic_arg_idx = list.length(macro_def_params) - 1

          let assert #(regular_macro_def_params, [variadic_macro_def_param]) =
            list.split(macro_def_params, variadic_arg_idx)

          let #(regular_args, variadic_args) =
            list.split(args, variadic_arg_idx)

          // add regular args
          list.zip(regular_macro_def_params, regular_args)
          |> dict.from_list()
          // add the variadic arg wrapped in a do block just in case
          |> dict.insert(
            variadic_macro_def_param,
            ListAtom([SymbolAtom("do"), ..variadic_args]),
          )
        }
        _ -> panic as "Variadic macro argument has to be in the last position"
      }
    }
    // regular args
    0 -> {
      macro_def_params
      |> list.zip(args)
      |> dict.from_list()
    }
    _ -> panic as "Macros can only have one vardiadic argument"
  }
}

pub fn expand(atom: Atom) -> Atom {
  let assert ListAtom(top_level_atoms) = atom
    as "Program has to be wrapped in a ListAtom"

  let #(macros, atoms_without_defs) = register_macro_defs(top_level_atoms)

  let visitor = fn(atom) {
    case atom {
      // check if a list is a macro call: (macro-name arg1 arg2)
      ListAtom([SymbolAtom(name), ..args]) -> {
        case dict.get(macros, name) {
          Error(_) -> atom
          Ok(macro_def) -> {
            let substitution_args =
              create_substitution_args(macro_def.params, args)

            let expanded_body =
              list.map(macro_def.body, fn(body_atom) {
                substitute_arg(body_atom, substitution_args)
              })

            // wrap in a do block just in case
            ListAtom([SymbolAtom("do"), ..expanded_body])
          }
        }
      }
      // not a macro call, do nothing
      _ -> atom
    }
  }

  // if there are no macros, we can skip the expansion pass
  case dict.is_empty(macros) {
    True -> atom
    False -> {
      let expand_pass = fn(current_atoms) {
        list.map(current_atoms, fn(a) { walk_atom(a, visitor) })
      }

      let expanded_atoms = utils.fixed_point(expand_pass, atoms_without_defs)

      ListAtom(expanded_atoms)
    }
  }
}
