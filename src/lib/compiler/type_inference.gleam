//// This is an implementation of Algorithm J, a type of Hindley-Milner type inference algorithm.
////
//// The algorithm itself uses most of the names from the above link, with
//// a few changed for ease of typing:
////   - Γ (gamma) => env
////   - ⊢ⱼ (perpendicular symbol with j subscript, a.k.a. algorithm J) => infer
////   - Γ¯ (gamma bar) => generalize
////
//// - Technical reference: [Wikipedia article](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J)

import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import lib/compiler/mod.{type Node}
import lib/utils

type TypeName {
  UnitT
  NumberT
  StringT
  BooleanT
}

type Type {
  TConstant(name: TypeName)
  TVariable(name: String)
  TArrow(arg: Type, ret: Type)
  TRecord(fields: List(#(String, Type)))
  TList(items: Type)
}

type Expr {
  UnitExpr
  NumberExpr(value: Float)
  StringExpr(value: String)
  BooleanExpr(value: Bool)
  VariableExpr(name: String)
  FunctionCallExpr(e0: Expr, e1: Expr)
  LambdaFunctionExpr(var: String, body: Expr)
  LetBindingExpr(var: String, e0: Expr, e1: Expr)
  RecordExpr(fields: List(#(String, Expr)))
  FieldExpr(e: Expr, label: String)
  ListExpr(items: List(Expr))
}

type TypedExpr {
  UnitTypedExpr(t: Type)
  NumberTypedExpr(t: Type, value: Float)
  StringTypedExpr(t: Type, value: String)
  BooleanTypedExpr(t: Type, value: Bool)
  VariableTypedExpr(t: Type, name: String)
  FunctionCallTypedExpr(t: Type, f: TypedExpr, arg: TypedExpr)
  LambdaFunctionTypedExpr(t: Type, var: String, body: TypedExpr)
  LetBindingTypedExpr(t: Type, var: String, value: TypedExpr, body: TypedExpr)
  RecordTypedExpr(t: Type, fields: List(#(String, TypedExpr)))
  FieldTypedExpr(t: Type, record: TypedExpr, label: String)
  ListTypedExpr(t: Type, items: List(TypedExpr))
}

type Scheme {
  Scheme(vars: List(String), t: Type)
}

type State {
  State(uf: Dict(String, Type), fresh_counter: Int)
}

fn initial_state() -> State {
  State(uf: dict.new(), fresh_counter: 0)
}

/// Look up a type variable in the current substitution until you reach a final type.
/// Think of it like following pointers until you find the real type.
fn find(t: Type, state: State) -> Type {
  case t {
    TVariable(name:) -> {
      case dict.get(state.uf, name) {
        Ok(uf_t) -> find(uf_t, state)
        Error(_) -> t
      }
    }
    _ -> t
  }
}

/// Check if a type variable appears inside another type.
/// This prevents definitions like `a = a -> b`, which would loop forever.
fn occurs(var: String, t: Type, state: State) -> Bool {
  let t_rep = find(t, state)

  case t_rep {
    TVariable(name:) -> var == name
    TArrow(arg:, ret:) -> occurs(var, arg, state) || occurs(var, ret, state)
    TRecord(fields:) ->
      dict.from_list(fields)
      |> dict.fold(False, fn(acc, _, field_t) {
        acc || occurs(var, field_t, state)
      })
    TList(items:) -> occurs(var, items, state)
    _ -> False
  }
}

/// Make two types agree by updating the substitution.
/// If one side is a variable, bind it; if both are compound, recurse into parts.
fn unify(t1: Type, t2: Type, state: State) -> State {
  let t1_rep = find(t1, state)
  let t2_rep = find(t2, state)

  case t1_rep == t2_rep {
    True -> state
    False -> {
      case t1_rep, t2_rep {
        TVariable(name: t1_name), _ -> {
          case occurs(t1_name, t2_rep, state) {
            True -> panic as "Occurs check failed"
            False -> State(..state, uf: dict.insert(state.uf, t1_name, t2_rep))
          }
        }
        _, TVariable(name: t2_name) -> {
          case occurs(t2_name, t1_rep, state) {
            True -> panic as "Occurs check failed"
            False -> State(..state, uf: dict.insert(state.uf, t2_name, t1_rep))
          }
        }
        TArrow(arg: t1_arg, ret: t1_ret), TArrow(arg: t2_arg, ret: t2_ret) -> {
          let state1 = unify(t1_arg, t2_arg, state)
          let state2 = unify(t1_ret, t2_ret, state1)
          state2
        }
        TConstant(name: t1_name), TConstant(name: t2_name)
          if t1_name == t2_name
        -> {
          state
        }
        TRecord(fields: t1_fields), TRecord(fields: t2_fields) -> {
          let t1_dict = dict.from_list(t1_fields)
          let t2_dict = dict.from_list(t2_fields)

          let common_keys =
            set.intersection(
              set.from_list(dict.keys(t1_dict)),
              set.from_list(dict.keys(t2_dict)),
            )
          let state_after_common =
            set.fold(common_keys, state, fn(acc, key) {
              let assert Ok(t1_field) = dict.get(t1_dict, key)
              let assert Ok(t2_field) = dict.get(t2_dict, key)
              unify(t1_field, t2_field, acc)
            })
          state_after_common
        }
        TList(items: t1_items), TList(items: t2_items) -> {
          unify(t1_items, t2_items, state)
        }
        _, _ -> {
          io.println_error(utils.format_error(
            message: "Mismatched types",
            hint: "Cannot use type "
              <> format_resolved_type(t2_rep, 0, state)
              <> " where type "
              <> format_resolved_type(t1_rep, 0, state)
              <> " was expected.",
          ))
          panic
        }
      }
    }
  }
}

/// Generate a brand-new type variable (a0, a1, a2...), so fresh names don’t clash.
fn fresh_var(state: State) -> #(Type, State) {
  let name = "a" <> int.to_string(state.fresh_counter)
  let new_state = State(..state, fresh_counter: state.fresh_counter + 1)

  #(TVariable(name:), new_state)
}

/// Replace any variables in a type according to the given substitution map.
/// "Subst" is just a dictionary from variable name to specific type.
fn apply_subst(t: Type, subst: Dict(String, Type)) -> Type {
  case t {
    TVariable(name:) -> dict.get(subst, name) |> result.unwrap(or: t)
    TConstant(name: _) -> t
    TArrow(arg:, ret:) ->
      TArrow(arg: apply_subst(arg, subst), ret: apply_subst(ret, subst))
    TRecord(fields:) -> {
      let new_fields =
        list.map(fields, fn(field) {
          let #(label, field_t) = field
          #(label, apply_subst(field_t, subst))
        })

      TRecord(fields: new_fields)
    }
    TList(items:) -> TList(items: apply_subst(items, subst))
  }
}

/// Turn a polymorphic type (scheme) into a concrete type by replacing each
/// quantified variable with a fresh one.
fn inst(scheme: Scheme, state: State) -> #(Type, State) {
  let #(subst, new_state) =
    list.fold(scheme.vars, #(dict.new(), state), fn(acc, var) {
      let #(subst, new_state) = acc

      let #(fresh_tvar, new_new_state) = fresh_var(new_state)
      let new_subst = dict.insert(subst, var, fresh_tvar)

      #(new_subst, new_new_state)
    })

  let instantiated_t = apply_subst(scheme.t, subst)

  #(instantiated_t, new_state)
}

/// Find every type variable not yet fixed inside a single type.
/// These are the "free" variables you might later quantify.
fn free_vars_type(t: Type, state: State) -> Set(String) {
  let t_rep = find(t, state)

  case t_rep {
    TVariable(name:) -> set.from_list([name])
    TConstant(name: _) -> set.new()
    TArrow(arg:, ret:) ->
      set.union(free_vars_type(arg, state), free_vars_type(ret, state))
    TRecord(fields:) ->
      dict.from_list(fields)
      |> dict.values()
      |> list.fold(set.new(), fn(acc, field_t) {
        set.union(acc, free_vars_type(field_t, state))
      })
    TList(items:) -> free_vars_type(items, state)
  }
}

/// For a scheme (polymorphic type), collect its free vars (minus the ones it binds).
fn free_vars_scheme(scheme: Scheme, state: State) -> Set(String) {
  set.difference(free_vars_type(scheme.t, state), set.from_list(scheme.vars))
}

/// Gather all free variables from every type in the environment.
fn free_vars_context(env: Dict(String, Scheme), state: State) -> Set(String) {
  list.fold(dict.values(env), set.new(), fn(acc, scheme) {
    set.union(acc, free_vars_scheme(scheme, state))
  })
}

/// Create a polymorphic type (scheme) by quantifying every free variable
/// in `t` that isn’t already fixed by the environment.
fn generalize(env: Dict(String, Scheme), t: Type, state: State) -> Scheme {
  let fv_t = free_vars_type(t, state)
  let fv_env = free_vars_context(env, state)

  let quantified_vars = set.to_list(set.difference(fv_t, fv_env))

  Scheme(vars: quantified_vars, t: t)
}

/// Recursively chase all variables to their final bindings,
/// then pretty-print the resulting type.
fn format_resolved_type(t: Type, level: Int, state: State) -> String {
  let t_rep = find(t, state)

  let pretty_t = case t_rep {
    TVariable(name:) -> name
    TConstant(name:) ->
      case name {
        StringT -> "String"
        NumberT -> "Number"
        UnitT -> "Unit"
        BooleanT -> "Boolean"
      }
    TArrow(arg:, ret:) -> {
      let pretty_arg = format_resolved_type(arg, level + 1, state)
      let pretty_ret = format_resolved_type(ret, level + 1, state)

      "(" <> pretty_arg <> " -> " <> pretty_ret <> ")"
    }
    TRecord(fields:) -> {
      let format_field = fn(field) {
        let #(label, field_t) = field
        label <> ":" <> format_resolved_type(field_t, level + 1, state)
      }

      let parts =
        list.map(fields, format_field)
        |> string.join(",")

      "Record::" <> parts
    }
    TList(items:) -> "List::" <> format_resolved_type(items, level + 1, state)
  }

  case level {
    0 -> "`" <> pretty_t <> "`"
    _ -> pretty_t
  }
}

/// The core of Algorithm J: walk the expression tree,
/// infer types for sub-expressions, unify constraints, and return a typed AST.
fn infer_j(
  env: Dict(String, Scheme),
  e: Expr,
  state: State,
) -> #(TypedExpr, Type, State) {
  case e {
    UnitExpr -> {
      let t = TConstant(UnitT)

      let te = UnitTypedExpr(t:)

      #(te, t, state)
    }
    NumberExpr(value:) -> {
      let t = TConstant(NumberT)

      let te = NumberTypedExpr(t:, value:)

      #(te, t, state)
    }
    StringExpr(value:) -> {
      let t = TConstant(StringT)

      let te = StringTypedExpr(t:, value:)

      #(te, t, state)
    }
    BooleanExpr(value:) -> {
      let t = TConstant(BooleanT)

      let te = BooleanTypedExpr(t:, value:)

      #(te, t, state)
    }
    VariableExpr(name:) -> {
      case dict.get(env, name) {
        Error(_) -> {
          io.println_error(utils.format_error(
            message: "Unbound variable",
            hint: "No variable with name `" <> name <> "` was found.",
          ))
          panic
        }
        Ok(scheme) -> {
          let #(t, new_state) = inst(scheme, state)

          let te = VariableTypedExpr(t:, name:)

          #(te, t, new_state)
        }
      }
    }
    FunctionCallExpr(e0: fn_e, e1: arg_e) -> {
      let #(fn_te, fn_t, state1) = infer_j(env, fn_e, state)
      let #(arg_te, arg_t, state2) = infer_j(env, arg_e, state1)
      let #(res_t, state3) = fresh_var(state2)

      let arrow = TArrow(arg_t, res_t)
      let state4 = unify(fn_t, arrow, state3)

      let te = FunctionCallTypedExpr(t: res_t, f: fn_te, arg: arg_te)

      #(te, res_t, state4)
    }
    LambdaFunctionExpr(var:, body:) -> {
      let #(param_t, state1) = fresh_var(state)

      let env1 = dict.insert(env, var, Scheme(vars: [], t: param_t))
      let #(body_te, body_t, state2) = infer_j(env1, body, state1)

      let lam_t = TArrow(arg: param_t, ret: body_t)

      let te = LambdaFunctionTypedExpr(t: lam_t, var:, body: body_te)

      #(te, lam_t, state2)
    }
    LetBindingExpr(var:, e0: value, e1: body) -> {
      let #(new_var_t, state1) = fresh_var(state)

      let env1 = dict.insert(env, var, Scheme(vars: [], t: new_var_t))

      let #(value_te, value_t, state2) = infer_j(env1, value, state1)

      let state3 = unify(new_var_t, value_t, state2)
      let gen = generalize(env, value_t, state3)

      let env2 = dict.insert(env, var, gen)
      let #(body_te, body_t, state4) = infer_j(env2, body, state3)

      let te =
        LetBindingTypedExpr(t: body_t, var: var, value: value_te, body: body_te)

      #(te, body_t, state4)
    }
    RecordExpr(fields:) -> {
      let #(fields_te, types, state_current) =
        list.fold(fields, #([], dict.new(), state), fn(acc, field) {
          let #(fields_te, types, state_current) = acc
          let #(label, expr) = field

          let #(expr_te, expr_t, new_state_current) =
            infer_j(env, expr, state_current)

          #(
            [#(label, expr_te), ..fields_te],
            dict.insert(types, label, expr_t),
            new_state_current,
          )
        })

      let record_t = TRecord(fields: dict.to_list(types))

      let te = RecordTypedExpr(t: record_t, fields: fields_te)

      #(te, record_t, state_current)
    }
    FieldExpr(e: value, label:) -> {
      let #(value_te, value_t, state1) = infer_j(env, value, state)
      let #(field_t, state2) = fresh_var(state1)

      let fields_t = [#(label, field_t)]
      let fields_te = [#(label, value_te)]

      let record_t = TRecord(fields: fields_t)
      let state3 = unify(value_t, record_t, state2)
      let record_te = RecordTypedExpr(t: record_t, fields: fields_te)

      let te = FieldTypedExpr(t: record_t, record: record_te, label:)

      #(te, field_t, state3)
    }
    ListExpr(items:) -> {
      let #(elem_t, state1) = fresh_var(state)

      let #(items_te, state2) =
        list.fold(items, #([], state1), fn(acc, item) {
          let #(items, current_state) = acc

          let #(item_te, item_t, after_infer_state) =
            infer_j(env, item, current_state)
          let unify_state = unify(item_t, elem_t, after_infer_state)

          #([item_te, ..items], unify_state)
        })

      let list_t = TList(items: elem_t)

      let te = ListTypedExpr(t: list_t, items: items_te)

      #(te, list_t, state2)
    }
  }
}

pub fn infer(ast: Node) -> Node {
  let context =
    dict.from_list([
      #(
        "if",
        Scheme(
          vars: ["t0"],
          t: TArrow(
            arg: TConstant(BooleanT),
            ret: TArrow(
              arg: TVariable("t0"),
              ret: TArrow(arg: TVariable("t0"), ret: TVariable("t0")),
            ),
          ),
        ),
      ),
      #(
        "<=",
        Scheme(
          vars: [],
          t: TArrow(
            arg: TConstant(NumberT),
            ret: TArrow(arg: TConstant(NumberT), ret: TConstant(BooleanT)),
          ),
        ),
      ),
      #(
        "+",
        Scheme(
          vars: ["t0"],
          t: TArrow(
            arg: TVariable("t0"),
            ret: TArrow(arg: TVariable("t0"), ret: TVariable("t0")),
          ),
        ),
      ),
      #(
        "-",
        Scheme(
          vars: ["t0"],
          t: TArrow(
            arg: TVariable("t0"),
            ret: TArrow(arg: TVariable("t0"), ret: TVariable("t0")),
          ),
        ),
      ),
      #(
        "print",
        Scheme(
          vars: ["t0"],
          t: TArrow(arg: TVariable("t0"), ret: TConstant(UnitT)),
        ),
      ),
    ])

  let expr =
    LetBindingExpr(
      "fib",
      LambdaFunctionExpr(
        "n",
        FunctionCallExpr(
          FunctionCallExpr(
            FunctionCallExpr(
              VariableExpr("if"),
              FunctionCallExpr(
                FunctionCallExpr(VariableExpr("<="), VariableExpr("n")),
                NumberExpr(1.0),
              ),
            ),
            VariableExpr("n"),
          ),
          FunctionCallExpr(
            FunctionCallExpr(
              VariableExpr("+"),
              FunctionCallExpr(
                VariableExpr("fib"),
                FunctionCallExpr(
                  FunctionCallExpr(VariableExpr("-"), VariableExpr("n")),
                  NumberExpr(1.0),
                ),
              ),
            ),
            FunctionCallExpr(
              VariableExpr("fib"),
              FunctionCallExpr(
                FunctionCallExpr(VariableExpr("-"), VariableExpr("n")),
                NumberExpr(1.0),
              ),
            ),
          ),
        ),
      ),
      LetBindingExpr(
        "fib-res",
        FunctionCallExpr(VariableExpr("fib"), NumberExpr(8.0)),
        FunctionCallExpr(VariableExpr("print"), VariableExpr("fib-res")),
      ),
    )

  "
  (let fib (fn [n]
    (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))
  (let fib-res (fib 8))
  (print fib-res)
  "

  let #(te_result, type_result, final_state) =
    infer_j(context, expr, initial_state())

  io.println("\nTE RESULT:\n" <> utils.styled(te_result))
  io.println(
    "\nTYPE RESULT:\n" <> format_resolved_type(type_result, 0, final_state),
  )
  io.println("\nFINAL STATE:\n" <> utils.styled(final_state))

  ast
}
