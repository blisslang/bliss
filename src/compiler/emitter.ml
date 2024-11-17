open Containers
open Types
open Util

let ops =
  Hashtbl.of_list
    [
      ("+", "+.");
      ("-", "-.");
      ("*", "*.");
      ("/", "/.");
      ("<", "<");
      ("<=", "<=");
      (">", ">");
      (">=", ">=");
      ("=", "=");
      ("not=", "<>");
      ("and", "&&");
      ("or", "||");
      ("<>", "^");
    ]

(* -4 -> not started
   -3 -> program list
   -2 -> top-level declaration
   0+ -> inside of top-level declaration *)
let depth = ref (-4)

(* Functions that turn nodes to strings *)
let emit_param = function
  | Symbol value -> value
  | _ ->
      prerr_endline "Params of a def may only contain symbols";
      exit 1

let emit_params name params =
  match params with
  | [] when String.(name <> "main") -> "()"
  | params -> List.to_string emit_param ~sep:" " params

let emit_number num = string_of_float num

let emit_string str = Printf.sprintf "\"%s\"" str

let emit_symbol sym =
  Printf.sprintf "%s" (sym |> dehyphen_smart |> escape_utf8 |> convert_modules)

let rec emit_operator op args =
  match (Hashtbl.get ops op, args) with
  | Some symbol, [ left; right ] ->
      let left_str = emit_expr left in
      let right_str = emit_expr right in
      Printf.sprintf "(%s %s %s)" left_str symbol right_str
  | None, _ ->
      prerr_endline "Non-operator used as operator";
      exit 1
  | _, _ ->
      prerr_endline @@ "Invalid use of operator " ^ op;
      exit 1

(* Expression emitting *)
and emit_expr = function
  | Number n -> emit_number n
  | String s -> emit_string s
  | Symbol s -> emit_symbol s
  | List exprs -> emit_list exprs
  | ValueList _ as vl -> emit_value_list vl

(* Special forms *)
and emit_mod = function
  | Symbol name :: exprs ->
      depth := !depth - 2;
      let result =
        Printf.sprintf "module %s = struct\n%send\n\n" (emit_symbol name)
          (emit_list exprs)
      in
      depth := !depth + 2;
      result
  | _ ->
      prerr_endline "Invalid mod declaration";
      exit 1

and emit_use = function
  | Symbol name -> Printf.sprintf "open %s\n\n" (emit_symbol name)
  | _ ->
      prerr_endline "Invalid use of use";
      exit 1

and emit_def ~rec' = function
  | Symbol name :: rest ->
      incr depth;
      let name_str = emit_symbol name in
      let param_str, body =
        match rest with
        | ValueList params :: body -> (emit_params name_str params, body)
        | body when String.(name_str = "main") -> ("", body)
        | body -> ("()", body)
      in
      let body_str = emit_body body in
      decr depth;

      Printf.sprintf "let %s%s %s = \n%s%s\n\n"
        (if rec' then "rec " else "")
        (if String.(name_str = "main") then "()" else name_str)
        param_str body_str
        (if !depth >= 0 then "in" else "")
  | _ ->
      prerr_endline "Invalid def declaration";
      exit 1

and emit_let bindings =
  if List.length bindings >= 2 && List.length bindings mod 2 = 0 then
    fold_pairs
      (fun acc name value ->
        let name_str = emit_expr name in
        let value_str = emit_expr value in
        Printf.sprintf "%slet %s = %s in" acc name_str value_str)
      "" bindings
  else (
    prerr_endline "Invalid let declaration";
    exit 1)

and emit_if ~else' = function
  | [ pred; then_expr ] when not else' ->
      Printf.sprintf "if %s then %s" (emit_expr pred) (emit_expr then_expr)
  | [ pred; then_expr; else_expr ] when else' ->
      Printf.sprintf "if %s then %s\nelse %s" (emit_expr pred)
        (emit_expr then_expr) (emit_expr else_expr)
  | _ ->
      prerr_endline "Invalid use of if";
      exit 1

(* List handling *)
and emit_list exprs =
  incr depth;
  let result =
    match exprs with
    | Symbol "mod" :: rest -> emit_mod rest
    | [ Symbol "use"; rest ] -> emit_use rest
    | Symbol "def" :: rest -> emit_def ~rec':false rest
    | Symbol "def-rec" :: rest -> emit_def ~rec':true rest
    (* | Symbol "def-extern" -> *)
    (* | Symbol "fun" -> *)
    | Symbol "let" :: rest -> emit_let rest
    (* | Symbol "let-mut" -> *)
    | Symbol "if" :: rest -> emit_if ~else':true rest
    | Symbol "when" :: rest -> emit_if ~else':false rest
    (* | Symbol "case" -> *)
    (* | Symbol "cond" -> *)
    (* | Symbol "and" -> *)
    (* | Symbol "or" -> *)
    (* | Symbol "use" -> *)
    | Symbol op :: rest when Hashtbl.mem ops op -> emit_operator op rest
    | _ -> (
        let inner = List.map emit_expr exprs |> String.concat " " in
        match !depth with
        | -3 -> inner
        | depth when depth <= 0 -> (
            match List.hd exprs with Symbol "let" -> inner | _ -> inner ^ ";")
        | _ -> Printf.sprintf "(%s)" inner)
  in
  decr depth;
  result

and emit_value_list = function
  | ValueList exprs ->
      Printf.sprintf "[%s]" (List.map emit_expr exprs |> String.concat "; ")
  | _ ->
      prerr_endline "Invalid use of a value list";
      exit 1

(* Body emission helper *)
and emit_body exprs = List.map emit_expr exprs |> String.concat "\n"

(** Emits OCaml code from the AST produced by [Categorizer.categorize] *)
let emit ast = emit_expr ast
