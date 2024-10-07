open Containers
open Types

module L = List
module S = String
module F = Float
module U = Util

let rec emit_def stack current rest =
  match rest with
  | Symbol name :: ValueList params :: rest' ->
      let symbol_params =
        L.to_string
          (fun elem ->
            match elem with
            | Symbol value -> value
            | _ ->
                prerr_endline "Params of a def may only contain symbols";
                exit 1)
          params ~sep:" "
      in

      let fun_start =
        let fun_name = if S.( = ) name "main" then "()" else name in
        U.join [ stack; "let "; fun_name; " "; symbol_params; " = " ]
      in
      emit fun_start current rest'
  | _ ->
      prerr_endline "Invalid def declaration";
      exit 1

and emit_list stack current rest =
  let emitted_list = emit "" current rest in
  emit (U.join [ stack; "( "; emitted_list; ") " ]) current

and emit stack current = function
  | [] -> stack
  | node :: rest ->
      print_endline @@ "STACK: " ^ stack;
      (match node with
      (* special forms (keywords) *)
      | Symbol "def" -> emit_def stack current
      (* | Symbol "def-rec" -> emit stack current *)
      (* | Symbol "def-extern" -> emit stack current *)
      (* | Symbol "fun" -> emit stack current *)
      (* | Symbol "let" -> emit stack current *)
      (* | Symbol "let-mut" -> emit stack current *)
      (* | Symbol "use" -> emit stack current *)
      (* | Symbol "if" -> emit stack current *)
      (* | Symbol "when" -> emit stack current *)
      (* | Symbol "case" -> emit stack current *)
      (* | Symbol "cond" -> emit stack current *)
      (* | Symbol "and" -> emit stack current *)
      (* | Symbol "or" -> emit stack current *)
      (* else *)
      | List [ List inner_list ] -> emit_list "" current inner_list
      | List inner_list -> emit_list "" current inner_list
      | ValueList _value -> emit stack current
      | Symbol value -> emit (U.join [ stack; value; " " ]) current
      | Number value -> emit (U.join [ stack; F.to_string value; " " ]) current
      | String value -> emit (U.join [ stack; "\""; value; "\" " ]) current)
        rest
;;
