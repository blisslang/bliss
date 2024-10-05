open Types

let rec emit_list acc = function
  | [ node ] -> emit acc node
  | node :: rest -> (
      match node with
      (* special forms (keywords) *)
      | Symbol "def" -> emit_list acc rest
      | Symbol "def-rec" -> emit_list acc rest
      | Symbol "def-extern" -> emit_list acc rest
      | Symbol "fun" -> emit_list acc rest
      | Symbol "let" -> emit_list acc rest
      | Symbol "let-mut" -> emit_list acc rest
      | Symbol "use" -> emit_list acc rest
      | Symbol "if" -> emit_list acc rest
      | Symbol "and" -> emit_list acc rest
      | Symbol "or" -> emit_list acc rest
      (* else *)
      | List _list -> emit_list acc rest
      | ValueList _value_list -> emit_list acc rest
      | Symbol _symbol -> emit_list acc rest
      | Number _number -> emit_list acc rest
      | String _string -> emit_list acc rest)
  | _ ->
      prerr_endline "Empty list";
      exit 1

and emit acc = function
  | List list ->
      print_endline @@ "ACC: \"" ^ acc ^ "\"";
      emit_list (acc ^ " (") list
  | _ ->
      prerr_endline "Invalid start of code";
      exit 1
;;
