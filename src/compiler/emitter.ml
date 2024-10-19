open Containers
open Types
open Util

let dehyphen_smart s =
  let len = String.length s in
  if len = 1 && String.(s = "-") then s
  else
    String.mapi
      (fun i c -> if Char.(c = '-') && i > 0 && i < len - 1 then '_' else c)
      s

let rec emit_def stack rest =
  let get_params =
    List.to_string
      (fun x ->
        match x with
        | Symbol value -> value
        | _ ->
            prerr_endline "Params of a def may only contain symbols";
            exit 1)
      ~sep:" "
  in

  match rest with
  | Symbol name :: ValueList params :: body ->
      let symbol_params = get_params params in

      let fun_name =
        dehyphen_smart @@ if String.(name = "main") then "()" else name
      in

      let emitted_fun = emit_list "" body in

      join
        [
          stack;
          "let ";
          fun_name;
          " ";
          symbol_params;
          " = \n";
          emitted_fun;
          "\n;;\n";
        ]
  | _ ->
      prerr_endline "Invalid def declaration";
      exit 1

and emit_list stack rest =
  let emitted_list = emit "" rest in
  join [ stack; " "; emitted_list; "; " ]

and emit stack = function
  | [] ->
      print_endline @@ "FINAL STACK: " ^ stack;
      print_newline ();
      String.replace stack ~which:`Right ~sub:";" ~by:"_"
  | node :: rest -> (
      print_endline @@ "NODE: " ^ show_node node;
      print_endline @@ "REST: " ^ show_node_list rest;
      print_endline @@ "STACK: " ^ stack;
      print_newline ();
      match node with
      (* special forms (keywords) *)
      | Symbol "def" -> emit_def stack rest
      (* | Symbol "def-rec" -> emit stack *)
      (* | Symbol "def-extern" -> emit stack *)
      (* | Symbol "fun" -> emit stack *)
      (* | Symbol "let" -> emit stack *)
      (* | Symbol "let-mut" -> emit stack *)
      (* | Symbol "use" -> emit stack *)
      (* | Symbol "if" -> emit stack *)
      (* | Symbol "when" -> emit stack *)
      (* | Symbol "case" -> emit stack *)
      (* | Symbol "cond" -> emit stack *)
      (* | Symbol "and" -> emit stack *)
      (* | Symbol "or" -> emit stack *)
      (* else *)
      | List [ List x ] -> emit (emit_list stack x) rest
      | List x -> emit (emit_list stack x) rest
      | ValueList _x -> emit stack rest
      | Symbol x -> emit (join [ stack; dehyphen_smart x; " " ]) rest
      | Number x -> emit (join [ stack; Float.to_string x; " " ]) rest
      | String x -> emit (join [ stack; "\""; x; "\" " ]) rest)
