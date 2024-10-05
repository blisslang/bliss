open Containers

let compile file =
  print_endline @@ " ==> Compiling: " ^ file;

  let contents =
    try In_channel.(with_open_text file input_all)
    with Sys_error reason ->
      prerr_endline reason;
      exit 1
  in

  if not @@ String.ends_with ~suffix:".bliss" file then (
    prerr_endline
    @@ file
    ^ ": File has to contain Bliss source code (using file extension .bliss)";
    exit 1);

  print_endline "     * Tokenizing source...";
  let tokens = Compiler.Tokenizer.tokenize contents in

  (* print_endline @@ "TOKENS:\n" ^ Categorizer.show_tokens tokens; *)
  print_endline "     * Parsing tokens...";
  match Compiler.Categorizer.categorize [] (List []) tokens with
  | Ok _ast ->
      print_endline "     * Emitting Ocaml...";
      (* print_endline @@ "AST:\n" ^ Categorizer.show_node ast; *)
      print_endline " ==> Compilation finished as \027[36mmain.exe\027[0m"
  | Error reason ->
      prerr_endline reason;
      exit 1
;;
