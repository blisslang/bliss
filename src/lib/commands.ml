open Containers

let compile file =
  let open Compiler in
  print_endline @@ " ==> Compiling: " ^ file;

  (* Exits with an error if the passed in source file isnt a bliss file *)
  if not @@ String.ends_with ~suffix:".bliss" file then (
    prerr_endline
    @@ file
    ^ ": File has to contain Bliss source code (using file extension .bliss)";
    exit 1);

  let contents =
    match In_channel.(with_open_text file input_all) with
    | a -> a
    | exception Sys_error reason ->
        prerr_endline reason;
        exit 1
  in

  print_endline "     * Tokenizing source...";
  let tokens = Tokenizer.tokenize contents in

  print_endline "     * Parsing tokens...";
  match Categorizer.categorize tokens with
  | Ok ast ->
      print_endline "     * Emitting OCaml...";
      let emitted = Emitter.emit ast in

      print_endline @@ "Emitted OCaml:\n" ^ emitted;

      let out_file = Out_channel.open_text "./_out/out.ml" in
      Out_channel.output_string out_file emitted;

      print_endline " ==> Compilation finished as \027[36mmain.exe\027[0m"
  | Error reason ->
      prerr_endline reason;
      exit 1

let prelude () =
  print_endline
    "                   \\   |   /            _\\/_\n\
    \                     .-'-.              //o\\  _\\/_\n\
    \  _  ___  __  _ --_ /     \\ _--_ __  __ _ | __/o\\\\ _\n\
     =-=-_=-=-_=-=_=-_= -=======- = =-=_=-=_,-'|\"'\"\"-|-,_\n\
    \ =- _=-=-_=- _=-= _--=====- _=-=_-_,-\"          |\n\
     jgs=- =- =-= =- = -  -===- -= - .\"\n\n\
     The compiler and toolchain for the Bliss programming language.\n\n\
     Credits:\n\
    \ * DJARUUN (https://github.com/djaruun)\n\
    \ * The almighty ChatGPT (https://chatgpt.com)"
