open Bliss
open Cmdliner

module Bliss = struct
  let info =
    Cmd.info "bliss"
      ~doc:"The compiler and toolchain for the Bliss programming language."
      ~version:"0.0.1"
  ;;

  module Compile = struct
    let info =
      Cmd.info "compile" ~doc:"Compile a .bliss file to a native executable."
    ;;

    let filename =
      Arg.(
        value
        @@ opt string "main.bliss"
        @@ info [ "f"; "file" ] ~docv:"FILENAME" ~doc:"The file to compile.")
    ;;

    let compile filename = ignore @@ Parser.parse filename

    let cmd = Cmd.v info Term.(const compile $ filename)
  end

  module Prelude = struct
    let info = Cmd.info "prelude" ~doc:"Show the prelude."

    let fake =
      Arg.(
        value
        @@ opt string "main.bliss"
        @@ info [ "f"; "fake" ] ~docv:"FAKE"
             ~doc:
               "This should not be here but I cannot for the life of me figure \
                out how to make it work without it.")
    ;;

    let prelude _ =
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
    ;;

    let cmd = Cmd.v info Term.(const prelude $ fake)
  end

  let cmd = Cmd.group info [ Compile.cmd; Prelude.cmd ]
end

let () = exit @@ Cmd.eval Bliss.cmd
