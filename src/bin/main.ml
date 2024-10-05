open Bliss
open Cmdliner

type cli =
  | Compile of {
      filename : string;
          [@pos 0]
          [@docv "FILENAME"]
          [@doc "The file to compile."]
          [@default "main.bliss"]
    }
      (**Compile a .bliss file to a native executable. Takes in a positional argument of the file to compile. If the file does not exist or is not a .bliss source file it stops immediately.*)
  | Prelude  (**Show the prelude.*)
[@@deriving subliner]

let run = function
  | Compile { filename } -> Commands.compile filename
  | Prelude ->
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

[%%subliner.cmds
eval.cli <- run]
[@@name "bliss"]
[@@version "0.0.1"]
[@@default
  Term.(
    ret
    @@ const
    @@ `Error (true, "unknown command, must be either 'compile' or 'prelude'."))]
(**The compiler and toolchain for the Bliss programming language.*)
