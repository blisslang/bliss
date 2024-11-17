open Bliss
open Cmdliner

type params =
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

let handle = function
  | Compile { filename } -> Commands.compile filename
  | Prelude -> Commands.prelude ()

[%%subliner.cmds
eval.params <- handle]
[@@name "bliss"]
[@@version "0.0.1"]
[@@default Term.(ret @@ const @@ `Error (true, "unknown command"))]
(**The compiler and toolchain for the Bliss programming language.*)
