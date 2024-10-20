open Containers
open Util

let pad_delims str ~acc =
  String.fold_right
    (fun c acc' ->
      if is_delim c then " " ^ String.of_char c ^ " " ^ acc'
      else String.of_char c ^ acc')
    str acc

(** Tokenizes Bliss source code by padding delimiters and splitting on whitespace *)
let tokenize code =
  let split =
    code
    |> pad_delims ~acc:""
    |> String.replace ~sub:"\n" ~by:" "
    |> String.replace ~sub:"\t" ~by:" "
    |> String.split ~by:" "
  in
  List.filter (fun s -> not @@ String.is_empty s) split
