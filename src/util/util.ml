open Containers

let whitespace = " \t\n"
let is_space c = String.contains whitespace c

let delims = "[]();\""
let is_delim c = String.contains delims c

let dehyphen_smart s =
  let len = String.length s in
  if len = 1 && String.(s = "-") then s
  else
    String.mapi
      (fun i c -> if Char.(c = '-') && i > 0 && i < len - 1 then '_' else c)
      s

let contains lst x = List.exists (fun x' -> String.(x = x')) lst

let rec fold_pairs f acc = function
  | [] -> acc
  | [ _ ] -> acc
  | [ x; y ] -> f acc x y
  | x :: y :: rest -> fold_pairs f (f acc x y) rest

let is_ocaml_friendly c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false

let escape_utf8 str =
  let buf = Buffer.create (String.length str * 2) in
  String.iteri
    (fun i c ->
      if is_ocaml_friendly c then Buffer.add_char buf c
      else
        let hex =
          Printf.sprintf
            (match i with
            | 0 -> "%02x__"
            | i when i = String.length str - 1 -> "__%02x"
            | _ -> "__%02x__")
            (Char.code c)
        in
        Buffer.add_string buf hex)
    str;
  Buffer.contents buf

let convert_modules str = String.replace ~sub:"__2f__" ~by:"." str
