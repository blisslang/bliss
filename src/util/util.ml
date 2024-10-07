open Containers

let whitespace = " \t\n"
let is_space c = String.contains whitespace c

let delims = "[]();\""
let is_delim c = String.contains delims c

let join arr = String.concat "" arr
