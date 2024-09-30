open Containers

let whitespace = " \t\n"
let is_space c = String.contains whitespace c

let delims = "[]();\""
let is_delim c = String.contains delims c

let take_last x = x |> String.rev |> String.take 1
