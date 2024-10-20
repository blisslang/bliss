open Containers

type node =
  | List of node list
  | ValueList of node list
  | String of string
  | Symbol of string
  | Number of float
[@@deriving show]

let show_node_list lst = List.to_string (fun x -> show_node x) lst
