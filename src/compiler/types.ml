type node =
  | List of node list
  | ValueList of node list
  | String of string
  | Symbol of string
  | Number of float
[@@deriving show]

type node_list = node list [@@deriving show]

type tokens = string list [@@deriving show]
