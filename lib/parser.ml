open Containers
open Util

type node =
  | List of node list
  | ValueList of node list
  | String of string
  | Symbol of string
  | Number of float
[@@deriving show]

type tokens = string list [@@deriving show]

let atom x =
  match Float.of_string_opt x with
  | Some x_float -> Number x_float
  | None -> Symbol x
;;

let add_to_node node y =
  match (node, y) with
  | List nodes, y -> Ok (List (y :: nodes))
  | ValueList value_nodes, y -> Ok (ValueList (y :: value_nodes))
  | String string, String y -> Ok (String (string ^ " " ^ y))
  | _, _ -> Error "x has to be a collection node"
;;

let reverse_node = function
  | List nodes -> List (List.rev nodes)
  | ValueList value_nodes -> ValueList (List.rev value_nodes)
  | other -> other
;;

let rec categorize stack current = function
  | [] -> Ok (reverse_node current)
  | token :: rest -> (
      match current with
      (* in string *)
      | String string_value -> (
          match token with
          (* closing *)
          | "\"" when not @@ String.equal (take_last string_value) "\\" -> (
              match stack with
              | new_current :: new_stack -> (
                  match add_to_node new_current @@ reverse_node current with
                  | Ok new_new_current ->
                      categorize new_stack new_new_current rest
                  | Error reason -> Error reason)
              | [] -> Error "Unexpected '\"'")
          (* else *)
          | _ -> (
              match add_to_node current @@ String token with
              | Ok new_current -> categorize stack new_current rest
              | Error reason -> Error reason))
      (* not in string *)
      | _ -> (
          match token with
          (* comment *)
          | ";" ->
              let new_rest =
                List.drop_while (fun elem -> not @@ String.equal ";" elem) rest
                |> List.drop 1
              in
              categorize stack current new_rest
          (* opening *)
          | "(" -> categorize (current :: stack) (List []) rest
          | "[" -> categorize (current :: stack) (ValueList []) rest
          | "\"" -> categorize (current :: stack) (String "") rest
          (* closing *)
          | ")" -> (
              match stack with
              | new_current :: new_stack -> (
                  match add_to_node new_current @@ reverse_node current with
                  | Ok new_new_current ->
                      categorize new_stack new_new_current rest
                  | Error reason -> Error reason)
              | [] -> Error "Unexpected ')'")
          | "]" -> (
              match stack with
              | new_current :: new_stack -> (
                  match add_to_node new_current @@ reverse_node current with
                  | Ok new_new_current ->
                      categorize new_stack new_new_current rest
                  | Error reason -> Error reason)
              | [] -> Error "Unexpected ']'")
          (* else *)
          | _ -> (
              match add_to_node current @@ atom token with
              | Ok new_current -> categorize stack new_current rest
              | Error reason -> Error reason)))
;;

let rec contextual_split acc = function
  | [] -> List.rev acc
  | c :: rest when String.contains "[]();\"" c ->
      contextual_split (String.of_char c :: acc) rest
  | c :: rest ->
      let token =
        List.take_while (fun c' -> not (is_delim c' || is_space c')) (c :: rest)
      in
      let rest' = List.drop (List.length token) rest in
      contextual_split (String.of_list token :: acc) rest'
;;

let pad_delims =
  String.fold_right
    (fun c acc ->
      if is_delim c then " " ^ String.of_char c ^ " " ^ acc
      else String.of_char c ^ acc)
    ""
;;

let tokenize code =
  contextual_split [] @@ String.to_list (code |> pad_delims |> String.trim)
  |> List.filter (fun s -> not @@ String.is_empty s)
;;

let parse file =
  print_endline @@ " * Compiling " ^ file;

  let contents =
    try In_channel.(with_open_text file input_all)
    with Sys_error reason ->
      prerr_endline reason;
      exit 1
  in

  let tokens = tokenize contents in
  print_endline @@ show_tokens tokens;

  let categorized = categorize [] (List []) tokens in
  match categorized with
  | Ok ast ->
      print_endline @@ show_node ast;
      ast
  | Error reason ->
      prerr_endline reason;
      exit 1
;;
