open Containers
open Types

let atom x =
  match Float.of_string_opt x with Some x' -> Number x' | None -> Symbol x

let string x = String x

let add_to_node node y =
  match (node, y) with
  | List nodes, y -> Ok (List (y :: nodes))
  | ValueList value_nodes, y -> Ok (ValueList (y :: value_nodes))
  | String string, String y -> Ok (String (string ^ " " ^ y))
  | _, _ -> Error "x has to be a collection node"

let reverse_node = function
  | List nodes -> List (List.rev nodes)
  | ValueList value_nodes -> ValueList (List.rev value_nodes)
  | other -> other

let rec categorize_closing stack current rest ~str_rep =
  match stack with
  | new_current :: new_stack -> (
      match add_to_node new_current @@ reverse_node current with
      | Ok new_new_current -> categorize new_stack new_new_current rest
      | Error reason -> Error reason)
  | [] -> Error ("Unexpected '" ^ str_rep ^ "'")

and categorize_comment stack current rest =
  let new_rest =
    List.drop_while (fun elem -> not @@ String.equal ";" elem) rest
    |> List.drop 1
  in
  categorize stack current new_rest

and categorize_else stack current rest token ~type_fun =
  match add_to_node current @@ type_fun token with
  | Ok new_current -> categorize stack new_current rest
  | Error reason -> Error reason

and categorize stack current = function
  | [] -> Ok (reverse_node current)
  | token :: rest -> (
      match current with
      | String string_value -> (
          match token with
          (* closing *)
          | "\"" when not @@ String.ends_with ~suffix:"\\" string_value ->
              categorize_closing stack current rest ~str_rep:"\""
          (* else *)
          | _ -> categorize_else stack current rest token ~type_fun:string)
      | _ -> (
          match token with
          (* comment *)
          | ";" -> categorize_comment stack current rest
          (* opening *)
          | "(" -> categorize (current :: stack) (List []) rest
          | "[" -> categorize (current :: stack) (ValueList []) rest
          | "\"" -> categorize (current :: stack) (String "") rest
          (* closing *)
          | ")" -> categorize_closing stack current rest ~str_rep:")"
          | "]" -> categorize_closing stack current rest ~str_rep:"]"
          (* else *)
          | _ -> categorize_else stack current rest token ~type_fun:atom))

let categorize tokens = categorize [] (List []) tokens
