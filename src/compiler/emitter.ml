open Types

let emit ast =
  print_endline @@ show_node ast;
  "<code-here>"
;;
