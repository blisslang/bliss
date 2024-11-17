module Bliss = struct
module IO = struct
let puts str = 
print_endline str;

end

 module String = struct
let of_float__21 str = 
string_of_float str;

end

 module Math = struct
let rec fib n = 
if (n < 2.) then n
else ((fib (n -. 1.)) +. (fib (n -. 2.)))

end

end

 open Bliss

 let ()  = 
IO.puts (String.of_float__21 (Math.fib 8.));

