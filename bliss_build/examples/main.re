module Core = {
module IO = {
let puts = (str) => {
print_string(str);
};
let putln = (x) => {
print_endline(x);
};
};
module Math = {
let fib = (n) => {
if (n <= 1.0) {
n;
}
else {
fib(n - 1.0) + fib(n - 2.0);
};
};
};
};
open Core;
let main = () => {
let res = Math.fib(8.0);
IO.puts("Fib result: ");
IO.putln(res);
};