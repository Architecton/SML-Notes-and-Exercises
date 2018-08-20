fun plus (a : int, b : int) = a + b;

(* add =: ^ f a b = f a b *)
fun add a b = plus(a, b);

val res = add 5;
val evaluation = res 4;
fun make_pair a b = (a, b);

(* compose := ^ f g x . f g x *)
fun compose f g x = f(g(x));

(* f(g(x)) *)
(* Note the anonymous functions *)
compose (fn x : int => 2 * x) (fn x : int => 5 * x) 2