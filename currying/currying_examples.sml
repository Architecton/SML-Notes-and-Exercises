(* Non-curried function - takes a single touple element *)
fun plus (a : int, b : int) = a + b;

(* Curried function that takes two arguments and can be partially applied *)
(* add =: ^ f a b = f a b *)
fun add a b = plus(a, b);

(* Partially aply add function. *)
val res = add 5;
(* Provide remaining argument. *)
val evaluation = res 4;
fun make_pair a b = (a, b);

(* Another example - composition *)
(* compose := ^ f g x . f g x *)
fun compose f g x = f(g(x));

(* f(g(x)) *)
(* Note the anonymous functions. *)
(* Compose the two anonymous functions. *)
apply = compose (fn x : int => 2 * x) (fn x : int => 5 * x)

(* Provide argument for composition. *)
apply 2

