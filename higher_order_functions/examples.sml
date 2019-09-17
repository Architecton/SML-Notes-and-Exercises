(* Simple example - make a function that takes an integer and returns an integer from anonymous function passed as an argument *)
fun make_int_function(int_function : int -> int) : int -> int =
	int_function

(* Create a function that doubles an integer using the make_int_function function. Pass anonymous function as argument *)
val double_int = make_int_function(fn x => 2 * x);

(* A well known example of a higher order function is the derivative operator. It is a function that takes a function and returns another function (the derivative of the original function) *)

