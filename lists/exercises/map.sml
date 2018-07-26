(* Define a function double that doubles an integer value. *)
val double = fn x => 2*x;

(* Create a list of integers for testing. *)
val test_list = [1, 2, 3, 4, 5, 6, 7, 8, 9];

(* Apply function to every element in test_list. *)
val doubled = List.map double test_list;

(* Define a function sqr that squares an integer value. *)
fun sqr (x : int) : int =
	x*x;

(* Apply function sqr to every element in test_list and save result in squared. *)
val squared = List.map sqr test_list;

(* Example of an explicitly written mapping function. *)
fun map f [] = []
  | map f (x::xs) = f(x) :: map f xs

(* Define a function that increments an integer value, *)
fun add_one x = x + 1;

(* Apply function add_one to every element in test_list *)
val incremented_list = map add_one test_list;