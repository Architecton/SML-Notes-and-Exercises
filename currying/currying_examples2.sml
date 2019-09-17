(* function add takes an integer and returns a function that takes another integer and returns the sum of the two integers. *)
val add1 : int -> int -> int  = fn a : int => fn b : int => a + b

(* Provide first element to function. *)
val addTo3 : int -> int = add1 3

(* This is equivalent to. *)
fun add2 a b = a + b
val addTo7 : int -> int = add2 7

