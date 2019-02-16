
(* Function that checks if the value is non-negative. *)
val is_positive = fn x => x >= 0

(* Create list for testing. *)
val test_list = [1, 2, 3, ~4, 5, ~1, ~12, 44]

(* Save only values (in positive_values) for which is_positive returns true in test_list *)
val positive_values = List.filter is_positive test_list



(* Different way *)

(* Define a function that returns true if argument integer lesser than 0  *)
fun is_negative(x : int) : bool = x < 0

(* Create a filter: a function that maps a ist of integers to a list of integers *)
val filt : int list -> int list = List.filter(is_negative)

(* Apply filter to test_list *)
val negative_values : int list = filt test_list



(* Another different way (more compact) *)
val greater_than_2 : int list = List.filter (fn x => x > 2) test_list 