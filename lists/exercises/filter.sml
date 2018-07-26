(* Function that checks if the value is non-negative. *)
val is_positive = fn x => x >= 0;

(* Create list for testing. *)
val test_list = [1, 2, 3, ~4, 5, ~1, ~12, 44];

(* Save only values (in positive_values) for which is_positive returns true in test_list *)
val positive_values = List.filter is_positive test_list