(* linear_search: find index of element el and return it *)
fun linear_search (el : int) (l : int list) : int option =
	let
		(* Auxiliary function which signals absence of element el in list l by returning a negative value. *)
		(* If el is present in the list l then it returns the index of the element el. *)
		fun linear_search_aux (el : int) (l : int list) : int =
			case l of
				[] 	  => valOf Int.minInt 									(* Return signal value for absence of el. *)
			|	[x]   => if x = el then 0 else valOf Int.minInt 			(* Return signal value for absence of el. The number negates any summation in the next case clause. *)
			|	x::xs => if x = el then 0 else 1 + linear_search_aux el xs	(* Compare head of list to el. If it is equal then el is at position 0. else count this cell... *)
																			(* ...and make recursive call for remainder of l. *)
		(* Result of linear_search_aux function *)
		val result : int = linear_search_aux el l
	in
		(* If result is positive return SOME result else signal absence with option NONE *)
		if result >= 0 then SOME result else NONE
	end