(* Implementation of the insertion_sort sorting algorithm *)
fun insertion_sort (l : int list) : int list =
	let
		(* auxiliary function insert: insert element el to correct spot in list l. *)
		fun insert (el : int) (l : int list) : int list = 
			case l of
				[] => [el] 								(* Base case: list is empty. *)
			|	x::xs => if el < x then el::x::xs 		(* if el is less than head of list, make it new head. *)
							else x :: (insert el xs) 	(* else keep current head and make recursive call on tail of list. *)
	in
		case l of
			[] 	  => [] 								(* Base case: l is an empty list. *)
		|	x::xs => insert x (insertion_sort xs) 		(* Recursive case: insert x at correct place in tail of list sorted with insertion sort. *)
	end