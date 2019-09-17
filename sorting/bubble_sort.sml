(* bubble_sort: a recursive implementation of the bubble_sort sorting algorithm *)
fun bubble_sort (l : int list) : int list =
	let
		(* get_last: get last element in list l *)
		fun get_last (l : int list) : int =
			case l of
				[] 	  => raise Fail "Empty list"								(* If list is empty, raise exception. *)
			|	[x]   => x 														(* Base case: list contains single element - return it. *)
			|	x::xs => get_last xs 											(* Recursive case: get last element in tail. *)

		(* pass: make a single bubble sort pass over list l and return resulting list.*)
		fun pass (l : int list) =
			case l of
				[] 	  => [] 													(* Base case: list l is empty *)
			|	[x]   => [x] 													(* Base case: list l contains a single element *)
			|	x::xs => if x > (hd xs) then (hd xs) :: pass (x :: (tl xs)) 	(* Recursive case: perform comparison, swap if necessary and make recursive call for tail *)
						 else x :: pass xs

		(* add_last: add element el to the end of list l. *)
		fun add_last (el : int) (l : int list) : int list =
			case l of
				[] 		=> [el] 												(* Base case: adding element to empty list *)
			|	x :: xs => x :: add_last el xs 									(* Recursive case: keep head and add element to tail. *)

		(* remove_last: remove last element from list l and return result. *)
		fun remove_last (l : int list) : int list =
			case l of
				[] 		=> raise Fail "Empty list" 								(* If list l is empty, raise exception. *)
			|	[x] 	=> [] 													(* Base case: if removing element from list with single element, return empty list. *)
			|	x :: xs => x :: remove_last xs 									(* Recursive case: keep head and remove last element from tail. *)

		(* passed_list is list l after a single bubble sort pass. *)
		val passed_list = pass l
	in
		if null l then [] 														(* Base case: if list l is empty, return empty list. *)
		else let
			(* last element in list after single bubble sort pass - this is the largest element in l. *)
			val last = get_last passed_list
			(* list after single bubble sort pass with last (largest) element removed *)
			val rem_list = remove_last passed_list
		in
			(* add largest element to the end of the rest of the list sorted with bubble sort. *)
			add_last last (bubble_sort rem_list)
		end
	end