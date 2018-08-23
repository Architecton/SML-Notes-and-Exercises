(* Implementation of the recursive selection sort algorithm *)
fun selection_sort (l : int list) : int list =
	let
		(* min: function that takes a list of integers and returns the minimum value in the list. *)
		fun min (l : int list) = 
			if null l (* If list is empty, return NONE : int option *)
			then NONE
			else let 
			(* Else define an auxiliary max function which finds the maximum element in a list that
			we know is not empty. *)
				fun min_nonempty (l : int list) =
					if null (tl l) (* If list contains only a single element, max is the head (only element). *)
					then hd l (* Return head. *)
					else let
						val tl_ans = min_nonempty(tl l) (* Recursive call - find maximum element in tail of list *)
					in
						if hd l < tl_ans (* If head is larger than maximum element found in tail... *)
						then hd l (* ...return head. *)
						else tl_ans (* Else return maximum element found in tail. *)
					end
			in
				(* Return SOME min_element *)
				SOME (min_nonempty l)
			end

		(* index: function that takes an ordered pair of element and list and returns the index of the element el in list l. *)
		fun index (el : int, xs : int list) : int =
			if null xs then ~1 							(* If list is empty, return signal negative value. *)
			else if null (tl xs) then 					(* If list contains single element, check if it is equal to el. *)
				if hd xs = el then 0 					(* If equal to el, it is found at index 0. *)
				else valOf Int.minInt 					(* Else return signal negative value that turns any previous summation of indices negative. *)
			else if hd xs = el then 0 					(* If head of list l is the element we are looking for, it is found at index 0. *)
			else 1 + index(el, tl xs); 					(* Else, count current position and make recursive call for rest of list. *)

		(* swap swap elments at indices i and j in list xs.*)
		fun swap (xs : int list, i : int, j : int) =
			let
				(* get_nth: get nth element in list l *)
			 	fun get_nth (n : int, l : int list) =
			 		if n = 0 then hd l
			 		else get_nth(n - 1, tl l)

			 	(* set_nth: set nth element in list l to el and return resulting list *)
			 	fun set_nth (n : int, el : int, l : int list) =
			 		if n = 0 then el :: tl l
			 		else hd l :: set_nth(n - 1, el, tl l)

			 	(* Save ith and jth elements in xs *)
			 	val ith = get_nth(i, xs)
			 	val jth = get_nth(j, xs)
			 in
			 	(* Get reulting list. Note the nested function calls. *)
			 	set_nth(j, ith, set_nth(i, jth, xs))
			 end 

		(* minimum value found in list - if list empty then NONE else SOME min_value. *)
		val min_value : int option = min l
	in
		(* If minimum value in list found (list not empty), then... *)
		if isSome min_value then 
			let
				val index_min = index (valOf min_value, l)			(* Index_min is the index of the found minimum value. *)
				val rest_of_list = tl (swap (l, 0, index_min)) 		(* The rest of list is the tail of the current list with the minimum element and the first element swapped. *)
			in
				valOf min_value :: (selection_sort rest_of_list) 	(* Concatenate minimum value with result of performing selection sort on the rest of list. *)
			end
		else [] 													(* If minumum value not found, it means the list is empty. Return empty list. *)
	end