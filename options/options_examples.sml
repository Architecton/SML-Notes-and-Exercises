(* Options

t option is a type for any type t
- (much like t list, but a different type, not a list)

Building:
	- NONE has type 'a option (much like [] has type 'a list)
	- SOME e has type t option if e has type t (much like e::[])

Accessing:
	- isSome has type 'a option -> bool
	- valOf has type 'a option -> 'a (exception if given NONE)

 *)

(* fn : int list -> int option *)
fun max(xs : int list) =
	if null xs (* If passed list is empty, return NONE : int option *)
	then NONE
	else
		let 
			val tl_ans = max(tl xs) (* Recursive call - find max in tail *)
		in
			if isSome tl_ans andalso valOf tl_ans > hd xs (* If tail was non empty and tail max is greater than head *)
			then tl_ans (* Return max found in tail. *)
			else SOME (hd xs) (* Else return head of list. *)
		end

(* Another implementation of the maximum finding function *)
(* This time we first check if the list is empty. If it is not, we define an auxiliary max function
which finds the maximum in a list that we know is not empty. *)
fun max_2(xs : int list) = 
	if null xs (* If list is empty, return NONE : int option *)
	then NONE
	else let 
	(* Else define an auxiliary max function which finds the maximum element in a list that
	we know is not empty. *)
		fun max_nonempty (xs : int list) =
			if null (tl xs) (* if singleton list, max is the head (only element) *)
			then hd xs (* Return head. *)
			else let
				val tl_ans = max_nonempty(tl xs) (* Recursive call - find maximum element in tail of list *)
			in
				if hd xs > tl_ans (* If head is larger than maximum element found in tail... *)
				then hd xs (* ...return head. *)
				else tl_ans (* Else return maximum element found in tail. *)
			end
	in
		(* Return SOME : int option *)
		SOME (max_nonempty xs)
	end

	(* The value from an int option can be extracted using the valOf function
	(fn : 'a option -> 'a) *)