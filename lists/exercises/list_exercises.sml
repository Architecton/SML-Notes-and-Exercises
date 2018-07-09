(* Construct a function first which returns the first element in a list of integers *)

fun first (xs : int list) =
	hd xs

(* Construct a function third which returns the third element in a list of integers*)
fun third (xs : int list) =
	let
		fun n_th (xs : int list, n : int) =
			if n = 1
				then hd xs
			else n_th(tl xs, n - 1)
	in
		n_th(xs, 3)
	end

(* Construct a function last which returns the last element in a list of integers *)
fun last (xs : int list) =
	if null (tl xs)
		then hd xs
	else last(tl xs)


(* Construct a function nth which returns the n-th element in a list of integers *)
fun nth (xs : int list, n : int) =
	if n = 1
		then hd xs
	else nth(tl xs, n - 1)

(* construct a function right which returns the element which is to the right of element el *)
fun right (xs : int list, el : int) = 
	if (hd xs) = el
		then hd (tl xs)
	else right(tl xs, el)

(* Construct a function split which splits a list of integers into a list containing the head and the tail as separate lists *)
fun split(xs : int list) =
	if null xs
		then []
	else [hd xs] :: tl xs :: [] 

(* Construct a function is_sublist which checks if xs is a sublist of ys *)
fun is_sublist (xs : int list, ys : int list) =
	let
		fun remainder (r1 : int list, r2 : int list) =
			if null r1
				then true
			else if hd r1 = hd r2
				then remainder(tl r1, tl r2)
			else false
	in
		if null xs
			then true
		else if null ys
			then false 
		else if hd xs = hd ys
			then remainder(tl xs, tl ys)
		else is_sublist(xs, tl ys)
	end

(* Construct a function add_start which adds integer el to the beginning of a list of integers xs *)
fun add_start(el : int, xs : int list) =
	el :: xs

(* Construct a function add_end which adds integer el to the end of the list of integers xs *)
fun add_end(el : int, xs : int list) =
	if null xs
	then [el]
	else hd xs :: add_end(el, tl xs)

(* Construct a function delete_one which deletes first instance of integer el from the list of integers xs *)
fun delete_one (el : int, xs : int list) =
	if null xs
	then []
	else if hd xs = el
	then tl xs
	else hd xs :: delete_one(el, tl xs)

(* Construct a function delete_all which deletes all instances of integer el from the list of integers xs *)
fun delete_all (el : int, xs : int list) =
	if null xs
	then []
	else if hd xs = el 
	then delete_all(el, tl xs)
	else hd xs :: delete_all(el, tl xs)

(* Construct a function all_equal which returns true if all integers in the list of integers xs are equal and false otherwise *)
fun all_equal(xs : int list) =
	if null xs
	then true
	else if null (tl xs)
	then true
	else if hd xs = hd (tl xs)
	then all_equal(tl xs)
	else false

(* Construct a function is_longer that checks if list of integers xs is longer than list of integers ys *)
fun is_longer (xs : int list, ys : int list) =
	if null xs andalso not (null ys) orelse null xs andalso null xs
	then false
	else if not (null xs) andalso null ys
	then true
	else is_longer(tl xs, tl ys)

(* Construct a function list_length which returns the length of the list of integers xs *)

fun list_length (xs : int list) =
	if null xs
	then 0
	else 1 + list_length(tl xs)

(* Construct a function expand which returns a list of n integers el *)
fun expand(el : int, n : int) =
	if n = 0
	then []
	else el :: expand(el, n - 1)

(* Construct a function sum_list which returns the sum of all the integers in the list xs *)
fun sum_list (xs : int list) =
	if null xs
	then 0
	else hd xs + sum_list(tl xs)

(* Construct a function reverse_list which reverses the list of integers xs*)
fun reverse (xs : int list) =
	let
		fun concat (a : int list, b : int list) =
			if null a
			then b
			else hd a :: concat(tl a, b)
	in
		if null xs
		then []
		else if null (tl xs)
		then [hd xs]
		else concat(reverse(tl xs), [hd xs])
	end

(* Construct a function equal which returns true if the lists of integers xs and ys are equal *)
fun equal (xs : int list, ys : int list) =
	if null xs andalso null ys then true
	else if null xs andalso not (null ys) orelse not (null xs) andalso null ys then false
	else if hd xs = hd ys then equal(tl xs, tl ys)
	else false 

(* Construct a function is_palindrome which returns true if the list of integers xs is a palindrome *)
fun is_palindrome (xs : int list) =
	equal(xs, reverse(xs))

(* Construct a function combine_integers which takes a list of integers and combines the digits into a single integer *)
fun combine_integers(xs : int list) =
	let
		fun list_length(xs: int list) =
			if null xs then 0
			else 1 + list_length(tl xs)

		val l_len = list_length(xs);

		fun power10(n : int) =
			if n = 0 then
				1
			else 10 * power10(n - 1)

		fun combine_integers_aux (l : int list, num : int) =
			if null l then 0
			else power10(l_len - num) * (hd l) + combine_integers_aux(tl l, num - 1)
	in
		combine_integers_aux(reverse(xs), l_len)
	end

(* Construct a function is_sorted_asc which returns true if the list of integers xs is sorted in ascending order *)
fun is_sorted_asc(xs : int list) =
	if null xs then true
	else if null (tl xs) then true
	else if hd xs <= hd (tl xs) then is_sorted_asc(tl xs)
	else false

(* Construct a function is_sorted_desc which returns true if the list of integers xs is sorted in ascending order *)
fun is_sorted_desc(xs : int list) =
	if null xs then true
	else if null (tl xs) then true
	else if hd xs >= hd (tl xs) then is_sorted_asc(tl xs)
	else false

(* Construct a function all_primes which returns true if all elements of the list of integers xs are primer numbers *)
fun all_primes(xs : int list) =
	let
		fun is_prime(num : int, n : int) =
			if (num mod n) <> 0 andalso n >= (num div 2) then true
			else if (num mod n) = 0 then false
			else is_prime(num, n + 1)
	in
		if null xs then false
		else if null (tl xs) then is_prime(hd xs, 2)
		else is_prime(hd xs, 2) andalso all_primes(tl xs)
	end

(* Construct a function every_second that returns every second element in the list of integers xs *)

(* Construct a function every_nth that returns every nth element in the list of integers xs *)

(* Construct a function nth_largest which return the n-th largest element in the list of integers xs *)

(* Construct a function nth_smallest which returns the n-th smallest element in the list of integers xs *)

(* Construct a function intersection which returns a list of elements that are both in list of integers xs and list of integers ys *)

(* Construct a function intersection which returns a list of elements that are both in list of integers xs and list of integers ys *)

(* Construct a funtion difference which returns removes from list of integers xs all elements that are also contained in ys *)

(* Construct a function swap that swaps elements at indices i and j in the list of integers xs *)

(* Construct a function index_max which returns the index of the largest element in the list of integers xs *)

(* Construct a function index_min which returns the index of the smallest element in the list of integers xs *)

(* Construct a function selection_sort which performs the recursive implementation of the selection sort sorting algorithm on the list of integers xs *)

(* Construct a function insertion_sort which performs the recursive implementation of the insertion sort sorting algorithm on the list of integers xs *)

(* Construct a function bubble_sort which performs the recursive implementation of the bubble sort sorting algorithm on the list of integers xs *)