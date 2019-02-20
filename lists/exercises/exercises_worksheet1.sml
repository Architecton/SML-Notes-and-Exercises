(* Construct a function first which returns the first element in a list of integers *)
fun get_first(l : int list) : int = hd l

(* Construct a function third which returns the third element in a list of integers*)
fun get_third(l : int list) : int =
	let
		fun get_nth(n, l) : int =
			if n = 1 then hd l
			else get_nth(n-1, tl l)
	in
		get_nth(3, l)
	end

(* Construct a function last which returns the last element in a list of integers *)
fun get_last(l: int list) =
	if null (tl l) then hd l
	else get_last(tl l)


(* Construct a function nth which returns the n-th element in a list of integers *)

fun nth 1 l = hd l
  | nth n l = nth (n-1) (tl l)


(* construct a function right which returns the element which is to the right of element el *)

fun right(el, l) =
	if hd l = el then hd (tl l)
	else right(el, tl l)

(* Construct a function split which splits a list of integers into a list containing the head and the tail as separate lists *)

fun split(l) = [[hd l], tl l]


(** Construct a function is_sublist which checks if xs is a sublist of ys *)

fun is_sublist(xs, ys) : bool =
	let
		fun remainder [] ys = true
		  | remainder (x::xs) (y::ys) =
		  	if x = y then remainder xs ys
		  	else false
		  | remainder xs ys = false
	in
		if null xs then true
		else if hd xs = hd ys then remainder xs ys
		else is_sublist(xs, tl ys)
	end

(* Construct a function add_start which adds integer el to the beginning of a list of integers xs *)
fun add_start el xs = el::xs

(* Construct a function add_end which adds integer el to the end of the list of integers xs *)
fun add_end el [] = [el]
  | add_end el (x::xs) = x::(add_end el xs)

(* Construct a function delete_one which deletes first instance of integer el from the list of integers xs *)
fun delete_one el (x::xs) = if x = el then xs else x::(delete_one el xs)
  | delete_one el [] = []

(* Construct a function delete_all which deletes all instances of integer el from the list of integers xs *)
fun delete_all(el : int, l : int list) : int list =
	if null l then []
	else if hd l = el then delete_all(el, tl l)
	else hd l::(delete_all(el, tl l))

(* Construct a function all_equal which returns true if all integers in the list of integers xs are equal and false otherwise *)
fun all_equal(xs : int list) : bool =
	if null (tl xs) orelse null xs then true
	else if hd xs = hd (tl xs) then all_equal(tl xs)
	else false

(* Construct a function is_longer that checks if list of integers xs is longer than list of integers ys *)
fun is_longer xs [] = true
  | is_longer [] ys = false
  | is_longer (x::xs) (y::ys) = if null xs andalso null ys 
  								then false else is_longer xs ys

(* Construct a function list_length which returns the length of the list of integers xs *)
fun list_length(xs : int list) : int =
	if null xs then 0 else 1 + list_length(tl xs)


(* Construct a function expand which returns a list of n integers el *)
fun expand(el, n) =
	if n = 0 then []
	else if n = 1 then [el]
	else el :: expand(el, n-1)


(* Construct a function sum_list which returns the sum of all the integers in the list xs *)
fun sum_list [] = 0
  | sum_list (x::xs) = x + sum_list xs

(** Construct a function reverse_list which reverses the list of integers xs*)
fun reverse_list(xs : int list) : int list =
	let
		fun concat(a : int list, b : int list) : int list =
			if null a then b
			else hd a :: concat(tl a, b)
	in
		if null xs then []
		else if null (tl xs) then xs
		else concat(reverse_list(tl xs), [hd xs])
	end

(* Construct a function equal which returns true if the lists of integers xs and ys are equal *)
fun equal(xs : int list, ys : int list) : bool =
	if null xs andalso null ys then true
	else if null xs orelse null ys then false
	else if hd xs = hd ys then
		equal(tl xs, tl ys)
	else
		false

(* Construct a function is_palindrome which returns true if the list of integers xs is a palindrome *)
fun is_palindrome(xs : int list) : bool =
	let
		fun concat(l1 : int list, l2 : int list) : int list =
			if null l1 then l2
			else hd l1 :: concat(tl l1, l2)

		fun reverse(xs : int list) : int list =
			if null xs then []
			else if null (tl xs) then [hd xs]
			else concat(reverse(tl xs), [hd xs])
	in
		if reverse(xs) = xs then true else false
	end


(* Construct a function combine_integers which takes a list of integers and combines the digits into a single integer *)

(* Construct a function is_sorted_asc which returns true if the list of integers xs is sorted in ascending order *)

(* Construct a function is_sorted_desc which returns true if the list of integers xs is sorted in ascending order *)

(* Construct a function all_primes which returns true if all elements of the list of integers xs are primer numbers *)

(* Construct a function every_second that returns every second element in the list of integers xs *)

(* Construct a function every_nth that returns every nth element in the list of integers xs *)

(* Construct a function intersection which returns a list of elements that are both in list of integers xs and list of integers ys *)

(* Construct a funtion difference which returns a list of integers that are in xs but not also in ys *)

(* Construct a function swap that swaps elements at indices i and j in the list of integers xs *)

(* Construct a function index which returns the index of element el in list *)

(* Return negative number if el is not in list. *)

(* Construct a function max which finds the maximum element in an int list *)

(* Connstruct a function min which finds the minimum element in an int list *)

(* Construct a function index_max which returns the index of the largest element in the list of integers xs *)

(* Construct a function index_min which returns the index of the smallest element in the list of integers xs *)

(* Construct a function selection_sort which performs the recursive implementation of the selection sort sorting algorithm on the list of integers xs *)

(* Construct a function insertion_sort which performs the recursive implementation of the insertion sort sorting algorithm on the list of integers xs *)

(* Construct a function bubble_sort which performs the recursive implementation of the bubble sort sorting algorithm on the list of integers xs *)

(* Construct a function rle which encodes a list of characters using the run-length encoding algorithm.*)

(* Construct a function mat_multiply which takes two matrices represented as lists of lists and returns their matrix product.*)

