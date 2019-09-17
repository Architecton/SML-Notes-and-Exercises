(* ----------------- PART 1 ----------------- *)

(* 1. Construct a function first which returns the first element in a list of integers *)
fun first (xs : int list) =
  hd xs

(* 2. Construct a function third which returns the third element in a list of integers*)
fun third (xs : int list) =
  let fun nth (xs : int list, n : int) =
      if n = 1 then hd xs
      else nth (tl xs, n-1)
  in
    nth(xs, 3)
  end

(* 3. Construct a function last which returns the last element in a list of integers *)
fun last (xs : int list) =
  if null (tl xs) then hd xs
  else last(tl xs) 

(* 4. Construct a function nth which returns the n-th element in a list of integers *)
fun nth (xs : int list, n : int) =
  if n = 1 then hd xs
  else nth (tl xs, n-1)

(* 5. construct a function right which returns the element which is to the right of element el *)
fun right (xs : int list, el : int) =
  if hd xs = el then hd (tl xs)
  else right(tl xs, el)

(* 6. Construct a function split which splits a list of integers into a list containing the head and the tail as separate lists *)
fun split (xs : int list) =
  if null xs then []
  else [hd xs] :: tl xs :: []

(*** 7. Construct a function is_sublist which checks if xs is a sublist of ys *)
fun is_sublist (xs : int list, ys : int list) =
    let fun remainder (xs : int list, ys : int list) =
        if null xs then true
        else if hd xs = hd ys then remainder (tl xs, tl ys)
        else false
    in
        if null xs then true
        else if null ys then false
        else if hd xs = hd ys then remainder (tl xs, tl ys)
        else is_sublist (xs, tl ys)
    end

(* ----------------- PART 2 ----------------- *)

(* 8. Construct a function add_start which adds integer el to the beginning of a list of integers xs *)
fun add_start (xs : int list, el : int) = el :: xs

(* 9. Construct a function add_end which adds integer el to the end of the list of integers xs *)
fun add_end (xs : int list, el : int) =
  if null xs then el :: []
  else hd xs :: add_end (tl xs, el)

(* 10. Construct a function delete_one which deletes first instance of integer el from the list of integers xs *)
fun delete_one (xs : int list, el : int) =
  if hd xs = el then tl xs
  else hd xs :: delete_one (tl xs, el)

(* 11. Construct a function delete_all which deletes all instances of integer el from the list of integers xs *)
fun delete_all (xs : int list, el : int) =
  if null xs then []
  else if hd xs = el then delete_all (tl xs, el)
  else hd xs :: delete_all (tl xs, el)

(* 12. Construct a function all_equal which returns true if all integers in the list of integers xs are equal and false otherwise *)
fun all_equal (xs : int list) =
  if null xs orelse null (tl xs) then true
  else if hd xs = hd (tl xs) then all_equal(tl xs)
  else false

(* ----------------- PART 3 ----------------- *)

(* 13. Construct a function is_longer that checks if list of integers xs is longer than list of integers ys *)
fun is_longer (xs : int list, ys : int list) =
  if null ys andalso not (null xs) then true
  else if null xs then false
  else is_longer (tl xs, tl ys)

(* 14. Construct a function list_length which returns the length of the list of integers xs *)
fun list_length (xs : int list) =
  if null xs then 0
  else 1 + list_length (tl xs)

(* 15. Construct a function expand which returns a list of n integers el *)
fun expand (n : int, el : int) =
  if n = 0 then []
  else el :: expand (n-1, el)

(* 16. Construct a function sum_list which returns the sum of all the integers in the list xs *)
fun sum_list (xs : int list) =
  if null xs then 0
  else hd xs + sum_list (tl xs)

(* ----------------- PART 4 ----------------- *)

(*** 17. Construct a function reverse_list which reverses the list of integers xs*)

(* 18. Construct a function equal which returns true if the lists of integers xs and ys are equal *)

(* 19. Construct a function is_palindrome which returns true if the list of integers xs is a palindrome *)

(*** 20. Construct a function combine_integers which takes a list of integers and combines the digits into a single integer *)

(** 21. Construct a function is_sorted_asc which returns true if the list of integers xs is sorted in ascending order *)

(* 22. Construct a function is_sorted_desc which returns true if the list of integers xs is sorted in ascending order *)

(* 23. Construct a function all_primes which returns true if all elements of the list of integers xs are primer numbers *)

(* ----------------- PART 5 ----------------- *)

(* 24. Construct a function every_second that returns every second element in the list of integers xs *)

(* 25. Construct a function every_nth that returns every nth element in the list of integers xs *)

(* 26. Construct a function intersection which returns a list of elements that are both in list of integers xs and list of integers ys *)

(* Construct a funtion difference which returns a list of integers that are in xs but not also in ys *)

(* Construct a function swap that swaps elements at indices i and j in the list of integers xs *)

(* Construct a function index which returns the index of element el in list *)

(* ----------------- PART 6 ----------------- *)

(* Construct a function max which finds the maximum element in an int list *)

(* Connstruct a function min which finds the minimum element in an int list *)

(* Construct a function index_max which returns the index of the largest element in the list of integers xs *)

(* Construct a function index_min which returns the index of the smallest element in the list of integers xs *)

(* Construct a function rle which encodes a list of characters using the run-length encoding algorithm.*)

(* Construct a function mat_multiply which takes two matrices represented as lists of lists and returns their matrix product.*)

