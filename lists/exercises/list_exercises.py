# (* Construct a function first which returns the first element in a list of integers *)
def first(l):
	return l[0]

# (* Construct a function third which returns the third element in a list of integers*)
def third(l):
	def nth(n, l):
		if n == 0:
			return l[0]
		else:
			return nth(n-1, l[1:])
	return nth(2, l)

# (* Construct a function last which returns the last element in a list of integers *)
def last(l):
	if len(l) == 1:
		return l[0]
	else:
		return last(l[1:])

# (* Construct a function nth which returns the n-th element in a list of integers *)
def nth(n, l):
	if n == 0:
		return l[0]
	else:
		return nth(n-1, l)

# (* construct a function right which returns the element which is to the right of element el *)
def right(el, l):
	if l[0] == el:
		return l[1]
	else:
		return right(el, l[1:])

# (* Construct a function split which splits a list of integers into a list containing the head and the tail as separate lists *)
def split(l):
	return [l[0], [l[1:]]]


# (** Construct a function is_sublist which checks if xs is a sublist of ys *)
def is_sublist(l1, l2):
	def remainder(s, l2):
		if len(s) == 0:
			return True
		elif s[0] == l2[0]:
			return remainder(s[1:], l2[1:])
		else:
			return False
	if len(l1) == 0:
		return true
	elif l1[0] == l2[0]:
		return remainder(l1[1:], l2[1:])
	else:
		return is_sublist(l1, l2[1:])


# (* Construct a function add_start which adds integer el to the beginning of a list of integers l *)
def add_start(el, l):
	return [el] + l

# (* Construct a function add_end which adds integer el to the end of the list of integers xs *)
def add_end(el, l):
	return l + [el]

# (* Construct a function delete_one which deletes first instance of integer el from the list of integers xs *)
def delete_one(el, l):
	if l[0] == el:
		return l[1:]
	else:
		return [l[0]] + delete_one(el, l[1:])

# (* Construct a function delete_all which deletes all instances of integer el from the list of integers xs *)
def delete_all(el, l):
	if len(l) == []:
		return []
	elif el == l[0]:
		return delete_all(el, l[1:])
	else:
		return [l[0]] + delete_all(el, l[1:])


# (* Construct a function all_equal which returns true if all integers in the list of integers xs are equal and false otherwise *)
def all_equal(l):
	if len(l) in {0, 1}:
		return True
	else:
		if l[0] == l[1]:
			return all_equal(l[1:])
		else:
			return False

# (* Construct a function is_longer that checks if list of integers xs is longer than list of integers ys *)
def is_longer(xs, ys):
	if len(ys) == 0:
		if len(xs) > 0:
			return True
		else:
			return False
	else:
		if len(xs) > 0:
			return is_longer(xs[1:], ys[1:])
		else:
			return False


# (* Construct a function list_length which returns the length of the list of integers xs *)
def list_length(l):
	if l == []:
		return 0
	else:
		return 1 + list_length(l[1:])

# (* Construct a function expand which returns a list of n integers el *)
def expand(el, n):
	if n == 0:
		return []
	else:
		return [el] + expand(el, n-1)

# (* Construct a function sum_list which returns the sum of all the integers in the list xs *)
def sum_list(xs):
	if len(xs) == 0:
		return 0
	else:
		return xs[0] + sum_list(xs[1:])

# (* Construct a function reverse_list which reverses the list of integers l*)
def reverse_list(l):
	if len(l) == 0:
		return []
	elif len(l) == 1:
		return l
	else:
		return reverse_list(l[1:]) + [l[0]]

# (* Construct a function equal which returns true if the lists of integers l1 and l2 are equal *)
def equal(l1, l2):
	if l1 != [] and l2 != []:
		return l1[0] == l2[0] and equal(l1[1:], l2[1:])
	elif l1 == [] and l2 == []:
		return True
	else:
		return False


"""

(* Construct a function is_palindrome which returns true if the list of integers xs is a palindrome *)

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

"""