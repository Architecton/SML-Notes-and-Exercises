(* Declare a function that doubles an element in a list of length 1. *)
(* Raise exceptions if list is empty or too long. *)
fun some_function [] = raise Empty
  | some_function [x] = [2 * x]
  | some_function xs = raise Fail "List is too long!"

(* Use some_function in another function and handle exceptions. *)
fun double_list xs = some_function xs
					handle Empty  => []
						|  Fail _ => []

