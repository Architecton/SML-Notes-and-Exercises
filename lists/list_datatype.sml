datatype 'a list = nil | :: of 'a * 'a list

(* Bind two test lists. *)
val test_list : int list = 1::2::3::2::nil
val test_list2 : int list = nil

(* Implement hd, tl and null *)
fun hd nil     = raise Empty
  | hd (h::_) = h

fun tl nil = raise Empty
  | tl (_::t) = t

fun null nil = true
  | null _ = false

fun hd2 (xs : 'a list) : 'a =
  case xs of
       nil => raise Empty
    |  (h::_) => h

fun tl2 (xs : 'a list) : 'a list =
  case xs of
       nil => raise Empty
    |  (_::t) => t

fun null2 (xs : 'a list) : bool =
  case xs of
       nil => true
    |  _ => false


