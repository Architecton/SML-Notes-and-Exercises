(* filter: take predicate and list and return list of elements for which predicate is true *)
fun filter pred [] = []
  | filter pred (x::xs) =
  	if pred x then x :: filter pred xs
  	else filter pred xs

(* Alternative Implementation *)
fun filter1(pred, l) : int list =
	if null l then []
	else if pred(hd l) then hd l :: filter1(pred, tl l)
	else filter1(pred, tl l)

(* ## Examples of use ## *)

val positive = filter (fn x => x >= 0) [1, 2, ~3, 6, ~22, 21]

(* Currying *)

val filt = filter (fn x => x < 0)

val negative = filt [1, 2, ~3, 6, ~22, 21]