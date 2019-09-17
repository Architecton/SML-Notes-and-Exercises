(* Implementation of the left fold operation. This higher order function accepts a
function mapping an ordered pair of values (where the first element is the next element in the list
and the second element is the accumulator.) and returns a function that accepts an accumulator value.
That function then returns a function that accepts a list and returns the desired result. *)
fun foldl (f: 'a*'b->'b) (acc: 'b) (l: 'a list): 'b =
	case l of
		[] => acc (* Base case: list is empty - return accumulator *)
	  | x::xs => foldl f (f(x,acc)) xs (* Recursive case - pattern match head and tail of list 
	  and make a recursive call to foldl. Pass function f, modified accumulator and the tail of list. *)


(* Implementation of the right fold operation *)
(* Accepts a function and returns a function that accepts an accumolator and returns a function that accepts a list *)
fun foldr (f:'a*'b->'b) (acc:'b) (l:'a list):'b =
	case l of
		[] => acc (* Base case: list is empty - return accumulator *)
	  | x::xs => f(x, (foldr f acc xs)) (* Recursive case - apply function f to head and make
	  recursive call for accumulator *)

(* Notice also that foldl is tail recursive whereas foldr is not. 
Typically when given a choice between using the two functions, you should use foldl 
for performance.  However, in many cases using foldr is easier, 
as in the concat function above.  If you need to use foldr on a very lengthy list, 
you may instead want to reverse the list first and use foldl. *)

(* Define a function sum using the foldl operation. Pass a function that sums next list value into
accumulator and define starting accumulator value to be 0. *)
val sum = foldl (fn (x, a) => x + a) 0

(* Define a function sub, that subtracts elements of an int list from an accumulator that contains an initial
value os 100. *)
val sub = foldl (fn (el, acc) => acc - el) 100;

(* Define a function len that returns the length of a list. *)
fun len l = foldl (fn (_, acc) => acc + 1) 0 l

(* Define a function rev that reverses a list *)
(* List.:: is a list constructor. It takes an ordered pair ('Z, 'Z list) *)
(* rev takes an empty list as a starting accumulator and gradually constructs a reversed list using
the accumulator as the second element in the ordered pair of the :: constructor. *)
fun rev l = foldl List.:: [] l