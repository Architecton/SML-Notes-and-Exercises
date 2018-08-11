signature STACK = 
sig
	type 'a stack
	exception Empty
	val empty : 'a stack
	val is_empty : 'a stack -> bool
	val push : ('a * 'a stack) -> 'a stack
	val pop : 'a stack -> 'a stack
	val top : 'a stack -> 'a
	val map : ('a -> 'b) -> 'a stack -> 'b stack
	val app :  ('a -> unit) -> 'a stack -> unit
	(* note: app traverses from top of stack down *)
end    


structure Stack : STACK =
struct
	
	(* Define type stack (using a list implementation) *)
	type 'a stack = 'a list

	(* Define exception for signaling accessing an empty stack *)
	exception Empty
	
	(* Define value empty that signals an empty stack. *)
	val empty : 'a stack = []
	
	(* isEmpty: check if stack is empty *)
	fun is_empty (s : 'a list) : bool =
		(* If passed list is empty return true, else return false. *)
		case s of
				[] => true
			| 	_  => false

	(* push: push element x to stack *)
	(* Takes element x and l representing a stack and returns a new stack containing this element *)
	fun push (el : 'a, s : 'a stack) : 'a stack = el::s


	(* pop: remove top element from stack. Return stack with the top element removed. 
	Raise empty exception if passed empty stack. *)
	fun pop (s : 'a stack) : 'a stack =
		case s of
		[] => raise Empty
		| (x::xs) => xs

	(* top: return top element from stack. Raise Empty exception if stack is empty. *)
	fun top (s : 'a stack) : 'a =
		case s of
			[] 		=> raise Empty
		| 	(x::xs) => x

	(* map: apply function f to all elements in stack and return the new stack. *)
	fun map (f : 'a -> 'b) (s : 'a stack) : 'b stack = List.map f s

	(* app: apply function f to every element in stack. *)
	fun app (f : 'a -> unit) (s : 'a stack) : unit = List.app f s
end