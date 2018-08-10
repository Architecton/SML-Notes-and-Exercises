(* Signature specifying the functionality of the queue ADT *)
signature QUEUE = 
sig
	type 'a queue
	exception EmptyQueue
	val empty : 'a queue
	val is_empty : 'a queue -> bool
	val enqueue : ('a * 'a queue) -> 'a queue
	val dequeue : 'a queue -> 'a queue
	val front : 'a queue -> 'a
	val map : ('a -> 'b) -> 'a queue -> 'b queue
	val app : ('a -> unit) -> 'a queue -> unit
end

(* Structure implementing the QUEUE signature *)
structure Queue :> QUEUE =
struct
	(* queue is implemented using a list. *)
	type 'a queue = 'a list

	(* Exception for signaling illegal operations on an empty queue. *)
	exception EmptyQueue

	(* An empty queue is internally represented as an empty list. *)
	val empty : 'a list = []
	
	(* is_empty: return true if this queue is empty and false otherwise. *)
	fun is_empty(q : 'a queue) : bool =
		if null q then true
		else false

	(* enqueue: add element el to queue q and return resulting queue. *)
	fun enqueue(el : 'a, q : 'a queue) : 'a queue =
		let
			(* Auxilliary function that adds element el to end of list l *)
			fun add_to_end(el : 'a, l : 'a list) : 'a list =
				case l of
					[] => [el] (* base case: adding to empty list *)
				|	x::xs => x::add_to_end(el, xs) (* Recursive call - keep head and make recursive call for tail of list. *)
		in
			case q of
				[] => [el] (* if adding to empty list. *)
			|	l  => add_to_end(el, q) (* If adding to non empty list, use auxiliary function add_to_end. *)
		end

	(* dequeue: remove next element from queue and return resulting queue. *)
	fun dequeue(q : 'a queue) : 'a queue =
		case q of
			[] => raise EmptyQueue
		|	x::xs => xs

	(* front: get element at first position in queue. *)
	fun front(q : 'a queue) : 'a =
		case q of
			[] 	  => raise EmptyQueue
		|	x::xs => x

	(* map: apply function f to every element in queue and return resulting queue. *)
	fun map(f : 'a -> 'b) (q : 'a queue) : 'b queue =
		List.map f q

	(* app: apply function to elements of queue. *)
	fun app(f : 'a -> unit) (q : 'a queue) : unit =
		List.app f q
end


(* Use example: *)

(* Create alias to Queue *)
structure Q = Queue

(* Enqueue some elements. *)
val my_queue = Q.enqueue(1, Q.empty)
val my_queue = Q.enqueue(2, my_queue)
val my_queue = Q.enqueue(3, my_queue)

(* Map a function that doubles each element to my_queue *)
val my_queue = Q.map (fn x => 2*x) my_queue

(* Take elements from queue. *)
val next_el = Q.front my_queue
val my_queue = Q.dequeue my_queue

val next_next_el = Q.front my_queue
val my_queue = Q.dequeue my_queue

val next_next_next_el = Q.front my_queue
val my_queue = Q.dequeue my_queue

val next_next_next_next_el = Q.front my_queue handle Q.EmptyQueue => ~1