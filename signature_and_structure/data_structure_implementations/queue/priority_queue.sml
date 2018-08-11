(* Signature specifying the priority queue ADT functionality *)
signature PRIORITY_QUEUE =
sig
    type 'a element 									(* type of element in queue *)
    val priority : 'a element -> int 					(* priority function : map from element to its priority *)
    type 'a queue 										(* type queue: type respresenting the queue *)
    val empty : 'a queue 								(* value representing the empty queue. *)
    exception EmptyQueue 								(* exception for signaling illegal operations on an empty queue *)
    val enqueue : 'a element -> 'a queue -> 'a queue 	(* put: put element in queue *)
    val front : 'a queue -> 'a element 					(* get: get next element from queue *)
    val dequeue : 'a queue -> 'a queue 					(* dequeue: return queue with element at front removed *)
end

(* structure implementing the PRIORITY_QUEUE interface *)
structure Queue :> PRIORITY_QUEUE where type 'a element = int * 'a =
struct

	(* exception for signaling illegal operations on an empty queue *)
	exception EmptyQueue

	(* Element of queue is an ordered pair where the first element is the value and the second is the priority. *)
	type 'a element = int * 'a
	
	(* priority: return priority value from queue element *)
	fun priority (x, y) = x

	(* Queue is internally represented as a list of element types. *)
	type 'a queue = 'a element list

	(* The empty queue is represented as an empty list. *)
	val empty = []

	(* put: put element x to queue. If putting to empty queue, priority need not be checked. *)
	fun enqueue x [] = [x]
	  | enqueue x (q as y :: ys) = (* If putting to non_empty queue *)
			case Int.compare (priority x, priority y) of (EQUAL | LESS) => x :: q (* Compare priority. If priority less or equal to first element in queue, make first element. *)
															 | GREATER  => y :: (enqueue x ys) (* else keep first element and make recursive call for tail. *)

	(* front: return element at front of queue. *)														 	
	fun front [] = raise EmptyQueue
	  | front (x :: xs) = x


	(* dequeue: remove front element from queue and return resulting queue. *)
	fun dequeue [] = raise EmptyQueue
	  | dequeue (x::xs) = xs
end

(* Example of use *)

(* Create alias to Queue structure *)
structure Q = Queue;

(* enqueue some elements *)
val my_queue = Q.enqueue (1, "alpha") Q.empty
val my_queue = Q.enqueue (3, "gamma") my_queue
val my_queue = Q.enqueue (2, "beta") my_queue

(* retrieve some elements *)
val next_element = Q.front my_queue
val my_queue = Q.dequeue my_queue
val next_next_element = Q.front my_queue
val my_queue = Q.dequeue my_queue