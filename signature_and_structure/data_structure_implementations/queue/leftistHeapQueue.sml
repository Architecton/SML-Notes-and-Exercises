signature PRIORITY_QUEUE =
sig
    type element
    val priority : element -> int
    type queue
    val empty : queue
    val put : element -> queue -> queue
    val get : queue -> element option * queue
end


(* Priority queue implemented using leftish heaps *)
functor LeftistHeapQueue (
	type t (* The queue stores contains elements of type t *)
	val priority : t -> int (* function that maps from t to priority *)
) :> PRIORITY_QUEUE where type element = t = (* make type element transparent *)
struct
	(* type element stored in heap is an alias to type t. *)
	type element = t
	(* priority function that maps from element to its priority. *)
	val priority = priority

	(* The datatype representing the queue is eaither a Leaf or a Node represented as a quadruplet of rank, element and child nodes/leaves. *)
	datatype queue = Leaf | Node of int * element * queue * queue

	(* Get rank of Node or leaf. Leaf has rank of 0 and Node rank equal to first element in quadruplet *)
	fun rank Leaf = 0
	  | rank (Node (r, _, _, _)) = r

	(* node: create Node with element el and children a and b *)
	fun node(el, a, b) =
		case Int.compare (rank a, rank b) of (* Compare rank of Nodes/Leaves a and b *)
						LESS => Node (1 + rank a, el, b, a) (* If rank of a is smaller than that of b, swap. *)
		|  (EQUAL | GREATER) => Node (1 + rank b, el, a, b) (* Else b has shorter or equal null path length. Keep order. *)

	(* meld: merge two heaps a and b *)
	fun meld a b =
		case (a, b) of
											   (_, Leaf) => a (* If right heap is Leaf, return left heap *)
		| 									   (Leaf, _) => b (* If left heap is Leaf, return right heap *)
		| 	(Node (_, ka, la, ra), Node (_, kb, lb, rb)) => (case Int.compare (priority ka, priority kb) of (* Compare priorities in left and right nodes being compared *)
																LESS => node (ka, la, meld ra b) (* min priority is in left heap. Recursively merge right subtree of left heap with right heap *)
												 | (EQUAL | GREATER) => node (kb, lb, meld a rb)) (* Else if greater or equal to element in queue, recursively merge
												 													left heap with right subtree of right heap. *)

	(* singleton: make a singleton node with rank 1, element el and two child leaves. *)
	fun singleton el = Node (1, el, Leaf, Leaf)

	(* the empty queue is represented by a single leaf. *)
	val empty = Leaf

	(* put: put element el int queue q. *)
	fun put el q = meld q (singleton el)

	(* get: get element at front of queue. Return ordered pair (next element, resulting queue with front element removed) *)
	fun get Leaf = (NONE, Leaf)
		| get (Node (_, y, l, r)) = (SOME y, meld l r)
end

(* Make a leftishHeapQueue structure that stores and ordered pair of integers. Let the first element represent the priority. *)
structure C = LeftistHeapQueue(type t = int * int
							   fun priority (x,_) = x)

(* Example of use *)

(* Create empty queue. *)
val my_queue = C.empty;

(* Add some elements to queue *)
val my_queue = C.put (1, 1) my_queue
val my_queue = C.put (2, 0) my_queue
val my_queue = C.put (0, 19) my_queue

(* Get next element from queue. *)
val (next_element, my_queue) = (fn (el, q) => (valOf(el), q)) (C.get(my_queue))
(* Get queue that is the result of removing the front element. *)
val my_queue = (fn (_, q) => q) (C.get(my_queue))

(* similar *)
val next_next_element = (fn (el, _) => valOf(el)) (C.get(my_queue))
val my_queue = (fn (_, q) => q) (C.get(my_queue))