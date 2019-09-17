use "stack.sml";

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

(* The simplest possible implementation for queues is to represent a queue with two stacks: 
one stack A on which to enqueue elements, and one stack B from which to deque elements.
When dequeuing, if stack B is empty, then we reverse stack A and consider it the new stack B. *)

structure Queue :> QUEUE = 
struct
	(* Create alias for stack *)
	structure S = Stack :> STACK;

	(* Create a type queue which is an alias to an ordered pair of two 'a stacks. *)
	type 'a queue = ('a S.stack * 'a S.stack)

	(* Exception for signaling acces to empty queue. *)
	exception EmptyQueue
	
	(* An empty queue is represented by a pair of empty stacks. *)
	val empty : 'a queue = (S.empty, S.empty)

	(* is_empty: check if this queue is empty. *)
	fun is_empty((s1, s2) : 'a queue) =
		S.is_empty(s1) andalso S.is_empty(s2) (* Check if both stacks are empty. *)

	(* enqueue: add element x to queue. *)
	fun enqueue(x : 'a, (s1, s2) : 'a queue) : 'a queue =
		(S.push(x, s1), s2) (* push element to first stack *)

	(* rev: reverse elements in a stack *)
	fun rev(s : 'a S.stack) : 'a S.stack =
		let
			(* loop: auxiliary function that pops elements from old stack and places them onto new stack. *)
			fun loop(old : 'a S.stack, new : 'a S.stack) : 'a S.stack =
				if (S.is_empty old) then new (* base case: if old stack is empty (all elements have been popped), return new stack. *)
				else loop (S.pop (old), S.push (S.top old, new)) (* recursive case: pop top element from old stack and push popped element to new stack. *)
		in
			loop(s, S.empty) (* Call loop with passed stack s as old stack and empty stack as new stack. *)
		end

	(* dequeue: dequeue next element from queue. *)
	fun dequeue ((s1, s2) : 'a queue) : 'a queue =
		(* If second stack is empty, then return pair of empty stack and reversed first stack without first element. *)
		if (S.is_empty s2) then (S.empty, S.pop(rev s1)) handle S.Empty => raise EmptyQueue
		else  (s1, S.pop (s2)) (* return pair first stack and second stack without top element. *)

	(* front: return element at the front of queue *)
	fun front ((s1, s2) : 'a queue) : 'a =
		(* If second stack is empty, return top of reversed first stack. *)
		if (S.is_empty s2) then S.top (rev s1) handle S.Empty => raise EmptyQueue
		else S.top s2 (* Else return top of second stack. *)

	(* map: apply function to every element in queue and return resulting queue. *)
	fun map (f : 'a -> 'b) ((s1, s2) : 'a queue) : 'b queue =
		(S.map f s1, S.map f s2)

	(* app: apply function f to every element in queue. *)
	fun app (f : 'a -> unit) ((s1, s2) : 'a queue) : unit =
		(S.app f s2;
		S.app f (rev (s1)))
end