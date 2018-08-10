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


structure Queue =
struct
	structure S = Stack;
	type 'a queue = 'a S.stack * 'a S.stack
	exception EmptyQueue
	val empty = (S.empty, S.empty)

	fun is_empty((s1, s2) : 'a queue) : bool =
		if S.is_empty s1 andalso S.is_empty s2 then true
		else false

	fun enqueue(el : 'a, (s1, s2) : 'a queue) : 'a queue =
		(S.push(el, s1), s2)

	fun rev(s : 'a S.stack) : 'a S.stack =
		let 
			fun loop(old : 'a S.stack, new : 'a S.stack) : 'a S.stack =
			if S.is_empty old then new
			else loop(S.pop old, S.push(S.top(old), new))
		in
			loop(s, S.empty)
		end

	fun dequeue((s1, s2) : 'a queue) : 'a queue =
		if S.is_empty s2 then (S.empty, S.pop(rev s1))
		else (s1, S.pop(s2))

	fun front((s1, s2) : 'a queue) : 'a =
		if S.is_empty s2 then S.top(rev s1)
		else S.top s2


	fun map(f : 'a -> 'b) ((s1, s2) : 'a queue) : 'b queue =
		(S.map f s1, S.map f s2)

	fun app(f : 'a -> unit) ((s1, s2) : 'a queue) : unit =
		(S.app f s2; S.app f (rev s1))

end