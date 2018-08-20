(* datatype 'a node = Null | Node of 'a * 'a node *)

(* Signature specifying linked list functionality *)
signature LINKED_LIST = 
sig
	type 'a node
	type 'a linked_list
	val empty : 'a linked_list
	exception EmptyLinkedList
	val is_empty : 'a linked_list -> bool
	val get : int -> 'a linked_list -> 'a
	val remove : int -> 'a linked_list -> 'a linked_list
	val add : int -> 'a -> 'a linked_list -> 'a linked_list
	val add_end : 'a -> 'a linked_list -> 'a linked_list
	val add_start : 'a -> 'a linked_list -> 'a linked_list
	val index_of : ''a -> ''a linked_list -> int
	val get_first : 'a linked_list -> 'a
	val get_last : 'a linked_list -> 'a
	val to_list : 'a linked_list -> 'a list
	val foldl : ('a * 'b -> 'b) -> 'b -> 'a linked_list -> 'b
	val foldr : ('a * 'b -> 'b) -> 'b -> 'a linked_list -> 'b
	val map : ('a -> 'b) -> 'a linked_list -> 'b linked_list
	val app : ('a -> unit) -> 'a linked_list -> unit
end

(* structure implementing the LINKED_LIST signature *)
structure LinkedList :> LINKED_LIST = 
struct
	datatype 'a node = Null | Node of 'a * 'a node (* node is either Null or an ordered pair of the node element and the next node *)
	type 'a linked_list = 'a node (* linked_list is an alias to a node (starting node of the linked list) *)
	val empty : 'a linked_list = Null (* Empty linked list is represented as a Null node. *)
	exception EmptyLinkedList (* Exception for signalling illegal operations on an empty linked list *)

	(* is_empty: return true if linked list l is empty and false otherwise. *)
	fun is_empty (l : 'a linked_list) : bool =
		case l of
			Null => true		(* Linked list is empty if starting node is empty. *)
		|	Node(_, _) => false

	(* get: get the nth element in the linked list l *)
	fun get (n : int) (l : 'a linked_list) : 'a =
		if n < 0 then raise Fail "Illegal index" (* If passed negative index, raise exception. *)
		else
			case l of
				Node(el, next) => if n = 0 then el else get (n - 1) next (* If getting head element return el, else make recursive call for tail. *)
			|	Null => raise Fail "Illegal index" (* If index is too large or linked list empty, raise exception. *)

	(* remove: remove the n-th element from linked list l and return the resulting linked list *)
	fun remove (n : int) (l : 'a linked_list) : 'a linked_list =
		if n < 0 then raise Fail "Illegal index" (* If index is negative raise exception. *)
		else
			case l of
				Null => raise Fail "Illegal index" (* If index is too large or linked list is empty *)
			|	Node(el, next) => if n = 0 then next (* If removing head, return next node. *)
									else Node(el, remove (n - 1) next) (* recursive call for tail of linked list *)

	(* add: add element v to position n in linked list l *)
	fun add (n : int ) (v : 'a) (l : 'a linked_list) : 'a linked_list =
		if n < 0 then raise Fail "Illegal index" (* If passed negative index, raise exception. *)
		else
			case l of
				Null => raise Fail "Illegal index" (* If index too large or linked list empty, raise exception. *)
			|	Node(el, next) => if n = 0 then Node(v, l) (* If adding to start, add Node and make next l. *)
								  else Node(el, add (n - 1) v next) (* Else make recursive call for next element. *)

	(* add_end: add element v to end of linked list l *)
	fun add_end (v : 'a) (l : 'a linked_list) : 'a linked_list =
		case l of
			Null => Node(v, Null) (* If linked list is empty, make first node. *)
		|	Node(el, next) => Node(el, add_end v next) (* Else make recursive call for tail of linked list. *)


	(* add_start: add element v to start of linked list l *)
	fun add_start (v : 'a) (l : 'a linked_list) : 'a linked_list =
		Node(v, l) (* Make new node with tail l. *)


	(* index_of: find index of first occurence of element v. *)
	fun index_of (v : ''a) (l : ''a linked_list) : int =
		case l of
			Null => raise Fail "Element not found" (* If linked list empty or element not found. *)
		|	Node(el, next) => if el = v then 0 else 1 + index_of v next (* If element at head return 0 else count node and make recursive call
																		   for tair *)

	(* get_first: get first element in linked list l *)
	fun get_first (l : 'a linked_list) : 'a =
		case l of
			Null => raise EmptyLinkedList (* If l is empty, raise exception. *)
		|	Node(el, next) => el (* Else return element of head node. *)

	(* get_last: get last element in linked list l *)
	fun get_last (l : 'a linked_list) : 'a =
		case l of
			Null => raise EmptyLinkedList (* If linked list is empty, raise exception. *)
		|	Node(el, next) => case next of (* Else, if next is null, return el, else make recursive call for tail. *)
										 Null => el 
							  | Node(_, next) => get_last next

	(* to_list: make a list representation of linked list l. *)
	fun to_list (l : 'a linked_list) : 'a list =
		case l of
			Null => [] 								(* If l is empty, return empty list. *)
		|	Node(el, next) => el :: to_list next	(* Else concatenate element to result of recursive call. *)


	(* foldl: left fold operation (see fold_examples.sml) *)
	fun foldl (f : 'a * 'b -> 'b) (acc : 'b) (l : 'a linked_list) : 'b =
		case l of
			Null => acc
		|	Node(el, next) => foldl f (f(el, acc)) next

	(* foldr: right fold operation (see fold_examples.sml) *)
	fun foldr (f : 'a * 'b -> 'b) (acc : 'b) (l : 'a linked_list) : 'b =
		case l of
			Null => acc
		|	Node(el, next) => f(el, foldr f acc next)


	(* map: apply function f to every element in linked list l and return resulting linked list *)
	fun map (f : 'a -> 'b) (l : 'a linked_list) : 'b linked_list =
		case l of
			Null => Null (* If empty list return empty list. *)
		|	Node(el, next) => Node(f el, map f next) (* Else apply function to element and make recursive call for next node. *)

	(* Apply function to every element in linked list. *)
	fun app (f : 'a -> unit) (l : 'a linked_list) : unit =
		case l of
			Null => () (* If linked list is empty return unit. *)
		|	Node(el, next) => (f el; 		(* Else apply function to head element and make recursive call for tail of linked list. *)
							   app f next)

end