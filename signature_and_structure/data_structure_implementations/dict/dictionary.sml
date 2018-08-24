(* A very useful type in programming is the dictionary. A dictionary is a mapping from strings to other
values. A more general dictionary that maps from one arbitrary key type to another is usually called a map
or an associative array, although sometimes “dictionary” is used for these as well. In any case, the
implementation techniques are the same. Here's a signature for dictionaries: *)

signature DICTIONARY =
sig
	type key
	type 'a dict
	val make : unit -> 'a dict
	val insert : 'a dict -> key -> 'a -> 'a dict 	(* take dict, take key, take value, return dictionary. *)
	val lookup : 'a dict -> key -> 'a 				(* take dictironary, take key, return value mapped by the key. *)
	exception NotFound 								(* notFound: exception for signalling unknown key. *)
end

(* Dictionary implementation using a function *)
structure FunctionDict :> DICTIONARY where type key = string = 
struct
	type key = string 											(* The key is a string. *)
	type 'a dict = string -> 'a 								(* The dictionary is represented as a mapping from key to value *)
	exception NotFound 											(* Exception for signaling value that was not found. *)
	fun make () = fn _ => raise NotFound 						(* Make an empty dictionary. Accessing anything from an empty dictionary raises exception. *)
	fun lookup (d : 'a dict) (key : string) : 'a = d key 		(* lookup: take dictionary, take key, pass key to mapping function. *)
	fun insert (d : 'a dict) (k : key) (el : 'a) : 'a dict = 	(* take dictionary, take key, take value... *)
		fn k' => if k = k' then el else d k'					(* If k passed to function is equal to key then return passed element else... *)
end 															(* ...return reuslt of applying k' to d. *)

(* Dictionary implementation using an association list. *)
structure AssocList :> DICTIONARY =
struct
	type key = string 											(* The dictionary key is a string. *)
	type 'a dict = (key * 'a) list 								(* The dictionary is represented as a pair of key and value *)
	fun make () : 'a dict = [] 									(* Make an empty dictionary - return empty list *)
	fun insert (d : 'a dict) (k : key) (x : 'a) : 'a dict = 	(* take dictionary, take key, take element, put (key, value) pair in list representing the dictionary. *)
		(k, x)::d

	exception NotFound

	fun lookup (d : 'a dict) (k : key) : 'a =
		case d of
			[] => raise NotFound
		|	((k', x)::rest) => if k = k' then x
								else lookup rest k
end

structure SortedAssocList :> DICTIONARY =
	struct
		type key = string 											(* key is of type string *)
		type 'a dict = (key * 'a) list 								(* the dictionary is represented as a pair of a key and value. *)

		fun make () : 'a dict = [] 									(* Make an empty dictionary - return an empty list *)


		fun insert (d : 'a dict) (k : key) (x : 'a) : 'a dict = 	(* insert take a dictionary, take a key, take a value and return dictionary with inserted value. *)
			case d of
				[] => (k, x) :: nil 								(* Base case: list is empty - add pair to list. *)
			|	(k', x')::rest => 									(* pattern match current dictionary state. *)
					(case String.compare(k, k') of 					(* Compare key being inserted to head of current list. *)
						GREATER => (k', x')::(insert rest k x) 		(* If key is greater, keep head and insert in tail. *)
					|	EQUAL 	=> (k, x)::rest 						(* If equal, replace current head of list. *)
					|	LESS 	=> (k, x)::(k',x')::rest) 				(* If less, make pair new head. *)

		exception NotFound

		fun lookup (d: 'a dict) (k : key) : 'a = 					(* lookup: take a dictionary, take a key and return value mapped by the key. *)
			case d of 												
				[] => raise NotFound 								(* If list is empty, raise NotFound exception. *)
			|	((k', x)::rest) => 									(* Pattern match list representing the dictionary. *)
					(case String.compare(k, k') of 					(* Compare key with key at head of list. *)
						EQUAL 	=> x 									(* If key equal, return element mapped by the key. *)
					|	LESS 	=> raise NotFound 						(* If key less, then dictionary does not contain value - list is sorted ascending by keys. *)
					|	GREATER => lookup rest k) 					(* If key greater, look for it in tail of list. *)
	end

(* Dictionary implementation using a binary tree *)
structure AssocTree :> DICTIONARY =
struct
	(* The key is represented as a string *)
	type key = string
	(* The dictionary is represented by a tree *)
	datatype 'a dict = Empty | Node of {key : key, datum : 'a, left : 'a dict, right : 'a dict}
	fun make () : 'a dict = Empty

	(* insert: take dictionary, take key, take value, return dictionary with value added. *)
	fun insert (d : 'a dict) (k : key) (x : 'a) : 'a dict =
		case d of
			Empty => Node{key = k, datum = x, left = Empty, right = Empty} 					(* If dictionary is empty, add root node to tree. *)
		|	Node {key = k', datum = x', left = l, right = r} => 							(* Pattern match root of tree. *)
				(case String.compare(k, k') of 												(* If key in root is... *)
					EQUAL 	=> Node{key = k, datum = x, left = l, right = r} 				(* ...equal to key being inserted, then swap root. *)
				|	LESS 	=> Node{key=k', datum=x', left = insert l k x, right = r } 		(* ...less than key being inserted, add to left subtree. *)
				|	GREATER => Node{key = k', datum = x', left = l, right = insert r k x}) 	(* ...greater than key at root, add to right subtree. *)

	(* Exception for signalling a value that was not found. *)
	exception NotFound


	(* lookup: take dictionary, take key and return value being mapped by that key. *)
	fun lookup (d : 'a dict) (k : key) : 'a =
		case d of
			(* If tree is empty, raise NotFound exception. *)
			Empty => raise NotFound
			(* Else pattern match node and compare key with key in root. *)
		|	Node{key = k', datum = x, left = l, right = r} =>
				(case String.compare(k, k') of
					EQUAL 	=> x 								(* If key is equal, the value has been found. *)
				|	LESS 	=> lookup l k 						(* If key is less, perform recursive search in the left subtree. *)
				|	GREATER => lookup r k) 						(* If key is greater than key in root, perform recursive search in right subtree.  *)

end