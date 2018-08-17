(* Signature specifying functionality of a set *)
signature SET =
sig
	type element 							 (* element stored in set *)
	val compare : element * element -> order (* compare two set elements *)
	type set 								 (* type for set *)
	val empty : set 						 (* value of type set that represents an empty set *)
	val member : element -> set -> bool 	 (* is element a memeber of set? *)
	val add : element -> set -> set 		 (* add element to set *)
	val remove : element -> set -> set 		 (* remove element from set *)
	val to_list : set -> element list 		 (* make a list representation of this set *)
	val fold : ('a -> element -> 'a) -> 'a -> set -> 'a (* fold operation on elements *)
end


(* set of integers implemented using a list *)
structure IntListSet :> SET where type element = int =
struct
	type element = int 				(* set stores elements of type int *)
	val compare = Int.compare		(* The compare function is the Int.compare function. *)

	type set = int list 			(* the type representing the set is a list of integers. *)

	val empty = [] 					(* An empty set is represented with an empty list. *)

	fun member x [] = false			(* No element is a member of an empty set. *)
	  | member x (y :: ys) = (compare (x,y) = EQUAL) orelse (member x ys) (* Compare element to head and make disjuction with recursive
																		   call for tail. *)

	(* add: add element x to this set (list ys) *)
	fun add x ys = if member x ys then ys else (x :: ys) (* Set does not contain duplicates. *)

	(* remove: remove element x from this set. *)
	fun remove x [] = [] (* Removing anything from an empty set yields an empty set. *)
	  | remove x (y :: ys) = (* Pattern match non-empty set (its list representation). *)
			case compare (x, y) of (* Compare x (element to be removed) with head of list. *)
				EQUAL => ys 	   (* If equal, return tail. *)
			  |  	_ => y :: remove x ys (* If not equal, keep head and make recursive call for tail *)

	(* to_list: make a list representation of this set. *)
	fun to_list ys = ys (* Since set is represented as a list, return the representing list. *)

	(* fold: apply the fold operation on the elements of this set *)
	fun fold f x [] = x
	  | fold f x (y :: ys) = fold f (f x y) ys
end


(* Use functor to construct set implemented using a list with a custom type. *)
(* also pass comparisom function *)
functor ListSet(type t
				val compare : t * t -> order
				) :> SET where type element = t =
struct
	(* similar *)
	type element = t
	val compare = compare

	type set = element list

	val empty = []

	fun member x [] = false
	  | member x (y :: ys) = (compare (x,y) = EQUAL) orelse (member x ys)

	fun add x ys = if member x ys then ys else (x :: ys)

	fun remove x [] = []
	  | remove x (y :: ys) =
	case compare (x, y) of 
		EQUAL => ys
	  | 	_ => y :: remove x ys

	fun to_list ys = ys

	fun fold f x [] = x
	  | fold f x (y :: ys) = fold f (f x y) ys
end


(* Set of a given type implemented with an AVL tree. *)
functor AVLSet(type t
			   val compare: t * t -> order
			  ) : SET =
struct
  type element = t
  val compare = compare

  datatype set = Empty | Node of element * int * set * set

  val empty = Empty

  fun height Empty = 0
    | height (Node (_, h, _, _)) = h

  fun leaf v = Node (v, 1, Empty, Empty)

  fun node (v, l, r) = Node (v, 1 + Int.max (height l, height r), l, r)

  fun member x Empty = false
    | member x (Node (y, _, l, r)) =
      case compare (x, y)
       of EQUAL => true
        | LESS => member x l
        | GREATER => member x r

  fun to_list Empty = []
    | to_list (Node (x, _, l, r)) = to_list l @ [x] @ to_list r

  fun rotateLeft (Node (x, _, a, Node (y, _, b, c))) = node (y, node (x, a, b), c)
    | rotateLeft t = t

  (* alternativni zapis s case *)
  fun rotateRight t =
    case t
     of Node (y, _, Node (x, _, a, b), c) => node (x, a, node (y, b, c))
      | _ => t

  fun imbalance Empty = 0
    | imbalance (Node (_, _, l, r)) = height l - height r

  fun balance Empty = Empty
    | balance (t as Node (x, _, l, r)) =
      case imbalance t
       of 2 =>
          (case imbalance l
            of ~1 => rotateRight (node (x, rotateLeft l, r))
             | _ => rotateRight t)
        | ~2 =>
          (case imbalance r
            of 1 => rotateLeft (node (x, l, rotateRight r))
            |  _ => rotateLeft t)
        | _ => t

  fun add x Empty = leaf x
    | add x (t as (Node (y, _, l, r))) =
      case compare (x, y)
       of EQUAL => t
        | LESS => balance (node (y, add x l, r))
        | GREATER => balance (node (y, l, add x r))

  fun remove x Empty = Empty
    | remove x (Node (y, _, l, r)) =
      let fun removeSuccessor Empty = raise Fail "impossible"
            | removeSuccessor (Node (x, _, Empty, r)) = (r, x)
            | removeSuccessor (Node (x, _, l, r)) =
                        let val (l', y) = removeSuccessor l
                        in (balance (node (x, l', r)), y)
                        end
      in
          case compare (x, y)
           of LESS => balance (node (y, remove x l, r))
            | GREATER => balance (node (y, l, remove x r))
            | EQUAL =>
              case (l, r)
               of (_, Empty) => l
                | (Empty, _) => r
                | _ =>
                  let val (r', y') = removeSuccessor r
                  in balance (node (y', l, r'))
                  end
      end

  fun fold f x Empty = x
    | fold f x (Node (y, _, l, r)) = fold f (f (fold f x l) y) r
end

(* množici števil z uporabo seznamov oziroma AVL dreves *)
structure SL = ListSet (type t = int val compare = Int.compare)
structure SA = AVLSet (type t = int val compare = Int.compare)

fun add_list 0 = SL.empty
  | add_list n = SL.add n (add_list (n-1)) ;

fun add_avl 0 = SA.empty
  | add_avl n = SA.add n (add_avl (n-1)) ;

fun time f =
    let val timer = Timer.startRealTimer ()
    in
        f () ;
        Timer.checkRealTimer timer
    end

(* čas vstavljanja 100000 števil: *)
(* time (fn () => add_list 100000) *)
(* time (fn () => add_avl 100000) *)

(* a functor that adds to a set structure some commonly used set operations. It returns a structure containing these
additional functions. *)
functor SetOps(S : SET) =
struct
	(* union: compute union of sets a and b *)
	fun union a b = S.fold (fn u => fn x => S.add x u) a b (* the first set a is the initial state of the accumulator. *)

	(* filter: make a set that contains only the elements of set a for which the function p returns true. *)
	fun filter p a = S.fold (fn b => fn x => if p x then S.add x b else b) S.empty a (* if p returns true for x, add to accumulator. *)

	(* intersection: return a set of elements that are both in set a and in set b *)
	fun intersection a b = filter (fn x => S.member x a) b (* return set of elements of b for which are also members of a. *)
end

structure SA_ops = SetOps(SA)

(* SA.to_list (SA_ops.filter (fn x => x > 10) (add_avl 20)) *)
