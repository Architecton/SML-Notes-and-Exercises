signature SET =
sig
    type element
    val compare : element * element -> order
    type set
    val empty : set
    val member : element -> set -> bool
    val add : element -> set -> set
    val remove : element -> set -> set
    val to_list : set -> element list
    val fold : ('a -> element -> 'a) -> 'a -> set -> 'a
end


(* množica celih števil, implementirana s seznamom *)
structure IntListSet : SET =
struct
    type element = int
    val compare = Int.compare

    type set = int list

    val empty = []

    fun member x [] = false
      | member x (y :: ys) = (compare (x,y) = EQUAL) orelse (member x ys)

    fun add x ys = if member x ys then ys else (x :: ys)

    fun remove x [] = []
      | remove x (y :: ys) =
        case compare (x, y)
         of EQUAL => ys
         | _ => y :: remove x ys

    fun to_list ys = ys

    fun fold f x [] = x
      | fold f x (y :: ys) = fold f (f x y) ys
end


(* množica elementov danega tipa, implementirana s seznamom *)
functor ListSet(type t
                  val compare : t * t -> order
                 ) : SET =
struct
  type element = t
  val compare = compare

  type set = element list

  val empty = []

  fun member x [] = false
    | member x (y :: ys) = (compare (x,y) = EQUAL) orelse (member x ys)

  fun add x ys = if member x ys then ys else (x :: ys)

  fun remove x [] = []
    | remove x (y :: ys) =
      case compare (x, y)
       of EQUAL => ys
       | _ => y :: remove x ys

  fun to_list ys = ys

  fun fold f x [] = x
    | fold f x (y :: ys) = fold f (f x y) ys
end


(* množica elementov danega tipa, implementirana z AVL drevesom *)
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


(* funktor, ki dani strukturi doda nekaj pogostih operacij nad množicami *)
functor SetOps(S : SET) =
struct
   fun union a b = S.fold (fn u => fn x => S.add x u) a b

   fun filter p a = S.fold (fn b => fn x => if p x then S.add x b else b) S.empty a

   fun intersection a b = filter (fn x => S.member x a) b
end

structure SA_ops = SetOps(SA)

(* SA.to_list (SA_ops.filter (fn x => x > 10) (add_avl 20)) *)
