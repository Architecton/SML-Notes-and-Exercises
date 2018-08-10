signature PRIORITY_QUEUE =
sig
    type element
    val priority : element -> int
    type queue
    val empty : queue
    val put : element -> queue -> queue
    val get : queue -> element option * queue
end

structure MyFirstQueue : PRIORITY_QUEUE =
struct
  type element = int * int
  fun priority (x, y) = x

  type queue = element list

  val empty = []

  fun put x [] = [x]
    | put x (q as y :: ys) =
      case Int.compare (priority x, priority y)
       of (EQUAL | LESS) => x :: q
        | GREATER  => y :: (put x ys)

  fun get [] = (NONE, [])
    | get (x :: xs) = (SOME x, xs)
end

(* Implementacija prioritetne vrste s seznami. To je funktor, ki
   sprejme tip elemetov in prioritetno funkcijo. *)
functor ListQueue (
    type t (* v vrsto bomo dajali elemente tipa t *)
    val priority : t -> int
  ) : PRIORITY_QUEUE =
struct
  type element = t
  val priority = priority

  type queue = element list

  val empty = []

  fun put x [] = [x]
    | put x (y :: ys) =
      case Int.compare (priority x, priority y)
       of (EQUAL | LESS) => x :: y :: ys
        | GREATER  => y :: (put x ys)

  fun get [] = (NONE, [])
    | get (x :: xs) = (SOME x, xs)
end

(* Naredimo prioritetno vrsto nizov, prioriteta je dolžina niza. *)
structure A =
  ListQueue(
      type t = string
      val priority = String.size
  )

(* Preizkus. *)
val example1 =
    A.get (A.put "limona" (A.put "jabolko" (A.put "banana" A.empty)))

(* Naredimo prioritetno vrsto parov števil. *)
structure PairsQueue =
  ListQueue(
      type t = string
      val priority = String.size
  )

(* Učinkovita implementacija z levičarskimi kopicami,
   glej https://en.wikipedia.org/wiki/Leftist_tree.
   Implementacija je abstraktna, ker uporabimo :>,
   vendar dodamo določilo, da je tip element enak tipu t.
 *)
functor LeftistHeapQueue (
    type t
    val priority : t -> int
  ) :> PRIORITY_QUEUE where type element = t =
struct
   type element = t
   val priority = priority

   datatype queue = Leaf | Node of int * element * queue * queue

   fun rank Leaf = 0
     | rank (Node (r, _, _, _)) = r

   fun node (x, a, b) =
       case Int.compare (rank a, rank b)
        of LESS => Node (1 + rank a, x, b, a)
         | (EQUAL | GREATER) => Node (1 + rank b, x, a, b)

   fun meld a b =
       case (a, b)
        of (_, Leaf) => a
         | (Leaf, _) => b
         | (Node (_, ka, la, ra), Node (_, kb, lb, rb)) =>
           (case Int.compare (priority ka, priority kb)
             of LESS => node (ka, la, meld ra b)
              | (EQUAL | GREATER) => node (kb, lb, meld a rb))

   fun singleton x = Node (1, x, Leaf, Leaf)

   val empty = Leaf

   fun put x q = meld q (singleton x)

   fun get Leaf = (NONE, Leaf)
     | get (Node (_, y, l, r)) = (SOME y, meld l r)
end


structure C = LeftistHeapQueue(type t = int * int
                              fun priority (x,_) = x)

val example2 =
    let fun loop q 0 = C.put (0, 0) q
          | loop q k = loop (C.put ((47 * k * k + 13) mod 1000, k) q) (k - 1)
    in loop C.empty 1000
    end
