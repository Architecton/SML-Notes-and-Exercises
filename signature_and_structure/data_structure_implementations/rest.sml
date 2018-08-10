signature PRIORITY_QUEUE =
sig
  type element
  val priority : element -> int
  type queue
  val empty : queue
  val enqueue : element -> queue -> queue
  val dequeue : queue -> queue
  val front : queue -> element
end

(* Implementation of a queue using lists. 
The functor takes type of element held by queue and a priority function*)
functor ListQueue (
  type t (* The queue will contain elements of type t *)
  val priority : t -> int (* function that takes an element and returns priority of element *)
) :> PRIORITY_QUEUE where type element = t =
struct
  (* The type of element stored in queue is t *)
  type element = t
  (* The priority function is priority *)
  val priority = priority

  (* queue is represented as a list of elements. *)
  type queue = element list

  (* Empty queue is represented as an empty list. *)
  val empty = []

  (* Exception for signaling illegal operations on an empty queue *)
  exception EmptyQueue

  (* put: put element in queue *)
  fun enqueue x [] = [x] (* If putting element to empty queue, priority need not be checked. *)
    | enqueue x (y :: ys) =
        case Int.compare (priority x, priority y) of (EQUAL | LESS) => x :: y :: ys (* If priority greater or equal to first element, add to front. *)
                                                         | GREATER  => y :: (enqueue x ys) (* Else keep front element and make recursive call for tail. *)
  (* front: get element at front of queue. *)
  fun front [] = raise EmptyQueue
    | front (x :: xs) = x

  (* dequeue: remove element at front of queue and return resulting element. *)
  fun dequeue [] = raise EmptyQueue
    | dequeue (x::xs) = xs
end

(* Make a priority queue of strings where priority is represented by the length of the string. *)
structure Q = ListQueue(
  type t = string
  val priority = String.size
)

val my_queue = Q.empty;
val my_queue = Q.enqueue "alpha" my_queue
val my_queue = Q.enqueue "beta" my_queue

val next_element = Q.front my_queue
val my_queue = Q.dequeue my_queue