use "queue.sml";

(* Create an alias to a Queue. *)
structure Q = Queue

(* Define empty queue. *)
val my_queue = Q.empty

(* Enqueue some elements to queue. *)
val my_queue = Q.enqueue(1, my_queue);
val my_queue = Q.enqueue(2, my_queue);
val my_queue = Q.enqueue(3, my_queue);

(* Map function to every element in queue. *)
val my_queue = Q.map (fn x => 2*x) my_queue;

(* Get front element of queue. *)
val front_el = Q.front(my_queue);
(* Remove front element from queue *)
val my_queue = Q.dequeue(my_queue);