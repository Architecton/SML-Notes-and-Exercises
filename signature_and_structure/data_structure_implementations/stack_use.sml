(* Example of using the stack data structure *)
use "stack.sml";

(* Introduce an alias to the structure Stack. *)
structure S = Stack;

(* Push 1 to an empty stack and save result to my_stack. *)
val my_stack : int S.stack = S.push(1, S.empty)

(* Push 2 to stack refered to by my_stack and save result to my_stack. *)
val my_stack = S.push(2, my_stack)

(* Push 3 to stack refered to by my_stack and save result to my_stack. *)
val my_stack = S.push(3, my_stack)

(* Get top element of stack refered to as my_stack. *)
val top_element = S.top(my_stack);

(* Remove top element from stack refered to as my_stack and save resulting stack in my_stack. *)
val my_stack = S.pop(my_stack);

(* Push 5 to stack refered to my_stack and save result to my_stack *)
val my_stack = S.push(5, my_stack);

(* Map function that doubles an integer to every element in stack refered to by my_stack. *)
val my_stack = S.map (fn x => 2 * x) my_stack