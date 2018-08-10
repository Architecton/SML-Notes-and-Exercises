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

(* ADT that is distinct from the type of lists: *)

(* Use opaque ascription to hide implementation details. (Can not tell that stack implemented using list) *)
(* Opaque ascription could be used when defining the signature in stack.sml *)
(* : means transparent ascription *)

(*

Transparent ascription: this allows the definitions of types in the principal signature to "show through", so long as that type is declared (not necessarily defined) in the transc.
Opaque ascription: this gives the structure the ascribed signature --- and no more. Therefore, some information in the structure's types may be hidden.

*)

structure S2 = Stack :> STACK

val stack_example = S2.push(1, S2.empty);

val stack_example : int list = S2.push(1, stack_example);

(*
Our new stack type is now distinct from the type of lists. Thus, we have
managed to implement an ADT: First, we managed to contain the description
of the ADT in a “single syntactic unit”, namely our signature STACK. Secondly,
we have managed to provide an implementation of the stack that did not reveal
its inner workings, in our structure ListStack.
Signature constraints not only restrict types, but also eliminate definitions
that were not promised by the signature.
*)

