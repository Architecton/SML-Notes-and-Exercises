(* Define a function times that multiplies two numbers. *)
fun times (a, b) = a * b;

(* Make the function infix. *)
infix times;

(* Apply function. *)
12 times 6;

(* Functions can be defined infix before they are declared. *)
infix minus;

(* Note the function header syntax. *)
fun a minus b = a - b;

(* Apply function. *)
12 minus 9;

(* Infix functions can be made prefix with the op keyword/function *)
op times(12, 5);
op minus(5, 7);