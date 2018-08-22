(* Make a reference and assign value 0 to memory. *)
val counter = ref 0;

(* Increment counter *)
counter := !counter + 1;

(* Make a list of references. *)
val l = [ref 0, ref 1, ref 2, ref 3]

(* get first element in list. *)
val tmp = List.nth(l, 1);

(* Increment value pointed to by the reference. *)
tmp := !tmp + 1;

(* Note that the value in the list has changed. *)
val again = List.nth(l, 1)