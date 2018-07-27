(* References in ML:

  -- An ML reference is a value which is the address of a location 
     in the memory. 
  -- The value in the location is the content. 
  -- The content can be read through the dereferenceing operation and
     changed through an assignment operation. *)


(* [1] Creating a reference 

  Using the constructor: ref
  
    -- Ref is an ML function.
    -- It expects an argument, the initial content of the reference.
    -- It returns a new address every time it is called. 
*)

(* Create a reference and assign value 0 to memory *)
val counter = ref 0;

(* [2] Dereferencing

  Using the function: !

    -- When ! is applied to a reference, it returns its contents. *)


(* Normally, ! is a function 'a ref -> 'a, which extracts the value from a reference cell. *)

(* [3] Updating

  Using the assignment operator: := 
  
    -- := operation updates the value of a reference.
    -- The type of := is unit. *)

(* Increment counter. Note use of dereferencing and the assignment operator *)
counter := !counter + 1;


(* Example of creating and manipulating a list of references. *)


(* Create a list of references to integers. *)
val list_of_references = [ref 0, ref 1, ref 2, ref 3];

(* Extract a reference from list. *)
val temp = List.nth (list_of_references, 1);

(* Increment value pointed to by the reference. Note that the value in the list also changes. *)
temp := !temp + 1;