(* Define two custom exceptions. First without a type constructor (with a nullary type constructor) and second with constructor. *)
exception MyException
exception MyExceptionWithMessage of string

(* Declare a function for testing the two exceptions. *)
fun some_function 0 = raise MyException
  |	some_function 42 = raise MyExceptionWithMessage "The value 42 is not allowed"
  | some_function x = 2 * x