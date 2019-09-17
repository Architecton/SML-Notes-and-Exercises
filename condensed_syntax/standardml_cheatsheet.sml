(* +, - and * are overloaded so they work for both int and real. *)
(* The same cannot be said for division which has separate operators: *)
val real_division = 14.0 / 4.0  (* gives 3.5 *)
val int_division  = 14 div 4    (* gives 3, rounding down *)
val int_remainder = 14 mod 4    (* gives 2, since 3*4 = 12 *)

val one_letter = #"a"        (* That funky syntax is just one character, a *)

val combined = "Hello " ^ "there, " ^ "fellow!\n"  (* Concatenate strings *)

val _ = print foo       (* You can print things. We are not interested in the *)
val _ = print combined  (* result of this computation, so we throw it away. *)
(* val _ = print one_letter *)  (* Only strings can be printed this way *)

(* List of characters *)
val bar = [ #"H", #"e", #"l", #"l", #"o" ]  (* SML also has lists! *)
(* val _ = print bar *)  (* Lists are unfortunately not the same as strings *)

(* Fortunately they can be converted.  String is a library and implode and size
   are functions available in that library that take strings as argument. *)
val bob = String.implode bar          (* gives "Hello" *)
val bob_char_count = String.size bob  (* gives 5 *)
(* Anonymous variable *)
val _ = print (bob ^ "\n")            (* For good measure, add a linebreak *)

(* Lists of the same kind can be appended using the @ ("append") operator *)
val guest_list = [ "Mom", "Dad" ] @ [ "Aunt", "Uncle" ]

(* Raising an exception *)
val x = 42
fun answer(question) =
    if question = "What is the meaning of life, the universe and everything?"
    then x
    else raise Fail "I'm an exception. Also, I don't know what the answer is."
val x = 43
val hmm = answer "What is the meaning of life, the universe and everything?"
(* Now, hmm has the value 42.  This is because the function answer refers to
   the copy of x that was visible before its own function definition. *)

(* Pattern matching is a funky part of functional programming.  It is an
   alternative to if-sentences.  The fibonacci function can be rewritten: *)
fun fibonacci 0 = 0  (* Base case *)
  | fibonacci 1 = 1  (* Base case *)
  | fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  (* Recursive case *)

(* Pattern matching is also possible on composite types like tuples, lists and
   records. Writing "fun solve2 (a, b, c) = ..." is in fact a pattern match on
   the one three-tuple solve2 takes as argument. Similarly, but less intuitively,
   you can match on a list consisting of elements in it (from the beginning of
   the list only). *)
fun first_elem (x::xs) = x
fun second_elem (x::y::xs) = y
fun evenly_positioned_elems (odd::even::xs) = even::evenly_positioned_elems xs
  | evenly_positioned_elems [odd] = []  (* Base case: throw away *)
  | evenly_positioned_elems []    = []  (* Base case *)
  
(* The case expression can also be used to pattern match and return a value *)
datatype temp =
      C of real
    | F of real

(* Create variable of type temp *)
val current_temp = C(21);
sc
fun temp_to_f t =
    case t of
      C x => x * (9.0 / 5.0) + 32.0
    | F x => x

(* When matching on records, you must use their slot names, and you must bind
   every slot in a record. The order of the slots doesn't matter though. *)

fun rgbToTup {r, g, b} = (r, g, b)    (* fn : {b:'a, g:'b, r:'c} -> 'c * 'b * 'a *)
fun mixRgbToTup {g, b, r} = (r, g, b) (* fn : {b:'a, g:'b, r:'c} -> 'c * 'b * 'a *)

(* If called with {r=0.1, g=0.2, b=0.3}, either of the above functions
   would return (0.1, 0.2, 0.3). But it would be a type error to call them
   with {r=0.1, g=0.2, b=0.3, a=0.4} *)

(* Higher order functions: Functions can take other functions as arguments.
   Functions are just other kinds of values, and functions don't need names
   to exist.  Functions without names are called "anonymous functions" or
   lambda expressions or closures (since they also have a lexical scope). *)
val is_large = (fn x => x > 37)
val add_them = fn (a,b) => a + b
val thermometer =
    fn temp => if temp < 37
               then "Cold"
               else if temp > 37
                    then "Warm"
                    else "Normal"

(* The following uses an anonymous function directly and gives "ColdWarm" *)
val some_result = (fn x => thermometer (x - 5) ^ thermometer (x + 5)) 37

(* Here is a higher-order function that works on lists (a list combinator) *)
(* map f l
       applies f to each element of l from left to right, 
       returning the list of results. *)
val readings = [ 34, 39, 37, 38, 35, 36, 37, 37, 37 ]  (* first an int list *)
val opinions = List.map thermometer readings (* gives [ "Cold", "Warm", ... ] *)

(* And here is another one for filtering lists *)
(* Apply is_large function on all elements in list *)
val warm_readings = List.filter is_large readings  (* gives [39, 38] *)

(* You can create your own higher-order functions, too.  Functions can also take
   several arguments by "currying" them. Syntax-wise this means adding spaces
   between function arguments instead of commas and surrounding parentheses. *)
fun map f [] = []
  | map f (x::xs) = f(x) :: map f xs

(* map has type ('a -> 'b) -> 'a list -> 'b list and is called polymorphic. *)
(* 'a is called a type variable. *)


(* We can declare functions as infix *)
val plus = add_them   (* plus is now equal to the same function as add_them *)
infix plus            (* plus is now an infix operator *)
val seven = 2 plus 5  (* seven is now bound to 7 *)

(* Functions can also be made infix before they are declared *)
infix minus
fun x minus y = x - y (* It becomes a little hard to see what's the argument *)
val four = 8 minus 4  (* four is now bound to 4 *)

(* An infix function/operator can be made prefix with 'op' *)
val n = op + (5, 5)   (* n is now 10 *)

(* 'op' is useful when combined with high order functions because they expect
   functions and not operators as arguments. Most operators are really just
   infix functions. *)
(* foldl f init [x1, x2, ..., xn]
       returns
       f(xn, ...f(x2, f(x1, init))...)
       or init if the list is empty. *)
val sum_of_numbers = foldl op+ 0 [1, 2, 3, 4, 5]


(* Datatypes are useful for creating both simple and complex structures *)
datatype color = Red | Green | Blue

(* Here is a function that takes one of these as argument *)
fun say(col) =
    if col = Red then "You are red!" else
    if col = Green then "You are green!" else
    if col = Blue then "You are blue!" else
    raise Fail "Unknown color"

val _ = print (say(Red) ^ "\n")

(* Datatypes are very often used in combination with pattern matching *)
fun say Red   = "You are red!"
  | say Green = "You are green!"
  | say Blue  = "You are blue!"

(* We did not include the match arm `say _ = raise Fail "Unknown color"`
because after specifying all three colors, the pattern is exhaustive
and redundancy is not permitted in pattern matching *)


(* Here is a binary tree datatype *)
datatype 'a btree = Leaf of 'a
                  | Node of 'a btree * 'a * 'a btree (* three-arg constructor *)

(* Here is a binary tree *)
val myTree = Node (Leaf 9, 8, Node (Leaf 3, 5, Leaf 7))

(* Drawing it, it might look something like...

           8
          / \
 leaf -> 9   5
            / \
   leaf -> 3   7 <- leaf
 *)

(* This function counts the sum of all the elements in a tree *)
fun count (Leaf n) = n
  | count (Node (leftTree, n, rightTree)) = count leftTree + n + count rightTree

val myTreeCount = count myTree  (* myTreeCount is now bound to 32 *)


(* Exceptions! *)
(* Exceptions can be raised/thrown using the reserved word 'raise' *)
fun calculate_interest(n) = if n < 0.0
                            then raise Domain
                            else n * 1.04

(* Exceptions can be caught using "handle" *)
val balance = calculate_interest ~180.0
              handle Domain => ~180.0    (* balance now has the value ~180.0 *)

(* Some exceptions carry extra information with them *)
(* Here are some examples of built-in exceptions *)
fun failing_function []    = raise Empty  (* used for empty lists *)
  | failing_function [x]   = raise Fail "This list is too short!"
  | failing_function [x,y] = raise Overflow  (* used for arithmetic *)
  | failing_function xs    = raise Fail "This list is too long!"

(* We can pattern match in 'handle' to make sure
   a specific exception was raised, or grab the message *)
val err_msg = failing_function [1,2] handle Fail _ => "Fail was raised"
                                          | Domain => "Domain was raised"
                                          | Empty  => "Empty was raised"
                                          | _      => "Unknown exception"

(* err_msg now has the value "Unknown exception" because Overflow isn't
   listed as one of the patterns -- thus, the catch-all pattern _ is used. *)

(* We can define our own exceptions like this *)
exception MyException
exception MyExceptionWithMessage of string
exception SyntaxError of string * (int * int)

(* File I/O! *)
(* Write a nice poem to a file *)
fun writePoem(filename) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file, "Roses are red,\nViolets are blue.\n")
        val _ = TextIO.output(file, "I have a gun.\nGet in the van.\n")
    in TextIO.closeOut(file)
    end

(* Read a nice poem from a file into a list of strings *)
fun readPoem(filename) =
    let val file = TextIO.openIn filename
        val poem = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") poem
    end

val _ = writePoem "roses.txt"
val test_poem = readPoem "roses.txt"  (* gives [ "Roses are red,",
                                                 "Violets are blue.",
                                                 "I have a gun.",
                                                 "Get in the van." ] *)

(* We can create references to data which can be updated *)
val counter = ref 0 (* Produce a reference with the ref function *)

(* Assign to a reference with the assignment operator *)
fun set_five reference = reference := 5

(* Read a reference with the dereference operator *)
fun equals_five reference = !reference = 5

(* We can use while loops for when recursion is messy *)
fun decrement_to_zero r = if !r < 0
                          then r := 0
                          else while !r >= 0 do r := !r - 1

(* This returns the unit value (in practical terms, nothing, a 0-tuple) *)

(* To allow returning a value, we can use the semicolon to sequence evaluations *)
fun decrement_ret x y = (x := !x - 1; y)

