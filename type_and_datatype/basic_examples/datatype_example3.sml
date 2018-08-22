(* Declare datatype color with three constant constructors *)
datatype color = Red | Green | Blue

(* Pattern matching example : Construct a function for every constructor of color type *)
(* The first parenthesis are optional *)
fun translate Red = "Rot" |
	translate Green = "Grun" |
	translate Blue = "Blau"

(* Another way to implement pattern matching *)
fun encode(c : color) =
	case c of
			Red => 0
		|	Green => 1
		|	Blue => 2


(* Another basic example *)
(* Define datatype with two constructors *)
datatype temp = C of real | F of real


val current_temp = C(21.0)

(* temp_to_f: convert temperature in C or F to F *)
fun temp_to_f t =
	case t of
		  C x => x * (9.0 / 5.0) + 32.0
		| F x => x