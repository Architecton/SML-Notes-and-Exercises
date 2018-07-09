(* Declare datatype color with three constant constructors *)
datatype color = Red | Green | Blue;

(* Pattern matching example : Construct a function for every constructor of color type *)
(* The first parenthesis are optional *)
fun translate(Red) = "Rot" |
	translate(Green) = "Grun" |
	translate(Blue) = "Blau"

(* Another way to implement pattern matching *)
fun encode(c : color) =
	case c of
			Red => 0
		|	Green => 1
		|	Blue => 2