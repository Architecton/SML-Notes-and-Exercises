(* Signature specifying the functionality of a fraction ADT *)
signature FRACTION = 
sig
	type fraction
	val make : int -> int -> fraction
	val numerator : fraction -> int
	val denominator : fraction -> int
	val toString : fraction -> string
	val toReal : fraction -> real
	val multiply : fraction -> fraction -> fraction
	val add : fraction -> fraction -> fraction
end


structure Fraction :> FRACTION = 
struct
	type fraction = {numerator : int, denominator : int}

	(* gcd: return greatest common denominator of integers x and y. Use Euclids algorithm.*)
	fun gcd (x : int) (y : int) : int =
		if (x = y) then x 					(* If values are equal, return x (one of them). *)
		else if x < y then gcd x (y - x)	(* if x is smaller, make recursive call. *)
		else gcd (x - y) y					(* else recursive call. *)

	(* Declare exception for signalling illegal denominator value *)
	exception BadDenominator

	(* make: make a fraction from nominator and denominator (integers). *)
	fun make (n : int) (d : int) : fraction =
		if (d <= 0) then raise BadDenominator (* Negative fraction should be represented by a negative numerator. *)
		else 									 
			let
				val g = gcd (abs n) (abs d) (* Compute gcd of numerator and denominator to simplify fraction. *)
				val n2 = n div g 			(* Simplify fraction. *)
				val d2 = d div g
			in
				if (d2 < 0) then {numerator = ~n2, denominator = ~d2}
				else {numerator = n2, denominator = d2}
			end

	(* Note  *)
	(* numerator: get numerator of fraction *)
	fun numerator (fr : fraction) : int =
		case fr of
			{numerator = n, denominator = _} => n

	(* denominator: get denominator of fraction *)
	fun denominator (fr : fraction) : int =
		case fr of
			{numerator = _, denominator = d} => d

	(* toString: return a string representation of a fraction *)
	fun toString (fr : fraction) : string =
		case fr of
			{numerator = n, denominator = d} => (Int.toString n) ^ "/" ^ (Int.toString d)

	(* toReal: Compute real number representation/approximation of a fraction *)
	fun toReal (fr : fraction) : real =
		case fr of
			{numerator = n, denominator = d} => (Real.fromInt n) / (Real.fromInt d)

	(* multiply: multiply fraction fr2 and fraction fr1 and return resulting fraction. *)
	fun multiply (fr1 : fraction) (fr2 : fraction) : fraction =
		case (fr1, fr2) of
			({numerator = n1, denominator = d1}, {numerator = n2, denominator = d2}) => make (n1 * d1) (n2 * d2) (* trivial *)

	(* add: add fraction fr1 to fraction fr2 and return  *)
	fun add (fr1 : fraction) (fr2 : fraction) : fraction =
		case (fr1, fr2) of
			({numerator = n1, denominator = d1}, {numerator = n2, denominator = d2}) => make (n1 * d2 + n2 * d1) (d1 * d2) (* standard procedure for adding fractions *)

end