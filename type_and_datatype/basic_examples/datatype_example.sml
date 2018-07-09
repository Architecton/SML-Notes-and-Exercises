(* Declare datatype complex with value constructor Complex *)
datatype complex = Complex of {re:real, im:real};

(* Define variable of complex type *)
val c = Complex{re=1.24, im = 2.47};

(* Functions for getting the real/complex part of the complex number *)
(* Notice the use of constructor *)
fun get_re(Complex{re, im}) = re
(* Optionally, the return type of the function can be explicitly stated *)
fun get_im(Complex{re, im}) : real = im;

(* Function for getting the absolute value of the complex number *)
open Math;
fun get_abs(Complex{re, im}) = sqrt(re*re + im*im);


(* Another way to write a function that returns the real part of the complex number *)
fun get_re2(c : complex) : real =
	case c of
		Complex {re, im} => re;