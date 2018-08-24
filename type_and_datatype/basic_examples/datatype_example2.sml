datatype complex = Complex of (real * real);

val c = Complex(2.12, 7.19)

(* Functions that return the real/complex part of the complex number *)
fun get_re(Complex(re,_)) = re	
fun get_im(Complex(_,im)) = im

(* Another way to write the function that returns the real part of the complex number *)
fun get_re2(c : complex) =
	case c of
		Complex c => #1 c

(* Yet another way to write the function that returns the real part of the complex number *)
fun get_re3(c : complex) =
	case c of
		Complex(re, im) => re;