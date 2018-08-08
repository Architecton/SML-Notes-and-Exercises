datatype 'a stream = Null | Cons of 'a * (unit -> 'a stream)

fun const(c : 'a) : 'a stream = Cons(c, fn () => const c);

fun nats(n : int) : int stream = Cons(n, fn() => nats(n + 1));

fun fib(a : int, b : int) : int stream = Cons(a, fn () => fib(b, a + b));

exception Empty

fun hd(s : 'a stream) : 'a =
	case s of
		Null => raise Empty
	|	Cons(h, _) => h

fun tl(s : 'a stream) : 'a stream =
	case s of
		Null => raise Empty
	|	Cons(h, t) => t ()

fun map (f : 'a -> 'b) (s : 'a stream) : 'b stream =
	case s of
		Null => Null
	|	Cons(h, t) => Cons(f h, fn () => map f (t()))


fun take_n(s : 'a stream, n : int) : 'a list =
	case (s, n) of
		(_, 0) => []
	|	(Null, _) => raise Empty
	|	(Cons(h, t), n) => h :: take_n(t(), n - 1)

fun filter(f : 'a -> bool) (s : 'a stream) : 'a stream =
	case s of
		Null => Null
	|	Cons(h, t) => if f h then Cons(h, fn () => filter f (t()))
							else filter f (t())

fun list_to_stream(l : 'a list) : 'a stream =
	case l of
		[] => Null
	|	h::t => Cons(h, fn () => list_to_stream t)

fun concat(s1 : 'a stream) (s2 : 'a stream) : 'a stream =
	case s1 of
		Null => s2
	|	Cons(h, t) => Cons(h, fn() => concat (t()) s2)

fun sift (p : int) (s : int stream) : int stream =
	filter (fn n => n mod p <> 0) s

fun sieve (s : int stream) : int stream =
	case s of
		Null => Null
	|	Cons(h, t) => Cons(h, fn () => sift h (t()))