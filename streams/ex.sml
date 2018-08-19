datatype 'a stream = Null | Cons of 'a * (unit -> 'a stream)

fun const(c : 'a) = Cons(c, fn() => const c)

exception Empty

fun hd(s : 'a stream) : 'a =
	case s of
		Null 	   => raise Empty
	|	Cons(h, _) => h

fun tl(s : 'a stream) : 'a stream =
	case s of
		Null 	   => raise Empty
	|	Cons(_, t) => t ()

fun map (s : 'a stream) (f : 'a -> 'b) : 'b stream =
	case s of
		Null 	   => Null
	|	Cons(h, t) => Cons(f h, fn () => map (t()) f)

fun take_n (n : int) (s : 'a stream) : 'a list =
	if n = 0 then []
	else
		case s of
			Null 	   => []
		|	Cons(h, t) => h :: take_n (n - 1) (t())

fun filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
	case s of
		Null 	   => Null
	|	Cons(h, t) => if f h then Cons(h, fn () => filter f (t()))
								else filter f (t())

val fs_ex : int stream = Cons(1, fn () => Cons(2, fn () => Cons(3, fn () => Cons(4, fn () => Null))))

fun nats (n : int) : int stream = Cons(n, fn () => nats(n + 1))

fun list_to_stream (l : 'a list) : 'a stream =
	case l of
		[] => Null
	|	x::xs => Cons(x, fn () => list_to_stream xs)

fun concat(s1 : 'a stream) (s2 : 'a stream) : 'a stream =
	case s1 of
		Null 	   => s2
	|	Cons(h, t) => Cons(h, fn () => concat (t()) s2)

fun fib(a : int, b : int) : int stream = Cons(a, fn () => fib(b, a + b))