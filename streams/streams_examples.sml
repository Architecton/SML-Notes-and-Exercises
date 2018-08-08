(*
Another drawback of these two datastructures is their finiteness. This statement might sound surprising
initially, since any datastructure that we want to represent must be finite for obvious reasons. There are
applications, however, in which it is more convenient to structure the computation assuming that we
have one or more infinite datasources, from which we can extract as much data as we need for the
problem at hand. If we are looking, for example, for the smallest prime number with a particular property,
we might not know how far in the infinite sequence of ordered prime numbers we need to search to find
it. In such a situation, it might make sense to conceive of our computation as using a potentially infinite
datasource that produces prime numbers on demand. A stream is such a datasource.

We stress that "infinite" and "infinity" refer to potential (theoretical) infinity, and not actual infinity. Due to
the obvious limitations of various resources (time, memory, and others), no computation will produce an
infinite sequence of values. Our streams are infinite in potential, not in fact.

Before we define the stream datatype, let us think for a minute about how we could specify an infinite
stream of values. It is immediately clear that we can not actually enumerate these values, so we are left
with the alternative of providing a method for computing them. This means that our streams must rely on
an infinity of function calls. This is only possible if (at least some of) the functions involved are directly or
indirectly recursive. Now, the infinite sequence of function calls can not run to the "end" (there is no end
of infinity, of course); at any given time, only a finite number of such calls must have been initiated. Thus
me must have a mechanism for (temporarily) stopping further recursive calls. If we understand how we
can suspend, and later resume, the sequence of computations that generates the stream values, then
we can write streams in SML.
*)

(* Let us consider the following definition *)
datatype 'a stream = Null | Cons of 'a * (unit -> 'a stream)

(* We see that a stream can be either empty (Null), or it can consist of a pait. The first element of the pair is
a value (the head of the stream), while the second element of the pair is is a 0-argument function that
produces a stream. The stream that the 0-argument function returns is the tail (the rest) of our
original stream. *)

(* Let us now define the simplest stream - an infinite stream that consists of the same repeated value,
ad infinitum: *)
fun const(c: 'a) = Cons (c, fn() => const c)

(* Examples of using this stream *)

(* Define an exception for signaling an empty stream *)
exception Empty

(* hd: Returns the first element of a stream. *)
fun hd(s: 'a stream): 'a =
	case s of
		Null 		=> raise Empty (* If stream is empty raise Empty exception. *)
	|   Cons(h, _) 	=> h (* Return head of stream *)


(* tl: Returns the stream that results after removing the first element. *)
fun tl(s: 'a stream): 'a stream =
	case s of
		Null 		=> raise Empty (* If stream is empty, raise Empty exception. *)
	|   Cons(_, t) 	=> t () (* Return tail of list. Pass unit to t function. *)

(* map: Applies a function to every element of a stream. *)
fun map (f: 'a -> 'b) (s: 'a stream): 'b stream =
	case s of
		Null 		=> Null (* If stream empty return empty *)
	|   Cons(h, t) 	=> Cons(f h, fn () => map f (t ())) 
  (* Return new stream constructor where function
  is applied to head of stream and to rest of stream. Remember - t () returns a new stream*)

(* Returns the ordered list of the first n elements of the stream. *)
fun take_n(s: 'a stream, n: int): 'a list =
	case (s, n) of
		(_, 0) 			=> [] (* If taking 0 elements return empty list. *)
	|   (Null, _) 		=> raise Empty (* If stream is empty raise Empty exception *)
	|   (Cons(h, t), n) => h :: (take_n (t(), n - 1)) (* Concatenate head to list returned by recursive call. *)

(* Filter: Produces a stream of values that satisfy a predicate. *)
fun filter (f: 'a -> bool) (s: 'a stream) : 'a stream =
	case s of
		Null 		=> Null (* If stream is empty return Null. *)
	| 	Cons(h, t) 	=> if f(h) then Cons(h, fn () => filter f (t())) (* If head value satisfies condition, add to head of stream. Make recursive call for tail of stream. *)
					  			else filter f (t()) (* Else apply filter to tail of stream. *)

(* Creating finite streams *)
val fs_ex : int stream = Cons(9, fn () => Cons(8, fn () => Cons(7, fn () => Null)));

(* list_to_stream: Produces a finite stream containing elements of passed list. *)
fun list_to_stream(l : 'a list) : 'a stream =
	case l of
		[] 		=> Null
	|	h::t 	=> Cons(h, fn () => list_to_stream t)

(* concat: concatenate streams s1 and s2. *)
fun concat(s1 : 'a stream, s2 : 'a stream) : 'a stream =
	case s1 of
		Null 		=> s2 (* If first stream is empty, return second stream. *)
	|	Cons(h, t) 	=> Cons(h, fn () => concat(t(), s2)); (* Else construct stream with head
	from first stream and recursive call to concat function for rest of stream. *)


(* nats: Returns a stream that contains natural numbers starting from n *)
fun nats(n : int) = Cons(n, fn () => nats (n + 1))

(* fib: Returns a stream that contains the Fibonacci sequence starting with a and b *)
fun fib(a : int, b : int) = Cons(a, fn () => fib (b, a + b))


(*

The Sieve of Eratosthenes is possibly the oldest systematic method (algorithm) for generating the
sequence of all prime numbers. The "sieve" can be described as follows:

step 1: Generate the sequence of natural numbers starting at 2.
step 2: Position yourself just before the beginning of the sequence.
step 3: Find the next available number in the sequence. Write it down; it is prime.
step 4: Cross out (delete) all multiples of the number identified in step 3.
step 5: Continue with step 3.

*)

(* sieve: implement the Sieve of Eratosthenes. This function expects to be passed a stream of natural numbers starting with 2. *)
fun sieve (s : int stream) : int stream =
	let
		fun filter(f : 'a -> bool) (s : 'a stream) : 'a stream =
			case s of
				Null => Null
			|	Cons(h, t) => if f h then Cons(h, fn () => filter f (t()))
										else filter f (t())
		(* sift: apply filter with function that returns true when stream element is not divisible by p. *)
		fun sift (p : int) (s : int stream) : int stream =
			filter (fn n => n mod p <> 0) s
	in
		case s of
			Null 		=> Null (* If stream is empty return empty stream. *)
		|	Cons(h, t) 	=> Cons(h, fn () => sieve(sift h (t()))) (* return stream with head from s and
		with all multiples of head element filtered from the stream. *)
	end
	

(* discard_n: return stream left after removing first n elements. *)


(* rle: encode stream using the rle encoding algorithm. Return ordered pair (int * 'a). *)


(* foldl_n: perform fold on first n elements of list. *)