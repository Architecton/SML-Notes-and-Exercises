datatype 'a stream = Cons of 'a * (unit -> 'a stream)

(* Prvih n elementov toka pretvori v seznam *)
fun to_list 0 _ = []
  | to_list n (Cons (x, s)) = x :: (to_list (n-1) (s ()))

(* n-ti element toka *)
fun elementAt 0 (Cons (x, _)) = x
  | elementAt n (Cons (_, s)) = elementAt (n-1) (s ())

fun from_list [] r = Cons (r, fn () => from_list [] r)
  | from_list (x::xs) r = Cons (x, fn () => from_list xs r)

fun head (Cons (x, s)) = x

fun tail (Cons (x, s)) = s ()

fun from k = Cons (k, fn () => from (k+1))

val nat = from 0

val fib =
  let fun f a b = Cons (a, fn () => f b (a+b))
  in f 0 1
  end

fun veckratniki_stevila k =
  let fun vec n = Cons (n, fn () => vec (n + k))
  in vec 0
  end

val veckratniki =
    let fun v k = Cons (veckratniki_stevila k, fn () => v (k+1))
    in v 0
    end

fun zip f (Cons (x, s)) (Cons (y, t)) =
  Cons (f x y, fn () => zip f (s ()) (t ()))

fun map f (Cons (x, s)) = Cons (f x, fn () => map f (s ()))

fun fold f a (Cons (x, s)) =
  let val (y, a) = f a x
  in Cons (y, fn () => fold f a (s ()))
  end

fun flatten ss =
  let fun flat neck [] (Cons (s, ss))  = flat [] (neck @ [s]) (ss ())
        | flat neck ((Cons (x, t)) :: ts) ss = Cons (x, fn () => flat (neck @ [t ()]) ts ss)
  in flat [] [] ss
  end

fun product s t = flatten (map (fn y => map (fn x => (x, y)) s) t)

val nuts =
    let fun f k = Cons (from (1000 * k), fn () => f (k + 1))
    in f 0
    end
