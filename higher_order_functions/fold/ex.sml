fun foldr (f : 'a * 'b -> 'b) (acc : 'b) (l : 'a list) =
	case l of
		[] => acc
	|	x::xs => f(x, foldr f acc xs)

fun sum(l : int list) : int =
	foldr (fn(el, acc) => acc + el) 0 l

fun foldl (f : 'a * 'b -> 'b) (acc : 'b) (l : 'a list) =
	case l of
		[] => acc
	|	x::xs => foldl f (f(x, acc)) xs

fun rev l = foldl List.:: [] l