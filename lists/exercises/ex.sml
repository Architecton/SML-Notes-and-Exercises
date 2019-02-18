fun map(f, l) : 'a list =
	if null l then []
	else f(hd l) :: map(f, tl l)