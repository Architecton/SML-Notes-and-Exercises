fun map(f, l) =
	if null l then []
	else f (hd l) :: map(f, tl l)