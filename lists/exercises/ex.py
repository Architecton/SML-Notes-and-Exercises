def filt(predicate, l):
	if len(l) == 0:
		return []
	elif predicate(l[0]):
		return [l[0]] + filt(predicate, l[1:])
	else:
		return filt(predicate, l[1:])