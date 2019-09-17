
# Basic map example
doubled = list(map(lambda x: 2*x, [1,2,3,4,5]))


# Explicit map function
def map_exp(pred, list):
	# Base case: empty list
	if len(list) == 0:
		return []
	else:
		# Recursive case: apply function to head and join with results of recursive call on tail.
		return [pred(list[0])] + map_exp(pred, list[1:])