
# Simple example 1

def is_positive(x):
	return x >= 0

test_list = [1,2,3,4,-3,22,-7,2]

positive_values = list(filter(is_positive, test_list))

# Simple example 2

negative_values = list(filter(lambda x: x < 0, test_list))


### Filter with currying ###

# 
from functools import partial
pos = partial(filter, lambda x: x > 0)
positive_values1 <- pos([1, 2, 3, 4, 5, 6])

# Another implementation
def filt(pred):
	def flt(l):
		return list(filter(pred, l))
	return flt

negative = filt(lambda x : x < 0) ([1, 2, 3, -1, -2])


# Explicitly written filter

def filt2(pred, l):
	# base case
	if len(l) == 0:
		return []
	elif pred(l[0]):
		# If predicate is true for head, keep it and make recursive call for tail.
		return [l[0]] + filt2(pred, l[1:])
	else:
		# If predicate is not true for head, discard it and make recursive call for tail.
		return filt2(pred, l[1:])