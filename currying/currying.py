from functools import partial

# Simple function that adds two numbers
def add(a, b):
	return a + b

# Set first argument to 2
add_part = partial(add, 2)

# Function now accepts only one argument.
res = add_part(5)