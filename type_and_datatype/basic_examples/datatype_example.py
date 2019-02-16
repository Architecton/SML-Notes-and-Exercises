class Complex(object):
	def __init__(self, re, im):
		self.re = re
		self.im = im

	def __add__(self, other):
		return complex(self.re + other.re, self.im + other.im)

	def __sub__(self, other):
		return complex(self.re - other.re, self.im - other.im)

	def print(self):
		if self.im >= 0:
			print("{0} + {1}i".format(self.re, self.im))
		else:
			print("{0} - {1}i".format(self.re, self.im))

c = complex(1.2, 3.3)

def get_re(c):
	return c.re

get_im = lambda c: c.im