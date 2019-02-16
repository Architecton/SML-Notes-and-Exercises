class Color(object):
	def __init__(self, type):
		if type in {"Red", "Green", "Blue"}:
			self.type = type
		else:
			self.type = "NA"


def translate(color):
	try:
		transl = {
			"Red" : "Rot",
			"Green" : "Grun",
			"Blue" : "Blau"
		}
		return transl[color.type]
	except (KeyError, AttributeError) as e:
		return "NA"