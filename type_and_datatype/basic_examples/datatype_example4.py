class Sex(object):
	def __init__(self, type):
		if type in {"Male", "Female"}:
			self.type = type
		else:
			self.type = "NA"

class Person_record(object):
	def __init__(self, name, surname, age, sex):
		if type(name) is str and type(surname) is str and type(age) is int and type(sex) is Sex:
			self.data = {
				'name' : name,
				'surname' : surname,
				'age' : age,
				'sex' : sex
			}

p1 = Person_record("Julia", "Smith", 25, Sex("Female"))


def fit_for_service(p):
	if type(p) is Person_record:
		return p.data['sex'].type == "Male" and p.data['age'] >= 18
	else:
		raise TypeError("Not a Person_record")
		return False

is_minor = lambda p: p.data['age'] < 18

