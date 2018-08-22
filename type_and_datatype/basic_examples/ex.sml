datatype sex = Male | Female

type person_record = {
	name : string,
	last_name : string,
	age : int,
	sex : sex
}

val p1 : person_record = {
	name = "Julia",
	last_name = "Smith",
	age = 22,
	sex = Female
}

fun is_minor {name, last_name, age, sex} = age < 18


fun fit_for_service(p as {name, last_name, age, sex}) : bool =
	#age p >= 18 andalso sex = Male