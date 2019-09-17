datatype sex = Male | Female

datatype person_record = Person_record of {
	name : string,
	surname : string,
	age : int,
	sex : sex
}

val p1 : person_record = Person_record {
	name = "Julian",
	surname = "Smith",
	age = 25,
	sex = Male
}

fun is_minor(Person_record{name, surname, age, sex}) : bool = age < 18

fun is_minor2(p : person_record) : bool =
	case p of
		Person_record{name, surname, age, sex} => age < 18


fun is_minor3(p : person_record) : bool =
	case p of
		Person_record x => #age x < 18


fun fit_for_service(Person_record{name, surname, age, sex}) : bool = age >= 18 andalso sex = Male

fun fit_for_service2(p : person_record) : bool =
	case p of
		Person_record x => #age x >= 18 andalso #sex x = Male

fun fit_for_service3(p : person_record) : bool =
	case p of
		Person_record{name, surname, age, sex} => age >= 18 andalso sex = Male

val fit_for_service4 = (fn Person_record{name, surname, age, sex} => age >= 18 andalso sex = Male)