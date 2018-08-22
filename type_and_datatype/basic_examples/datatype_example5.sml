(* Declare datatype that represents a person's sex. *)
datatype sex = Male | Female

(* Declare a datatype person_record that represents a person's data record. *)
datatype person_record = Person_record of {
	first_name : string,
	last_name : string,
	age : int,
	sex : sex
}

(* Define and initialize a new variable of type person_record using the Person_record constructor. *)
val p1 : person_record = Person_record{
	first_name = "Julia",
	last_name = "Smith",
	age = 25,
	sex = Female
}

(* Check if person is a minor. *)
(* Note optional explicit return type declaration *)
fun is_minor(Person_record{first_name, last_name, age, sex}) : bool = age < 18

(* Another implementation of the function that checks if the person is a minor *)
fun is_minor2(p : person_record) =
	case p of 
		  Person_record{first_name, last_name, age, sex} => age < 18

(* Yet another implementation of the function that checks if the person is a minor. *)
fun is_minor3(p : person_record) =
	case p of
		  Person_record x => #age x < 18

(* Function that checks if a person is fit for military service. *)
fun fit_for_service(Person_record{first_name, last_name, age, sex}) = age >= 18 andalso sex = Male

(* Another implementation of a function that checks if a person is fit for military service. *)
fun fit_for_service2(p : person_record) =
	case p of
		  Person_record x => #age x >= 18 andalso #sex x = Male

(* Yet another function that checks if a person is fit for military service *)
(* Optional return type specification *)
fun fit_for_service3(p : person_record) : bool =
	case p of
		  Person_record{first_name, last_name, age, sex} => age >= 18 andalso sex = Male

(* Yet another function that checks if a person is fit for military service. *)
val fit_for_service4 = (fn Person_record{first_name, last_name, age, sex} => age >= 18 andalso sex = Male)