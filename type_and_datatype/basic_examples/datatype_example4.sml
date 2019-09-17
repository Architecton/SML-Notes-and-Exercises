(* Declare new datatype representing a person's sex *)
datatype sex = Male | Female

(* Declare new type *)
type person_record = {
	first_name : string,
	last_name : string,
	age : int,
	sex : sex
}

(* Define and initialize variable of person_record type *)
val p1 : person_record = {
	first_name = "Julia",
	last_name = "Smith",
	age = 25,
	sex = Female
}

(* Check if person is a minor *)
fun is_minor(p : person_record) =
	#age p < 18

(* Check if person is fit for millitary service *)
fun fit_for_service(p : person_record) =
	#age p >= 18 andalso #sex p = Male

(* Another implementation of the function that checks if the person if fit for military service *)
val fit_for_service2 = (fn p : person_record => #age p >= 18 andalso #sex p = Male);
