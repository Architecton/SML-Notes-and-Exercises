(* simple example of a signature definition *)
(* Structures implementing this signature will have to implement all the value specifications presented in this signature *)
signature VEHICLE =
sig
	type vehicle_color;
	val color : vehicle_color;
	val top_speed : int;
	val start : unit -> string;
end

(* Create a structure car that implements the VEHICLE signature. All value specifications must be matched *)
structure car : VEHICLE =
struct
	(* Define type vehicle_color. *)
	type vehicle_color = {r : int, g : int, b : int};
	(* Define value color of type vehicle color. *)
	val color : vehicle_color = {r=0, g=0, b=255};
	(* Define top_speed value. *)
	val top_speed = 250;
	(* Implement function that takes unit and returns string. *)
	val start = (fn () => "Starting the engine...");
end

(* The functor takes structure and returns another structure. It acts as a kind of a constructor. *)
(* Here we define a make_aeroplane functor. The functor takes implementations of value specifications and assigns them to a structure implementing the VEHICLE signature. *)
functor make_aeroplane (
	type aeroplane_color;
	val color : aeroplane_color;
) : VEHICLE =
struct
	type vehicle_color = aeroplane_color;
	val color = color;
	val top_speed = 1000;
	val start = (fn () => "Starting the engines");
end

(* Example of using the functor to create a structure by passing in a limited number of value specifications *)
structure a1 = make_aeroplane (
	type aeroplane_color = {r:int, g:int, b:int};
	val color : aeroplane_color = {r = 0, g = 0, b = 0};
)