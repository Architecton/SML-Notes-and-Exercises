(* A Signature in SML is similar to an interface in Java *)
(* A Structure in SLM is similar to a Class in java *)

(* Example *)
signature VEHICLE =
sig
	type color;
	val this_color : color;
	val top_speed : int;
	val start : unit -> string;
end

(* Example of an implementation of a signature *)
structure car : VEHICLE =
struct
	(* Each attribute of the signature VEHICLE must be matched in the structure
	that implements the signature *)
	type color = {r : int, g : int, b : int};
	val this_color : color = {r=0, g=0, b=0}
	val top_speed = 250;
	(* Called with a unit type as:
	car.start () *)
	val start = (fn () => "Starting the engine...");
end

(* Example of using a functor to implement a signature *)
(* we need a "function" that takes in a structure and returns 
another structure Such "functions" are precisely the ML functors. *)

(* Think constructor *)
functor make_vehicle(
	type color;
	val this_color : color;
	val top_speed : int;
	val start : unit -> string;
) : VEHICLE = 
struct
	type color = color;
	val this_color = this_color;
	val top_speed = top_speed;
	val start = start;
end

(* Make structure using functor *)
structure motorbike = make_vehicle (
	type color = {r:int, g:int, b:int};
	val this_color = {r=12, g=0, b=255};
	val top_speed = 160;
	val start = (fn () => "rpporrprkrad")
)