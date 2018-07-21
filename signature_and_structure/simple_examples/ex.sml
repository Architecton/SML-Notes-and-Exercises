signature VEHICLE =
sig
	val top_speed : int;
	type color;
	val vehicle_color : color;
	val start : unit -> string;
end

structure my_car : VEHICLE =
struct
	val top_speed = 200;
	type color = {r : int, g : int, b : int};
	val vehicle_color : color = {r = 255, g = 255, b = 255};
	val start = (fn () => "Starting engine!");
end