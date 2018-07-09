signature VEHICLE =
sig
	type vehicle_color;
	val color : vehicle_color;
	val top_speed : int;
	val start : unit -> string;
end

structure car : VEHICLE =
struct
	type vehicle_color = {r : int, g : int, b : int};
	val color = {r=0, g=0, b=255};
	val top_speed = 250;
	val start = (fn () => "Starting the engine...");
end

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

structure a1 = make_aeroplane (
	type aeroplane_color = {r:int, g:int, b:int};
	val color : aeroplane_color = {r = 0, g = 0, b = 0};
)