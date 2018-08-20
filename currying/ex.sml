fun compose f g x = f(g(x));

val composed = compose (fn x : int => x*x) (fn x : int => x*x)

fun apply_function f x = f x;
 