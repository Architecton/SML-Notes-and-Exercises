use "stack.sml";

structure S = Stack;
val my_stack : string S.stack = S.empty;

val my_stack : string S.stack = S.push("tralala", my_stack);
val my_stack : string S.stack = S.push("hopsasa", my_stack);

val top_of_stack : string = S.top(my_stack);