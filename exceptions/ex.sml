exception MyException of string;

fun some_function 0 = raise MyException
  |	some_function 42 = 1
  | some_function x = 2 * x