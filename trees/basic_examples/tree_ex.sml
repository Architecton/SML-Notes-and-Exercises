datatype 'a btree = Leaf of 'a |
					Node of ('a btree * 'a * 'a btree);

val example_tree = Node(Leaf 5, 6, Node(Leaf 2, 4, Leaf 7));

(* Function that sums elements in the example_tree *)

fun sum_elements(Leaf(x)) = x |
	sum_elements(Node(l, el, r)) = sum_elements(l) + el + sum_elements(r);

(* Another way to implement the sum_elements function *)
fun sum_elements2(t : int btree) =
	case t of
		  Leaf(x) => x
		| Node(l, el, r) => sum_elements(l) + el + sum_elements(r)

(* Yet another way to implement the sum_elements function *)
fun sum_elements3(t : int btree) =
	case t of
		  Leaf x => x
		| Node x => sum_elements(#1 x) + (#2 x) + sum_elements(#3 x)

(* Yet another way to implement the sum_elements function *)
val sum_elements4 = (fn Leaf(el) => el | Node(l, el, r) => sum_elements l + el + sum_elements r);