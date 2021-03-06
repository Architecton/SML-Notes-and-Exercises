(* datatype representing a searchtree. The searchtree is eather empty or it is a node containing an integer value and tow child nodes *)
datatype searchtree = Empty | Node of int * searchtree * searchtree

(* Explicitly writing out a binary tree *)
(*
           5
          / \
         3   8
          \   \
           4  10
             /  \
            9   20
*)

val explicit_example =
    Node (5,
          Node (3,
               Empty,
               Node (4, Empty, Empty)),
          Node (8,
               Empty,
               Node (10,
                     Node (9, Empty, Empty),
                     Node (20, Empty, Empty)
                    )
               )
         )

(* height: return height of tree *)
fun num_nodes Empty = 0 (* Height of empty tree (with no nodes) is 0. *)
  | num_nodes (Node (_, l, r)) = 1 + num_nodes l + num_nodes r (* Count this node and make recursive calls for
  														left and right subtree. *)

(* height: return height of tree (alternative version) *)
fun num_nodes'(t : searchtree) : int  =
    case t
     of Empty => 0 (* Height of empty tree (with no nodes) is 0. *)
      | Node (_, l, r) => 1 + num_nodes' l + num_nodes' r (* Count this node and make recursive calls for left and right subtrees. *)

(* to_list: make a list of elements contained in tree *)
fun to_list Empty = [] (* Return empty list for empty tree (with no nodes). *)
  | to_list (Node (el, l, r)) = to_list l @ [el] @ to_list r (* Pattern match node and concatenate recursive calls and element. *)

(* search: return true if search tree contains element el and false otherwise. *)
fun search el Empty = false 		(* An empty tree does not contain any elements. *)
  | search el (Node (y, l, r)) =	(* Pattern match node and compare to its element. *)
    case Int.compare (el, y)
     of EQUAL => true				(* If found element return true. Else make recursive call based on comparison result. *)
     |  LESS => search el l
     |  GREATER => search el r

(* alternate version with case *)
fun search' (el : int) (s : searchtree) : bool =
	case s of
		Empty 		  => false
	|	Node(y, l, r) => case Int.compare (el, y) of
							EQUAL 	=> true
						|	LESS 	=> search el l
						|	GREATER => search el r

(* add: Add element el to this tree. *)
fun add el Empty = Node (el, Empty, Empty) (* If adding to empty tree, create Node with value el and Empty children. *)
  | add el (t as Node (y, l, r)) = (* Else pattern match root Node as t. *)
    case Int.compare (el, y) (* Compare el with value in root node. *)
     of EQUAL => t (* Duplicate element - keep original. *)
      | LESS => Node (y, add el l, r) (* If el lesser than root node value, make recursive call and try to add to left subtree. *)
      | GREATER => Node (y, l, add el r) (* If el greater than root node value, make recursive call and try to add to right subtree. *)

(* alternative version *)
fun add' (el : int) (s : searchtree) =
	case s of
		Empty 		  	   => Node (el, Empty, Empty)
	|	t as Node(y, l, r) => case Int.compare(el, y) of
								EQUAL 	=> t
							|	LESS  	=> Node(y, add el l, r)
							|	GREATER => Node(y, l, add el r)


(* remove: Remove element  *)
fun remove el Empty = Empty (* Trying to remove an element from an empty tree (with not nodes) does nothing. *)
  | remove el (Node (y, l, r)) = (* Pattern matrch root node. *)
    let 
    	(* split: get largue value in left subtree and new right child of left child of node replacing the deleted node. *)
    	fun split Empty = raise Fail "split undefined for Empty" (* Split function is undefined for empty trees. *)
          | split (Node(y, l, Empty)) = (l, y)					 (* Pattern match Node with no right child and return (left node, node value) *)
          | split (Node(y, l, r)) =								 (* Pattern match Node with two children (left can be empty). *)
                   let 
                   		val (r', z) = split r 					 (* make recursive call for right Node. *)
    				in 
    					(Node (y, l, r'), z)					 (* Return Node with value y, l as left child and r' as right child and z as value. *)
					end
    in
	    case Int.compare (el, y) of 				(* Compare element being removed with root node value. *)
	 		LESS => Node (y, remove el l, r) 		(* If smaller, make recursive call for left subtree. *)
	     |  GREATER => Node (y, l, remove el r) 	(* If greate, make recursive call for right subtree. *)
	     |  EQUAL => 								(* If found node that is to be deleted... *)
	        case (l, r) 							(* Pattern match left and right child of found node. *)
	         of (Empty, Empty) => Empty 			(* If both children are empty, replace node being removed with empty. *)
	         |  (Empty, _) => r 					(* If left node is empty, replace node being removed with its right child. *)
	         |  (_, Empty) => l 					(* If right node is empty, replace node being removed with its left child. *)
	         |  (_, _) => 							(* If Node to be removed has both children... *)
	            let val (l', y') = split l 			(* Pattern match and assign to variables l' and y' the value of split. *)
	            in Node (y', l', r)					(* Replace node being removed with node constructed with value y', left subtree l' and right subtree r. *)
	            end
    end

(* print_pr: print leaf values for elements of which the function f returns true. *)
fun print_pr (f : int -> bool) (s : searchtree) : unit =
	case s of
		Empty 		   		   => () 													(* Return unit for empty node. *)
	|	Node(el, Empty, Empty) => if f el then print((Int.toString el) ^ " ") else () 	(* If the node is a leaf (both children are empty), check result of f. *)
	|	Node(el, l, r)		   => (print_pr f l;  										(* If node not a leaf, make recursive calls for children. *)
									print_pr f r)

(* count_pr: print leaf values for elements of which the function f returns true. *)
fun count_pr (f : int -> bool) (s: searchtree) : int =
	case s of
		Empty 				   => 0 								(* Empty node: return 0. *)
	|	Node(el, Empty, Empty) => if f el then 1 else 0 			(* Leaf node: check if f returns true for el. If it does, count leaf. *)
	|	Node(el, l, r) 		   => (count_pr f l) + (count_pr f r) 	(* Recursive case: add up counts of both children. *)


(* print_one_child: print elements of nodes with one child node *)
fun print_one_child (s : searchtree) : unit =
	case s of
		Empty 				   => ()
	| 	Node(el, Empty, Empty) => ()
	|	Node(el, l, Empty) 	   => (print ((Int.toString el) ^ " "); print_one_child l)
	|	Node(el, Empty, r) 	   => (print ((Int.toString el) ^ " "); print_one_child r)
	|	Node(el, l, r)	   	   => (print_one_child l; print_one_child r)

exception EmptyTree

(* max: return maximum element in tree *)
fun max (s : searchtree) : int =
	case s of
		Node(el, Empty, Empty) => el
	|	Node(el, l, Empty) 	   => Int.max(el, max l)
	|	Node(el, Empty, r)	   => Int.max(el, max r)
	|	Node(el, l, r)		   => Int.max(Int.max(el, max l), max r)
	|	Empty 				   => raise EmptyTree

(* print_less: print all nodes with element less than a given value *)
fun print_less (lim : int) (s : searchtree) : unit =
	case s of
		Empty 		   => ()
	|	Node(el, l, r) => (if el < lim then print((Int.toString el) ^ " ") else (); print_less lim l; print_less lim r)

(* heigh: return height of tree (maximum number of nodes on a path from a root to a leaf node) *)
fun height (s : searchtree) : int =
	case s of
		Empty 		  => 0
	|	Node(_, l, r) => 1 + Int.max(height l, height r) 		

(* min_cost: return path with least cost (path where sum of elements in nodes is minimal) *)
fun min_cost (s : searchtree) : int =
	case s of
		Node(el, Empty, Empty) => el
	|	Node(el, l, Empty)	   => el + min_cost l
	|	Node(el, Empty, r)	   => el + min_cost r
	|	Node(el, l, r) 		   => el + Int.min(min_cost l, min_cost r)
	|	Empty 				   => raise EmptyTree

(*

A binary tree is said to be "balanced" if both of its subtrees  
are balanced and the height of its left subtree differs from the
height of its right subtree by at most 1.  Write a C function to
determine whether a given binary tree is balanced.

*)

(* is_balanced: return true if passed tree is balanced and false otherwise. *)
fun is_balanced (s : searchtree) : bool =
	case s of
		Empty 		   => true 																		(* An empty tree is balanced. *)
	|	Node(_, l, r)  => abs(height l - height r) <= 1 andalso is_balanced l andalso is_balanced r (* Check if conditions for balance are met. *)

(* map: apply function f to every element in each node and return the resulting tree. *)
fun map (f : int -> int) (s : searchtree) : searchtree =
	case s of
		Empty 		   => Empty
	|	Node(el, l, r) => Node(f el, map f l, map f r)

(* app: apply function f to every element in each node and return the resulting tree. *)
fun app (f : int -> unit) (s : searchtree) : unit =
	case s of
		Empty => ()
	|	Node(el, l, r) => (f el; app f l; app f r)
