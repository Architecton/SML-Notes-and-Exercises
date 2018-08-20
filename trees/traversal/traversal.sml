(* datatype representing a searchtree. The searchtree is eather empty or it is a node containing an integer value and tow child nodes *)
datatype bintree = Empty | Node of int * bintree * bintree

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

val extree =
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

(* preorder: preorder traversal of tree t *)
(* root-l-r *)
fun preorder (t : bintree) : unit =
	case t of
		Empty => ()
	|	Node(el, l, r) => (print((Int.toString el) ^ " "); preorder l; preorder r)

(* inorder: inorder traversal of tree t *)
(* l-root-r *)
fun inorder (t : bintree) : unit =
	case t of
		Empty => ()
	|	Node(el, l, r) => (inorder l; print((Int.toString el) ^ " "); inorder r)

(* postorder: postorder traversal of tree t *)
(* l-r-root *)
fun postorder (t : bintree) : unit =
	case t of
		Empty => ()
	|	Node(el, l, r) => (postorder l; postorder r; print((Int.toString el) ^ " "))