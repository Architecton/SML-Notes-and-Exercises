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