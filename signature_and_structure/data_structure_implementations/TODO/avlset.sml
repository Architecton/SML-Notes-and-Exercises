functor AVLSet(
    type t
    val compare : t * t -> order
) : SET =
struct
    type element = t
    val compare = compare
    datatype avltree = Empty | Node of t * int * avltree * avltree
    type set = avltree

    fun height Empty = 0
      | height (Node (_, h, _, _)) = h

    fun leaf v = Node (v, 1, Empty, Empty)
    fun node (v, l, r) = Node (v, 1 + Int.max (height l, height r), l, r)

    val empty = Empty

    fun search x Empty = false
      | search x (Node (y, _, l, r)) =
            case compare (x, y)
             of EQUAL => true
              | LESS => search x l
              | GREATER => search x r
    val member = search

    fun rotateLeft (Node (x, _, a, Node (y, _, b, c))) = node (y, node (x, a, b), c)
      | rotateLeft t = t

    fun rotateRight (Node (y, _, Node (x, _, a, b), c)) = node (x, a, node (y, b, c))
      | rotateRight t = t

    fun imbalance Empty = 0
      | imbalance (Node (_, _, l, r)) = height l - height r

    fun balance Empty = Empty
      | balance (t as Node (x, _, l, r)) =
            case imbalance t
             of 2 =>
                    (case imbalance l
                      of ~1 => rotateRight (node (x, rotateLeft l, r))
                       | _ => rotateRight t)
              | ~2 =>
                    (case imbalance r
                      of 1 => rotateLeft (node (x, l, rotateRight r))
                       |  _ => rotateLeft t)
              | _ => t

    fun add x Empty = leaf x
      | add x (t as (Node (y, _, l, r))) =
            case compare (x, y)
             of EQUAL => t
              | LESS => balance (node (y, add x l, r))
              | GREATER => balance (node (y, l, add x r))

    fun remove x Empty = Empty
      | remove x (Node (y, _, l, r)) =
            let fun removeSuccessor Empty = raise Fail "impossible"
                  | removeSuccessor (Node (x, _, Empty, r)) = (r, x)
                  | removeSuccessor (Node (x, _, l, r)) =
                      let val (l', y) = removeSuccessor l
                      in (balance (node (x, l', r)), y)
                      end
            in case compare (x, y)
                of LESS => balance (node (y, remove x l, r))
                 | GREATER => balance (node (y, l, remove x r))
                 | EQUAL => case (l, r)
                     of (_, Empty) => l
                      | (Empty, _) => r
                      | _ =>
                           let val (r', y') = removeSuccessor r
                           in balance (node (y', l, r'))
                           end
            end

    fun to_list Empty = []
      | to_list (Node (x, _, l, r)) = to_list l @ [x] @ to_list r   
end
