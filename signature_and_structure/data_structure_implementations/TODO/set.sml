signature SET =
sig
    type element
    val compare : element * element -> order
    type set
    val empty : set
    val member : element -> set -> bool
    val add : element -> set -> set
    val remove : element -> set -> set
    val to_list : set -> element list
end
