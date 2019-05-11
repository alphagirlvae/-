
fun reverse [] = []
    |reverse (x::xs) = (reverse xs) @ [x]

fun interleave ([],xs) = xs
    |interleave (ys,[]) = ys
    |interleave (x::xs,y::ys) = x::y::interleave(xs,ys)

datatype tree = Empty | Br of tree * int * tree
fun listToTree [] = Empty
  | listToTree xs =
        let val k =length xs div 2
            val y::ys = List.drop(xs,k)
        in Br(listToTree (List.take(xs,k)),y,listToTree ys)
        end

fun revT Empty = Empty
   | revT (Br(t1, x, t2)) = Br(revT t2, x, revT t1)
   
fun trav Empty = []
   | trav (Br(t1, x, t2)) = trav t1 @ (x :: trav t2)

val old = listToTree[1,2,3,4,5,6,7]
val after = revT old
val new = trav after

fun binarySearch(Empty:tree,_:int):bool = false
    |binarySearch(Br(t1, x, t2),a) = (case Int.compare(x,a) of
     GREATER => binarySearch(t1,a)
    | EQUAL => true 
    | LESS => binarySearch(t2,a))

val t = listToTree[1,2,3,4,5,6,7,8]
val t1 = binarySearch(t,5)
val t2 = binarySearch(t,9)
;