datatype 'a tree = Empty | Br of 'a tree*'a*'a tree

fun listToTree [] = Empty
  | listToTree xs =
        let val k =length xs div 2
            val y::ys = List.drop(xs,k)
        in Br(listToTree (List.take(xs,k)),y,listToTree ys)
        end
fun trav Empty = []
   | trav (Br(t1, x, t2)) = trav t1 @ (x :: trav t2)

fun mychange p [] = []
  | mychange p (x::L) =
  if(p(x))
  then (SOME x)::(mychange p L)
  else NONE::(mychange p L)

fun treeFilter p = 
let fun trans T = 
  let val a = trav T
      val b = mychange p a
  in  listToTree b
  end
in trans
end

fun oddP(0:int):bool = false
	|oddP(1) = true
	|oddP(n) = oddP(n-2);
val a = treeFilter oddP (listToTree[1, 2, 3, 4, 5])
val b = trav a
;