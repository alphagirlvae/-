fun thenAddOne(f, x) = f(x) + 1
fun double x = 2*x
fun square x = x*x

fun fact 0 = 1
  | fact x = x*fact(x - 1)

val testdouble = thenAddOne(double, 5) 
val testsquare = thenAddOne(square, 3)
val testfact = thenAddOne(fact, 4)


fun mapList(f, []) = []
  | mapList (f, x::L) = f(x)::(mapList(f, L))

val testlistdouble = mapList(double, [1,2,3])
val testlistsquare = mapList(square, [1,2,3])
val testlistfact = mapList(fact, [1,2,3])


fun mapList' f [] = []
       | mapList' f (x::R) = (f x) :: (mapList' f R)

val testList'double = mapList'(double)[1,2,3]
val testList'square = mapList'(square)[1,2,3]
val testList'fact = mapList'(fact)[1,2,3]


fun findOdd [] = NONE
  | findOdd (x::L) = 
  if (x mod 2 = 1) then
   SOME x
  else
    findOdd(L)
val testodd = findOdd([])
val testodd2 = findOdd([2])
val testodd3 = findOdd([2,3])


fun map f [] = []
  | map f(x::R) = (f x)::(map f R)


fun sublists [] = [[]]
  | sublists (x::R) = 
  let
    val S = sublists R
  in
    S @ (map (fn A => x::A) S)
  end

fun foldr F z [] = z
  | foldr F z (x::L) = F(x, foldr F z L)
  
fun sum L = foldr(op+) 0 L

fun findsub([], a) = NONE
  | findsub(x::L:int list list, a) = 
  if(sum(x) = a)
  then SOME(x) 
  else findsub(L, a)

fun subsetSumOption(L:int list, a:int) = 
  let val sub = sublists(L)
  in findsub(sub, a)
  end

val a = subsetSumOption([], 1)
val b = subsetSumOption([1,2,3], 6)
val c = subsetSumOption([1,2,3,4], 7)


fun exists p [] = false
  | exists p (x::L) = p(x) orelse exists p L


fun forall p [] = true
  | forall p (x::L) = p(x) andalso forall p L

fun oddP(0:int):bool = false
	|oddP(1) = true
	|oddP(n) = oddP(n-2)

val t1 = exists oddP [1,2]
val t2 = exists oddP [3,5,7]
val t3 = forall oddP [1,2]
val t4 = forall oddP [3,5,7]
;