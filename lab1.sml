
fun mult [] = 1
	| mult (x::L) = x * mult(L);

val intlist = [2,5,6]
val intlistlist = [[1,2],[2,4],[5]]
val test_mult = mult intlist;
fun Mult[] = 1
	|Mult(r::R) = mult(r) * Mult(R);
val test_Mult = Mult intlistlist;	

fun mult'([],a) = a
	|mult'(x::L,a) = mult'(L,a*x);

fun Mult'([],a) = a
	|Mult'(r::R,a) = Mult'(R,mult'(r)*a);
	

fun double(0:int):int = 0
	|double n = 2 + double(n-1);

val doubleeight = double 8;

fun square 0 = 0
	|square n = 1 + double(n-1) + square(n-1);
val squareeight = square 8;



fun divisibleByThree(0:int):bool = true
	|divisibleByThree 1 = false
	|divisibleByThree 2 = false
	|divisibleByThree n = if(n>0) then  divisibleByThree (n-3) else divisibleByThree (n+3);

val testdivisible1 = divisibleByThree 2;
val testdivisible2 = divisibleByThree ~6;
val testdivisible3 = divisibleByThree 9;

fun oddP(0:int):bool = false
	|oddP(1) = true
	|oddP(n) = oddP(n-2);
val testodd = oddP 3;
val testodd2 = oddP 2;