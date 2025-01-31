
// Exercise 1.1
let sqr (x: float) = x * x


// Exercise 1.2
let pow (x: float) (n: float) = System.Math.Pow(x,n)


// Exercise 1.3
// let g n  = n + 4
let g (n: int) = n + 4


// Exercise 1.4
let h (x: float) (y: float) = System.Math.Sqrt((sqr x) + (sqr y))


// Exercise 1.5
let rec f = function
    | 0 -> 0
    | n -> n + f(n-1) // but if n = -1 it will create infinite recursion, leading to a stack overflow


// Exercise 1.6
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2)


// Exercise 1.7
let rec sum = function
  //| ((m:int),0) -> 0 WRONG
  //| ((m:int),(n:int)) -> (m+n) + sum(m+(n-1)) WRONG
    | ((m:int),0) -> m
    | ((m:int),(n:int)) -> (m+n) + sum(m,n-1)


// Exercise 1.8
(* 
(System.Math.PI, fact -1)       = (float, int)
fact(fact 4)                    = int
power(System.Math.PI, fact 2)   = float
(power, fact)                   = (float * int -> float, int -> int)
*)


// Exercise 1.9
(*
a -> 5
f -> fun a -> a+1
g -> fun b -> (f b) +a

f 3 = val it: int = 4
g 3 = val it: int = 9 
*)

// Exercise 1.10
let dup (a:string) = a + a

// Exercise 1.11

let rec dupn (s,n) =
    match (s,n) with 
    |(s,0) -> ""
    |(s,n) -> s + dupn(s,n-1)

dupn("hi", 3)
