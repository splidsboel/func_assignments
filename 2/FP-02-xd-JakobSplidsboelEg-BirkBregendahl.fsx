// Exercise 2.1
let timediff (a,b)(c: int,d) = ((c-a)*60)+(d-b)

// Exercise 2.2
let minutes (a, b) = timediff(0,0) (a,b)

// Exercise 2.3
let rec pow (s, n) =
    match n with
    | 0 -> ""
    | n -> s + pow (s, n - 1)

// Exercise 2.4
let rec bin (n,k) = 
    match (n, k) with 
    |(_,0) -> 1
    |(n,k) when k=0->1
    |(n,k) when k>n->0
    |(n,k) -> bin(n-1,k-1) + bin(n-1,k)

// Exercise 2.5
(* 2.
1. int*int->int function
2. non-negative integers
3. 
    f(2,3)
    f(2-1,2*3)
    f(1,6)
    f(1-1,6*1)
    f(0,6)
    6
4. (x!)*y
*)

// Exercise 2.6
(*
1. boolean*int->int
2. never terminate
3. 0
*)

// Exercise 2.7
let curry f = fun a -> fun (b: 'b) ->f (a,b)

let uncurry g = fun(a,b)-> g a b

