(*------------------7.1 (HR 9.2)-------------------*)
(*Consider the function g declared on Page 202 and the stack and heap after the evaluation of g 2
shown in Figure 9.2. Reproduce this resulting stack and heap by a systematic application of push
and pop operations on the stack, and heap allocations that follow the step by step evaluation of
g 2.*)


//declarations
#time
let xs = [1;2]
let rec g = function
    | 0 -> xs
    | n ->  let ys = n::g(n-1)
            List.rev ys  

//No idea xd





(*-----------------7.2 (HR 9.3)--------------------*)
//assuming that to make it 'iterative' is to make it tail-recursive
let rec sum m n acc = 
    match n with
    |0 -> acc
    |n-> sum m (n-1) (acc+n)






(*------------------------7.3 (HR 9.4)-----------------------*)
//difference to a non-tail-recursive function is passing the accumulator and adding 1 to the accumulator for each element of the list.
//Then returning whatever the accumulator ends up being. 
let rec length lst acc = 
    match lst with 
    |[] -> acc
    |_::tail -> length tail (acc+1)

//lambda function instead. don't know when and why you would do this. 
let rec lengthLambda = fun lst acc ->
    match lst with
    | [] -> acc
    | _::tail -> lengthLambda tail (acc + 1)




(*----------------------7.4 HR(9.6)-----------------------*)
#time
(* 
Declare a continuation-based version of the factorial function and compare the run time with
the results in Section 9.4. 
*)


//list for comparison between functions
let xs16 = List.init 1000000 (fun i -> 16)


//continuation based factorial function 
let rec factC n c = 
    match n with
    | 0 -> c 1
    | _ -> factC (n-1) (fun x -> c (x*n))


//attempt at visualizing the recursive calls for the continuation based factC:
(* factC 3 id ->
factC 2 (fun x -> id (x*3))
factC 1 (fun x1 -> (fun x -> id (x*3)) (x1*2))
factC 0 (fun x2 -> (fun x1 -> (fun x -> id (x*3)) (x1*2)) (x2*1)) 1
(fun x2 -> (fun x1 -> (fun x -> id (x*3)) (x1*2)) (x2*1)) 1
(fun x1 -> (fun x -> id (x*3)) (x1*2)) (1*1)
(fun x -> id (x*3)) (1*2)
id (2*3)
6 *)



//non-continuation based fact. m is the accumulating parameter
let rec factA = function
| (0,m) -> m
| (n,m) -> factA(n-1,n*m)


for i in xs16 do let _ = factA (i,0) in ()
//results -> Real: 00:00:00.070, CPU: 00:00:00.070, GC gen0: 0, gen1: 0, gen2: 0


for i in xs16 do let _ = factC i id in ()
//results -> Real: 00:00:00.204, CPU: 00:00:00.487, GC gen0: 1, gen1: 1, gen2: 0




(*-------------------7.5 (HR 8.6)------------------*)
(*
Declare a function for computing Fibonacci numbers Fn (see Exercise 1.5) using a while
loop. Hint: introduce variables to contain the two previously computed Fibonacci numbers.
*)
let fib n= 
    let mutable steps = 0
    let mutable a = 0
    let mutable b = 1
    while (steps < n) do 
        let mutable temp = a+b
        a <- b
        b <- temp
        steps <- steps+1
    a


(*----------------------7.6 (HR 9.7)--------------------*)
//1

let rec fibA n n1 n2 =
    match n with
    | 0 -> n2
    | 1 -> n1
    | _ -> fibA (n - 1) (n1 + n2) n1

//2
(* 
A continuation-based version fibC: int -> (int -> int) -> int that is based on the
definition of Fn given in Exercise 1.5. 
*)
let rec fibC n c = 
    match n with 
    |0 -> c 0
    |1 -> c 1
    |_ -> fibC (n-1) c + fibC (n-2) c


//non-tail-recursive fib to compare (just for curiosity's sake)
let rec fibSlow n =
    if n <= 1 then n
    else fib (n - 1) + fib (n - 2)

//I compare the three functions finding the 40th fib number

//while version
fib 40
//results -> Real: 00:00:00.000, CPU: 00:00:00.002, GC gen0: 0, gen1: 0, gen2: 0

//accumulator version
fibA 40 1 0 
//results -> Real: 00:00:00.001, CPU: 00:00:00.002, GC gen0: 0, gen1: 0, gen2: 0

//continuation-based 
fibC 40 id
//Real: 00:00:00.433, CPU: 00:00:00.436, GC gen0: 0, gen1: 0, gen2: 0

//non-tail-recursive fib 
fibSlow 40
//results -> Real: 00:00:00.002, CPU: 00:00:00.003, GC gen0: 0, gen1: 0, gen2: 0
//actually not that slow
