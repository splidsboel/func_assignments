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
(* Declare a continuation-based version of the factorial function and compare the run time with
the results in Section 9.4. 
*)


//list for comparison between functions
let xs16 = List.init 1000000 (fun i -> 16)


//continuation based factorial function copied from p.206 of the book
let rec factA = function
    | (0,m) -> m
    | (n,m) -> factA(n-1,n*m)

//non-continuation based fact
let rec fact n =
    if n = 0 then 1
    else n * fact (n - 1)


for i in xs16 do let _ = fact i in ()
//results -> Real: 00:00:00.029, CPU: 00:00:00.029, GC gen0: 0, gen1: 0, gen2: 0


for i in xs16 do let _ = factA (i,1) in ()
//results -> Real: 00:00:00.074, CPU: 00:00:00.074, GC gen0: 0, gen1: 0, gen2: 0

(*
For some reason factA is in my case slower than the non-continuation based. I ran it several times
with similar results. I have no idea why, since I assumed tail-recursion would be faster. 
*)



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
    b


(*----------------------7.6 (HR 9.7)--------------------*)
let rec fibA n n1 n2 = 
    match n with 
    |0 -> 0
    |1 -> 1
    |n -> 