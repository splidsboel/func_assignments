(*
I hereby declare that I myself have created this exam hand–in in its entirety without help from AI
tooling and anybody else.
*)

(*------------------------------------------------Question 1------------------------------------------------*)
//1.1
let noRemainderP m n =
    if m%n=0 then true else false

//recursively checks eachs n if they satisfy noRemainderP. If one fails, returns false
let rec checkNumber n m =
    match n with
    |1 -> true
    |x -> if (noRemainderP m n) then checkNumber (n-1) m else false

//pattern matches on (f n). continues recursive calls with incrementing values for n until an evaluation of (f n) returns true. Then returns n
let untilTrue f =
    let rec untilTrueAcc f n =
        match f n with 
            |true -> n
            |false -> untilTrueAcc f (n+1)
    untilTrueAcc f 1
(*
untilTrue is tail-recursive because the only thing that happens in each recursive
call is the next recursive call. Therefore, it does not build up the stack
*)


//untilTrue (fun _ -> false)
(*
This function call never stops since the function given as argument to untilTrue
always evaluates to false. Therefore, it never reaches the base case of untilTrue
and continues forever
*)

let findSmallest n = untilTrue (checkNumber n)

//1.2

(*
Uses helper function reverseList to reverse xs and appends it to ys with @
*)
let revAppend xs ys =
    let rec reverseList ls acc =
        match ls with
        |[] -> acc
        |x::xs -> reverseList xs (x::acc)
    (reverseList xs []) @ ys

//uses helper function with an accumulator to sum up values in xs and length counter with which we divide the accumulator in the base case
let average xs =
    let rec averageA (xs: float list) acc len =
        match xs with
        |[] -> acc/len
        |y::ys -> averageA ys (acc+y) (len+1.0)
    match xs with
    |[] -> 0.0
    |ls -> averageA xs 0.0 0
    
//uses helper function that introduces the max argument. Recursion call changes depending the evaluation of (f y). Returns the max element found in the base case
let maxBy f xs =
    let rec maxByA f xs max =
        match xs with
        |[] -> max
        |y::ys ->
            if (f y)>max then maxByA f ys y
            else maxByA f ys max
    match xs with
    |[] -> failwith "maxBy: empty list"
    |xs -> maxByA f xs 0


//1.3

let rec collect f = function
    [] -> []
    | x::xs -> f x @ collect f xs

(*
collect is not tail-recursive because (f x) is appended to the result of the recursive call
This results in variables (for the evaluation of (f x)) being built up on the stack 
*)

//uses helper function with an accumulator to collect the appends
let collectTailRecursive f xs =
    let rec collectA f xs acc =
        match xs with
        |[] -> acc
        |y::ys -> collectA f ys (revAppend (f y) acc)
    List.rev (collectA f xs [])

collectTailRecursive id [[1;2];[3;4]]


