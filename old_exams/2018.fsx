(*--------------------------------------Question 1-------------------------------*)
//1.1
type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

let ex3 = HP(1, HP(2, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), 
        HP(4, EmptyHP, EmptyHP))    
//the type of ex3 is monomorphic because it stores ints

let empty = EmptyHP

exception HeapError of string

let isEmpty = function
    |EmptyHP -> true
    |_ -> false

let find h = 
    match h with
    |HP(root, leftNode, rightNode) -> root
    |_ -> raise( HeapError "Heap is empty")

find ex3

let rec chkHeapProperty h =
    match h with
    |EmptyHP -> true
    |HP(root, leftNode, rightNode) -> 
        let leftOk =
            match leftNode with
            | EmptyHP -> true
            | HP(leftRoot, _, _) -> leftRoot >= root && chkHeapProperty leftNode
        let rightOk =
            match rightNode with
            | EmptyHP -> true
            | HP(rightRoot, _, _) -> rightRoot >= root && chkHeapProperty rightNode
        leftOk && rightOk


let rec map f h =
    match h with 
    |EmptyHP -> EmptyHP
    |HP(root, right, left) ->
        let leftMapped = map f left
        let rightMapped = map f right
        HP(f root, leftMapped, rightMapped)

(*
The traversal order is post-order because we map the left first, then right and lastly the root
*)

chkHeapProperty (map ((%) 3) ex3)


(*------------------------------------Question 2------------------------------------*)
//2.1
//random numbers (call with ())
let random =
    let rnd = System.Random()
    fun () -> rnd.Next(1,10000)

let genRandoms n = Seq.init n (fun i -> random()) |> Seq.toArray

//parallel array generation
let genRandomsP n =
    let rnd = System.Random()
    Array.Parallel.init n (fun _ -> rnd.Next(1,10000))

//2.2
let split xs = 
    let mid = (List.length xs)/2
    let left = xs.[0..(mid-1)]
    let right = xs.[mid..]
    (left, right)

//test 1
split [22;746;931;975;200]
(*
Tests than a list with uneven length is split correctly
*)

//test 2
split [1;2;3;4]
(*
Tests that a list with even length is split correctly
*)

let indivisible xs = if (List.length xs) <= 1 then true else false

//merge two lists
let rec merge xs ys = 
    match (xs, ys) with
    |([], ys) -> ys
    |(xs, []) -> xs
    |(x::xs', y::ys') ->
        if x<y then
            x:: merge xs' ys
        else
            y:: merge xs ys'


//merge([1;3;4;5], [1;2;7;9])

//2.3

let divideAndConquer split merge indivisible p =
    let rec dc p =
        if indivisible p
        then merge p
        else split p
    dc p

(*--------------------------------Question 3--------------------------------*)
let triNum = Seq.initInfinite (fun n -> (n*(n+1)/2))

let triNumC = Seq.cache triNum 

let rec myFilterOddIndex s = 
    Seq.delay (fun () ->
        if Seq.isEmpty s then
            Seq.empty
        else
            Seq.append
                (Seq.singleton (Seq.item 0 s))
                (myFilterOddIndex (Seq.skip 2 s))
    )





    

    