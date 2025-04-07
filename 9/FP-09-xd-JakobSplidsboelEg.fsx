(*-------------------------------------Question 1-----------------------------------------*)

(*
Disclaimer!!
I realize now that I used the wrong argument names for some of my functions in the heap exercises.
I'm too lazy to go back and fix it, so I pray for your forgiveness instead.
*)

type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

//1.1
let ex3 = HP(1,HP(2,HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))

//value type of ex3 is int. It is monomorphic because there are only ints in the heap.
//The heap would be polymorphic if it had both strings and ints

let empty = EmptyHP

exception HeapError of string


//1.2
//use empty heap value from previous 
let isEmpty hp = if hp = empty then true else false 


//when at root, add size of both subtrees (found by calling size recursively) and return result
let rec size hp = 
    match hp with
    |EmptyHP -> 0
    |HP(n, hpl, hpr) -> size hpl+ size hpr + 1


//min value is always at the top, so return first value we see. Raise exception from before if heap is empty
let find hp = 
    match hp with
    |HP(n, hpl, hpr) -> n
    |EmptyHP -> raise(HeapError "Heap is empty, find makes no sense")


//check that the value (n1/n2) of subtrees is less than value of original node
let rec chkHeapProperty hp = 
    match hp with
    |EmptyHP -> true
    |HP(n, hpl, hpr) -> 
        let leftOk = 
            match hpl with
            |EmptyHP -> true
            |HP(n1, _, _) -> n<=n1 && chkHeapProperty hpl
        let rightOk =
            match hpr with
            |EmptyHP -> true
            |HP(n2, _, _) -> n<=n2 && chkHeapProperty hpr
        leftOk && rightOk


//1.3
//use function recursively on left and right subtrees and then return the heap made up 
//of those with the function applied on the top value
let rec map f h = 
    match h with
    |EmptyHP -> EmptyHP
    |HP(n, hpl, hpr) ->  
        let leftMapped = map f hpl
        let rightMapped = map f hpr
        HP(f n, leftMapped, rightMapped)

(* 
the function is applied to node values in pre-order traversal, since we first 
transform the value of the original node, then left sub-tree and finally the right sub-tree
*)

//example of function that results in a heap that violates the heap property
chkHeapProperty (map (fun x -> x%2) ex3)


(*-------------------------------------Question 3-------------------------------------*)
//3.1
//feed the formula for trinums into Seq.initInfinite
let triNum = Seq.initInfinite (fun n -> (n*(n+1)/2))

//pipe previous sequence into Seq.cache
let triNumC = triNum |> Seq.cache


//3.2
//use seq.delay and use the stuff in the example as the generator. Also added a clause for the empty sequence.
let rec myFilterOddIndex s = 
    Seq.delay (fun () ->
        if Seq.isEmpty s then
            Seq.empty
        else
            Seq.append
                (Seq.singleton (Seq.item 0 s))
                (myFilterOddIndex (Seq.skip 2 s))
    )

//3.3
let rec zipSeq s1 s2 =
    seq {
        let e1 = Seq.item 0 s1
        let e2 = Seq.item 0 s2
                    }