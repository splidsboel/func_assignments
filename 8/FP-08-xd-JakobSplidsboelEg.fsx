(*----------------------8.1 (HR 9.8)---------------------*)
(*
Develop a version of the counting function for binary trees
countA: int -> BinTree<’a> -> int
that makes use of an accumulating parameter. Observe that this function is not tail recursive.
*)

//type declaration for binary trees
type 'a BinTree = Leaf| Node of 'a * 'a BinTree * 'a BinTree


//I'm assuming that counting means counting the number of nodes in the tree??
let rec countA  (acc:int) t = 
    match t with
    |Leaf -> acc
    |Node(_, left, right) -> 
        let leftCount = countA (acc+1) left //make leftCount variable to pass to be able to pass an incremented accumulator to the counting of the right subtree
        (countA leftCount right)

//example tree to test counting function
let testTree = 
    Node(1,
        Node(2,
            Leaf,
            Node(3, Leaf, Leaf)
        ),
        Node(4, Leaf, Leaf)
    )

//should equal 4
countA 0 testTree

//I made this alternative to sum values of the nodes in the tree. Only works for int trees ofc
let rec countAValues  (acc:int) t = 
    match t with
    |Leaf -> acc
    |Node(n, left, right) -> 
        let leftCount = countAValues (acc+n) left //make leftCount variable to pass to be able to pass an incremented accumulator to the counting of the right subtree
        countAValues leftCount right

//should equal 10
countAValues 0 testTree


(*-------------------------8.2 (HR 9.9)--------------------------*)

let rec countAC t a c = 
    match t with 
    |Leaf -> c a
    |Node(_, left, right) -> 
        countAC left (a+1) (fun leftResult -> countAC right leftResult c) //count the right subtree in the anonymous function

//should return 4 just as before
countAC testTree 0 id


(*-----------------------8.3 (HR 9.10)------------------------------*)

let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res))

//bigListK 300000 id 

(*
Since bigListK is tail-recursive, we don't push any new stack frames
even for a large n. This means that for each recursive call we further nest
the chain of continuations. This is stored on the heap (I think).
When we reach the base case, it is time to evaluate the closure chain,
and it is here it gets tricky. When we evaluate the anonymous functions, we use the 
cons operator. When we have to evaluate 1::k(res) we build a long chain of 1::(1::(1::.....)). 
When they have to be evaluated, each little parenthesis has to be evaluated before the cons
operator can be applied. All these small evaluations build up the stack, until it 
all comes crashing down :(.
So the stack overflow actually doesn't come from the recursive calls but the evaluation part after the closure chain has been built.
*)


(*--------------------------------------8.4 (HR 9.11)-----------------------------------------*)

(*
Declare tail-recursive functions leftTree and rightTree. By use of leftTree it should
be possible to generate a big unbalanced tree to the left containing n+ 1 values in the nodes so
that n is the value in the root, n− 1 is the value in the root of the left subtree, and so on. All
subtree to the right are leaves. Similarly, using rightTree it should be possible to generate a
big unbalanced tree to the right.

1. Use these functions to show the stack limit when using count and countA from Exercise 9.8.

2. Use these functions to test the performance of countC and countAC from Exercise 9.9.
*)

//for some reason, I use a different type declaration for BinTree in 8.1 and 8.2. Now the value is in the middle and the subtrees of the 
//node is left or right in the type declaration. 
type 'a BinTreeMid = Leaf| Node of 'a BinTreeMid * 'a * 'a BinTreeMid

//functions to use later 
let rec count = function
    | Leaf -> 0
    | Node(tl,n,tr) -> count tl + count tr + 1

//had to rewrite to fit with new type declaration
let rec countACMid t a c = 
    match t with 
    |Leaf -> c a
    |Node(left, _, right) -> 
        countACMid left (a+1) (fun leftResult -> 
            countACMid right leftResult c)

let rec countC t c =
    match t with
    | Leaf -> c 0
    | Node(tl,n,tr) ->
    countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))


//functions to build unbalanced trees
let rec leftTree n acc =
    match n with 
    |0 -> acc
    |n -> 
        let newAcc = Node(acc, n, Leaf)
        leftTree (n-1) newAcc
let rec rightTree n acc = 
    match n with 
    |0 -> acc
    |n ->
        let newAcc = Node(acc, n, Leaf)
        rightTree (n-1) newAcc


//testing count and countA
let bigLeftTree = leftTree 1000000 Leaf
let bigRightTree = rightTree 1000000 Leaf

let mediumLeftTree = leftTree 10000 Leaf
let mediumRightTree = rightTree 10000 Leaf



(* 1. Show stack limit *)

//gives a stack overflow 
//count bigLeftTree //commented out to not break stuff
//continuation-based tail-recursive does not give an error
countC bigLeftTree id 


(* 2. Testing performance of countA and countAC*)
#time
countC bigLeftTree id
//result: Real: 00:00:00.029, CPU: 00:00:00.029, GC gen0: 0, gen1: 0, gen2: 0

countACMid bigLeftTree 0 id
//result: Real: 00:00:00.018, CPU: 00:00:00.018, GC gen0: 0, gen1: 0, gen2: 0

(*
countAC is faster because it has the accumulator that it can return. 
*)


(*-------------------------------------8.5 (HR 11.1)-----------------------------------------*)

//create infinite sequence of natural numbers and filter out all even numbers
let nats = Seq.initInfinite (fun i-> i)
let odds = Seq.filter (fun i -> i%2=1) nats


(*-----------------------------------8.6 (HR 11.2)------------------------------------------*)

//need fact function to pass to sequence-constructing function
let rec fact n = 
    match n with
    |0 -> 1
    |_ -> n* fact(n-1)
let facts = Seq.initInfinite (fun i -> fact i)

//example of finding the third element in the sequence of factorials
Seq.item 3 facts