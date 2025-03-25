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

bigListK 300000 id 

(*
Since bigListK is tail-recursive, we don't push any new stack frames
even for a large n. This means that for each recursive call we further nest
the chain of continuations. This is stored on the heap (I think).
When we reach the base case, it is time to evaluate the closure chain,
and it is here it gets tricky. When we evaluate the anonymous functions, we use the 
cons operator. When we have to evaluate 1::k(res) we build a long chain of 1::(1::(1::.....)). 
When they have to be evaluated, each little parenthesis has to be evaluated before the cons
operator can be applied. All these small evaluations build up the stack, until it 
all comes crashing down :(
*)

