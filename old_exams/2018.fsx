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
    |HP(root, leftNode, rightNode) when (find leftNode<root) && (find rightNode<root) -> 
        (chkHeapProperty leftNode) && (chkHeapProperty rightNode)
    |_ -> false

chkHeapProperty ex3

