(* 5.1 *)
type 'a BinTree = Leaf| Node of 'a * 'a BinTree * 'a BinTree

let rec inOrder = function
    |Leaf -> []
    |Node(j, t1, t2) -> inOrder t1 @ [j] @ inOrder t2

let intBinTree = Node(43, Node(25, Node(56,Leaf, Leaf), Leaf), Node(562, Leaf, Node(78, Leaf, Leaf)))

inOrder intBinTree

(* 5.2 *)

let mapInOrder f tr = List.map f (inOrder tr)

mapInOrder (fun x-> x+1) intBinTree
(* 
    mapInOrder applies the function while traversion where as mapPostOrder applies 
    the function after traversing the tree. However, as the function is still applied 
    to the nodes once, the result tree will be the same.
*)

(* 5.3 *)
let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf), Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

let foldInOrder f s tr = List.fold f s (inOrder tr)

foldInOrder(fun n a -> a + n ) 0.0 floatBinTree

(* 5.4 *)

//type declarations
type aExp = (* Arithmetical expressions *)
    | N of int (* numbers *)
    | V of string (* variables *)
    | Add of aExp * aExp (* addition *)
    | Mul of aExp * aExp (* multiplication *)
    | Sub of aExp * aExp (* subtraction *)

type bExp = (* Boolean expressions *)
    | TT (* true *)
    | FF (* false *)
    | Eq of aExp * aExp (* equality *)
    | Lt of aExp * aExp (* less than *)
    | Neg of bExp (* negation *)
    | Con of bExp * bExp (* conjunction *)

type stm = (* statements *)
    | Ass of string * aExp (* assignment *)
    | Skip
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else *)
    | While of bExp * stm (* while *)
    | RU of bExp * stm (* Repeat Until *)
    | IT of bExp * stm (*if-then*)

//evaluation function for arithmetic expressions
let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s

//evaluation function for boolean expressions
let rec B b s = 
    match b with
    | TT -> true
    | FF -> false
    | Eq(a1, a2) -> if A a1 s = A a2 s then true else false
    | Lt (a1, a2) -> if A a1 s < A a2 s then true else false
    | Neg (b) -> not (B b s)
    | Con(b1, b2) ->  if B b1 s && B b2 s then true else false
let update x v s = Map.add x v s

let rec I stm s =
    match stm with
    | Ass(x,a) -> update x ( A a s ) s
    | Skip -> s
    | Seq(stm1, stm2) -> 
        let s' =I stm1 s 
        I stm2 s'
    | ITE(b,stm1,stm2) -> 
        if B b s then I stm1 s else I stm1 s
    | While(b, stm) -> 
        if B b s then 
            let s' = I stm s
            I stm s'
        else 
            s
    | RU(b, stm) ->
        if not (B b s) then 
            let s' = I stm s
            I stm s'
        else s
    | IT(b,stm1) ->
        if B b s then 
            I stm1 s
        else
            s


let stmt0 = Ass("res",(Add(N 10, N 30)))
let state0 = Map.empty

I stmt0 state0