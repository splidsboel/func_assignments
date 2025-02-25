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
    JLKJLJLKJLJKLKJLKJLKJLKJLKJ
*)

(* 5.3 *)
let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf), Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

let foldInOrder f s tr = List.fold f s (inOrder tr)

foldInOrder(fun n a -> a + n ) 0.0 floatBinTree

(* 5.4 *)
