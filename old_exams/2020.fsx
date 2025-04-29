(*------------------------Question 1------------------------*)
//1.1
type mymap<'a,'b> = MyMap of list<'a*'b>

let ex1 = MyMap [('A',65);('B',66);('C',67)]
let dice1 = MyMap[(1,4); (2, 2); (3,3); (4,2);(5,2);(6,2)]
let dice2 = MyMap[(1,4);(2,2);(3,3);(4,3);(5,5);(6,3)]

(*
The type for ex1 is mymap<char,int> because it consist of a list of tuples that have the type char*int
The tyoe for dice1 is mymap<int,int> because it consists of tuples of ints
*)

let emptyMap() = MyMap []

let size (MyMap m) = 
    let rec getSize m acc =
        match m with
        |[] -> acc
        |x::xs -> getSize xs (acc+1)
    getSize m 0

//1.2
let isEmpty m = if size m >0 then false else true

let  tryFind k (MyMap m)= 
    let rec tryFindMap k m =
        match m with
        |[] -> None 
        |(a,b)::xs -> if a=k then Some(a,b) else tryFindMap k xs
    tryFindMap k m 

tryFind 'B' ex1

(*
The 'when 'a:equality' constraint means that you can only pass a comparable argument as k
because we are comparing it against the keys in our mymap
*)

let remove k (MyMap m) =
    let rec removeA key m = 
        match m with
        |[] -> m
        |x::(a, b)::xs -> if a =key then x::xs else removeA key ((a,b)::xs)
    removeA k m 

let add k v (MyMap m) = 
    let addA k v m =
        match m with
        |n -> (k,v)::m
    MyMap (addA k v m)

//1.3
(* let upd f k v m =
    let updA f k v m =

        match m with
        | *)
