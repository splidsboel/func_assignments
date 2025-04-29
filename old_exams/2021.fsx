(*------------------------------Question 2------------------------------*)
type Rope = 
    Leaf of string *int
    |Node of Rope * int *Rope


let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)),
    20,
    Node(Leaf("ogr",3),3,Leaf("amming",6)))

//1.1
let rope2 = Node(Leaf("_and", 4), 
    4, 
    Node(Leaf("_very_", 6), 6, Leaf("mich_F#", 7)))

(*
The type Rope is monomorphic because it has concrete types.
ie. it can only work with the types that are specified in the declaration
*)

let rope3= Node(Node(Leaf("example_", 8), 8, Leaf("with_", 5)), 
    13,
    Leaf("5_nodes", 7))


//1.2
//summing elements in tree recursively
let rec length r =
    match r with 
    |Leaf(s, id) -> id
    |Node(r1, i, r2) -> (length r1) + (length r2)

let rec flatten r =
    match r with
    |Leaf(s, id) -> s
    |Node(r1, i, r2) -> (flatten r1) + (flatten r2)

//find depth of tree. max function
let maxDepth r =
    let rec countDepth r acc =
        match r with
        |Leaf(s, i) -> acc+1
        |Node(r1, i, r2) -> max (countDepth r1 (acc+1)) (countDepth r2 (acc+1))
    countDepth r 0

//string indexing
let index i r = 
    let s = flatten r
    if i>(s.Length-1) then failwith "index outside string"
    else s.[i]


//1.3
let concat r1 r2 =
    Node(r1, (length r1) + (length r2), r2)

//pretty printing tree structure. tabulation of strings
let rec prettyPrint r = 
    match r with
    |Leaf(s, i) -> $"\tLeaf({s}, {i})"
    |Node(r1, i, r2) -> $"Node(\n\t{prettyPrint r1}, \n\t\t{i}, \n\t{prettyPrint r2})"

printfn ($"{prettyPrint rope1}")
(*------------------------------Question 2------------------------------*)

let list01 = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
              'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']

//Record type definition
type Bucket<'a> ={
    sizeBucket: int;
    elems : List<'a>
}

type UList<'a> = Bucket<'a> list

let ulist01 = [ 
    { sizeBucket = 4; elems = ['A'; 'B'; 'C'; 'D'] };
    { sizeBucket = 2; elems = ['E'; 'F'] };
    { sizeBucket = 6; elems = ['G'; 'H'; 'I'; 'J'; 'K'; 'L'] };
    { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
    { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
    { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]
//the type is Bucket<char> list because the type contained in the list 'elems' in each bucket is of the type char

let ulist02 = [
    { sizeBucket = 3; elems = ['A'; 'B'; 'C'] };
    { sizeBucket = 3; elems = ['D';'E'; 'F'] };
    { sizeBucket = 6; elems = ['G'; 'H'; 'I'; 'J'; 'K'; 'L'] };
    { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
    { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
    { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } 
]
(* 
ulist02=ulist01 returns false because comparison on lists in F# compare the ith element
of list 1 with the ith element of list 2. Therefore, elements with the same index must be the same.
But in this case, I changed the 0th and 1st element of the list of buckets
 *)


//create empty record
let emptyUL : UList<'a> = []

//2.2
//pattern matching on record type
let sizeUL ul = 
    let rec size ul acc =
        match ul with
        |[] -> acc
        |{sizeBucket =s}::xs -> size xs (acc+s)
    size ul 0

let isEmptyUL ul=
    if (sizeUL ul) = 0 then true else false

let rec existsUL e ul =
    //manually written helper function to check each elems list in record 
    let rec checkList el li =
        match li with 
        |[] -> false
        |x::xs -> if x=el then true else checkList el xs 

    //using List.forall
    let checkListL el li = List.forall (fun x -> x <> el) li

    match ul with 
    |[] -> false
    |{elems = list}::xs -> if checkListL e list then true else existsUL e xs

//helper function to concatenate all chars
let rec concatUL ul acc =
    match ul with 
    |[] -> acc
    |{elems = list}::xs -> concatUL xs (list@acc)

//list indexing
let itemUL ul i =
    let lis = concatUL ul []
    lis.[i]

let ulist03Wrong = [ { sizeBucket = 2; elems = [1] };
    { sizeBucket = 0; elems = [] };
    { sizeBucket = 5; elems = [2;3;4;5;6] } ]

//2.3
let rec chkUL ul =
    match ul with 
    |[] -> true
    |{sizeBucket = i; elems = list}::xs ->
        if List.length list <> i then false else
        if i>4 then false else
        if i<1 then false else
        chkUL xs


let map f ul =
    let rec mapUL f ul acc = 
        match ul with
        |[] -> acc
        |{sizeBucket =s; elems=list}::xs -> mapUL f xs ({sizeBucket=s; elems=(List.map f list)}::acc)

    List.rev(mapUL f ul [])

map (int) ulist01



(*----------------------------------------Question 3 ----------------------------------------*)

//3.1
let rec G (m,n) =
    match n with
    |n when n<=0 -> n+m
    |n  -> G((2*m), (n-1))+m

(* 
G is not tail recursive because the recursive call is not the last thing that happens;
we add m after the recursive call. 
 *)

let GA (m,n) =
    let rec Gacc (m,n) acc =
        match n with 
        |n when n<= 0 -> acc+m
        |_ -> Gacc((2*m),(n-1)) acc+m
    Gacc(m,n) 0


//3.2
let mySeq = seq{
    for x in 1..100 do
        for y in 1..100 do 
            yield (x,y) 
}

let gSeq = Seq.map GA mySeq

(*--------------------------Question 4--------------------------*)
type stack = int list
type inst =
    ADD
    | SUB
    | PUSH of int
    | LABEL of string
    | IFNZGOTO of string
    | EXIT

let insts01 =
    [PUSH 10;
    PUSH 12;
    ADD;
    EXIT]

let insts02 =
    [PUSH 10;
    LABEL "sub1";
    PUSH 1;
    SUB;
    IFNZGOTO "sub1";
    EXIT]


let execInsts insts =
    let rec exec insts s =
        match (insts,s) with
        | (SUB::is,v1::v2::s) -> exec is (v2-v1::s)
        | (ADD::is, v1::v2::s) -> exec is (v2+v1::s)
        | (PUSH x::is, s) -> exec is (x::s)
        | (LABEL lab::_,s) -> failwith "LABEL not implemented"
        | (IFNZGOTO lab::_,s) -> failwith "IFNZGOTO not implemented"
        | (EXIT::is, s) -> List.sum s
        | _ -> failwith "Missing stack values for instruction"
    exec insts []
    
type resolvedInst =
    RADD
    | RSUB
    | RPUSH of int
    | RIFNZGOTO of int
    | REXIT

type prog = Map<int,resolvedInst>

