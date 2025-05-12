type Rope =
    Leaf of string * int
    | Node of Rope * int * Rope


let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)),
    20,
    Node(Leaf("ogr",3),3,Leaf("amming",6)))

let rope2 = Node(Leaf("_and", 4), 4, Node(Leaf("_very_", 6), 6, Leaf("much_F#", 7)))


(*
The type Rope is monomorphic because it is not parameterized with a generic type
*)

let rope3 =
    Node(Node(Leaf("example_", 8), 8, Leaf("with_", 5)), 13, Leaf("5_nodes", 7))

//1.2
let rec length r =
    match r with
    |Node (l, i, r)->
        i+(length r)
    |Leaf (s, i) ->
        i


let rec flatten r =
    match r with
    |Leaf (s, i) -> s
    |Node (l, i, r) ->
        $"{flatten l}{flatten r}"

let maxDepth r =
    let rec getDepth r acc=
        match r with
        |Leaf(s, i) -> (acc+1)
        |Node(l, i, r) -> max (getDepth l (acc+1)) (getDepth r (acc+1))
    getDepth r 0


let index i r =
    let s = flatten r
    if i>(String.length s-1) then failwith "index out of bounds" else
    s.[i]


let concat r1 r2 =
    Node(r1, (length r1), r2)

(*------------------------------------Question 2------------------------------------*)

let list01 = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']


type Bucket<'a> = {
    sizeBucket : int;
    elems : List<'a>
    }

type UList<'a> = Bucket<'a> list

let ulist01 = [ 
    { sizeBucket = 4; elems = ['A';'B';'C';'D'] };
    { sizeBucket = 2; elems = ['E';'F'] };
    { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
    { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
    { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
    { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

//type of ulist01 is Bucket<char> list because the type of what is in the elem of the buckets
//in the list is chars

let ulist02 = [ 
    { sizeBucket = 4; elems = ['A';'B';'C';'D'] };
    { sizeBucket = 1; elems = ['E'] };
    { sizeBucket = 7; elems = ['F';'G';'H'; 'I'; 'J'; 'K'; 'L'] };
    { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
    { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
    { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

ulist02 = ulist01
(*
returns false because F# compares lists on elements are in them. And since 
two buckets changed their size and elems, they are no longer equal*)

let emptyUL<'a> () : UList<'a> = []

//2.2
let rec existsUL e ul =
    match ul with
    |[] -> false
    |x::xs ->
        match x with
        |{sizeBucket =s; elems= list} -> 
            if List.contains e list then true else existsUL e xs

let itemUL ul i =
    let rec flattenUL x acc =
        match x with
        |[] -> acc
        |x::xs ->
            match x with
            |{sizeBucket =s; elems= list} -> 
                flattenUL xs (acc@list)
    let flattenedUL = flattenUL ul []
    if i>((List.length flattenedUL)-1) 
        then failwith "index out of bounds" 
        else flattenedUL.[i]

let filterUL p ul =
    let rec filterULA p ul acc =
        match ul with
        |[] -> List.rev acc
        |x::xs ->
            match x with
            |{sizeBucket =s; elems= list} ->
                let newList = List.filter p list
                let newSize = List.length newList
                if newSize>0 then
                    filterULA p xs ({sizeBucket = newSize; elems = newList}::acc)
                else 
                    filterULA p xs (acc)
    filterULA p ul []

filterUL (fun e -> e < 'I') ulist01

//2.3
let ulist03Wrong = [ { sizeBucket = 2; elems = [1] };
    { sizeBucket = 0; elems = [] };
    { sizeBucket = 5; elems = [2;3;4;5;6] } ]

let chkUL ul =
    match ul with
    |[] -> true
    |x::xs ->
        match x with
        |{sizeBucket =s; elems= list} ->
            if s<5 && s>0 && (List.length list)=s then true else false

let map f ul =
    let rec mapA f ul acc =
        match ul with
            |[] -> List.rev acc
            |x::xs ->
                match x with
                |{sizeBucket =s; elems= list} ->
                    mapA f xs ({sizeBucket = s; elems =(List.map f list)}::acc)
    mapA f ul []

let rec fold f a ul =
    match ul with
    |[] -> a
    |x::xs ->
        match x with
        |{sizeBucket =s; elems= list} ->
            let newAcc = f a list
            fold f newAcc xs


(*------------------------------------Question 3------------------------------------*)
let rec G (m,n) =
    match n with
    |n when n<= 0 -> 
        n+m
    |n when n>0 ->
        G((2*m), (n-1)) + m

G(10,10)

(*
G is not tail-recursive because m is added to each recursive call. This results in 
building up the stack
*)

let GA (m,n)=
    let rec GAcc (m,n) acc=
        match n with
        |n when n<= 0 -> 
            acc+(n+m)
        |n when n>0 ->
            GAcc ((2*m), (n-1)) (acc+m)
    GAcc (m,n) 0

//3.2
let mySeq = seq{
    for i in [1..100] do
        for j in [1..100] do
            yield (i,j)
}

let gSeq = seq{
    for i in [1..100] do
        for j in [1..100] do
            yield GA(i,j)
}

