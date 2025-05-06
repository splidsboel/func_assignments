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


(*------------------------------------Question 2------------------------------------*)
//2.1
let even n = if n%2=0 then true else false

let collatz n =
    match n with
    |n when (even n) -> n/2
    |n -> 3*n+1

let collatz' n =
    match n with
    |n when (even n) && n<>0 -> n/2
    |n when n>0 -> 3*n+1
    |_ -> failwith "n is zero or less"

//2.2
let applyN f n N = 
    let rec applyNacc f n N acc =
        match N with
        |0 -> n::acc
        |_ -> applyNacc f (f n) (N-1) (n::acc)
    List.rev(applyNacc f n N [])

let applyUntilOne f n =
    let rec applyUntilOneX f n i = 
        match i with
        |0 -> failwith "should not get here"
        |_ -> if n=1 then 100000000-i else applyUntilOneX f (f n) (i-1)
    applyUntilOneX f n 100000000


//2.3
let rec mySeq f x =
    seq{ 
        yield x
        yield! mySeq f (f x)
    }

Seq.take 20(mySeq collatz 42) |> Seq.toList
(*
The function returns an infinite sequence with x being the first number
followed by the rest of the sequence recursively calculated. This basically
just returns the collatz sequence 
*)

let rec g x = 
    seq{
        yield x
        yield! g (x*2)
    }

(*----------------------------------------Question 3----------------------------------------*)
type name = string
type quantity = float
type date = int * int * int
type price = float
type transType = Buy | Sell
type transData = date * quantity * price * transType
type trans = name * transData

let ts : trans list =
    [("ISS", ((24,02,2014),100.0,218.99,Buy)); ("Lego",((16,03,2015),250.0,206.72,Buy));
    ("ISS", ((23,02,2016),825.0,280.23,Buy)); ("Lego",((08,03,2016),370.0,280.23,Buy));
    ("ISS", ((24,02,2017),906.0,379.46,Buy)); ("Lego",((09,11,2017), 80.0,360.81,Sell));
    ("ISS", ((09,11,2017),146.0,360.81,Sell)); ("Lego",((14,11,2017),140.0,376.55,Sell));
    ("Lego",((20,02,2018),800.0,402.99,Buy)); ("Lego",((02,05,2018),222.0,451.80,Sell));
    ("ISS", ((22,05,2018),400.0,493.60,Buy)); ("ISS", ((19,09,2018),550.0,564.00,Buy));
    ("Lego",((27,03,2019),325.0,625.00,Sell)); ("ISS", ((25,11,2019),200.0,680.50,Sell));
    ("Lego",((18,02,2020),300.0,720.00,Sell))]

//Map.tryFind. matching on options
let addTransToMap (t:trans) (m: Map<name, transData list>) =
    match t with
    |(k,v) -> 
        match (Map.tryFind k m) with
        |None -> Map.add k (v::[]) m
        |Some x -> Map.add k (v::x) m

let m1 = addTransToMap ("ISS", ((24,02,2014),100.0,218.99,Buy)) Map.empty
let m2 = addTransToMap ("ISS", ((22,05,2018),400.0,493.60,Buy)) m1

let shares = List.foldBack addTransToMap ts

//3.2



(*------------------------------------------------Question 4------------------------------------------------*)

//4.1
let rec dup = function
    |[]->[]
    |x::xs ->x::x::dup xs

(*
The dup function returns a list with each element duplicated. 
For instance (dup [1;2;3]) would return [1;1;2;2;3;3]
*)
let rec dupA acc = function
    |[]-> List.rev acc
    |x::xs -> dupA (x::x::acc) xs


//4.2
//constructing finite sequence
let replicate2 i = 
    seq{
        yield i
        yield i
    }

//infinite sequence of duplicate numbers. duplicate numbers in sequence
let dupSeq  = Seq.initInfinite (fun i -> i/2)

//4.3
let dupSeq2 s =
    seq{
        for i in s do
            yield i
            yield i
    }

