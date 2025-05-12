(*------------------------------------Problem 1------------------------------------------------*)

type Multiset<'a when 'a : equality> = ('a * int) list

let test:Multiset<'a> = [("b", 3); ("a", 5); ("d", 1)]

//check if list contains duplicates. List.forall. Anonymous function with tuple
let rec inv (ms: Multiset<'a>) =
    match ms with
    |[] -> true
    |(e, _) :: xs ->
        if List.forall (fun (x, _) -> x <> e) xs then inv xs
        else false

//insert element into list of tuples. List.map
let insert e n (ms: Multiset<'a>) : Multiset<'a> =
    ms |> List.map (fun(str, i) ->
        if str = e then (str, i+n) else (str, i))

//find element in list of tuples
let rec numberOf e (ms:Multiset<'a>) =
    match ms with
    |[] -> failwith $"Multiset does not contain {e}"
    |(s, i)::xs -> 
        if s=e then i
        else numberOf e xs

//smarter way with List.tryFind
let numberOfBetter e (ms: Multiset<'a>) =
    match List.tryFind (fun (s, _) -> s = e) ms with
    | Some (_, i) -> i
    | None -> failwith $"Multiset does not contain {e}"

//smarter way with piping and using the option type. 
(*
Option.map applies a function only if there is a Some (not None)
snd returns second element in a tuple
*)
let numberOfEvenBetter e (ms:Multiset<'a>) = ms |> List.tryFind (fun(s,_) -> s=e) |> Option.map snd

//remove tuple from list of tuples. List.choose
let delete e (ms: Multiset<'a>) : Multiset<'a>=
    ms |> List.choose (fun (str, i) ->
        if str=e then 
            if 1> 1 then Some (str, i-1)
            else None
        else Some (str, i))

//List.fold. Combine two lists of tuples
let union ((ms1:Multiset<'a>), (ms2:Multiset<'a>)) :Multiset<'a> =
    List.fold (fun acc (e,n) -> insert e n acc ) ms1 ms2

type multisetMap<'a when 'a: comparison> = Map<'a, int>

//Map.forall
let invMap msm =  Map.forall (fun k v -> v>0) msm

//Map.change. Insert kv pair dependant on existing map
let insertMap e n (msm:multisetMap<'a>) :multisetMap<'a> = 
    Map.change e (fun opt ->
        match opt with
        |Some v -> Some (v+n)
        |None -> Some n) msm

//Map.fold. Combine two maps
let unionMap ms1 ms2 = 
    Map.fold (fun acc key count ->
        let existing = Map.tryFind  key acc |> Option.defaultValue 0
        Map.add key (existing+count) acc
        ) ms1 ms2


(*------------------------------------Problem 2------------------------------------------------*)
let rec f i = function
    |[] ->[]
    |x::xs -> (i,x) :: f(i*i) xs

//type is int-> 'a list -> (int * 'a) list

(* 
f takes an integer and a list and returns a list of tuples. The tuple consists of the original element
from the list gives as an argument and i multiplied by itself x times dependant on where the element 
was in the original list
*)

type 'a Tree = 
    |Lf
    |Br of 'a Tree * 'a * 'a Tree

let rec g p = function 
    |Lf -> None
    |Br(_, a, t) when p a -> Some t
    |Br(t1, a, t2) ->
        match g p t1  with
            |None -> g p t2
            |res -> res

//type is: ('a -> bool) -> 'a Tree -> 'a Tree option

(*
g searches a tree (type declared in problem description) for a node that satisfies the predicate p. 
It then returns the right subtree of that node (None, if it does not have a subtree). 
The way of traversal is pre-order traversal. 
*)

//tail-recursive list building
let rec fA acc i li =
    let rec loop acc i li =
        match li with
        |[] -> List.rev acc
        |x::xs -> loop ((i,x)::acc) (i*i) xs
    loop [] i li

//continuation on lists
let rec fC i li c =
    match li with 
    |[] -> c []
    |x::xs -> fC (i*i) xs (fun acc -> c ((i,x)::acc))


let rec h f (n, e) = 
    match n with
    | 0 -> e 
    | _ -> h f (n-1, f n e)

//type of h is 

let A = Seq.initInfinite id

let B = seq {
    for i in A do 
        for j in seq{0 .. i} do
            yield (i,j)     }

let C = seq { 
    for i in A do
        for j in seq{0..i} do
            yield (i-j, j)
    }

let X = Seq.toList (Seq.take 4 A)

let Y = Seq.toList (Seq.take 6 B)

let Z = Seq.toList (Seq.take 10 C)

h ( * ) (4,1)



(*--------------------------Problem 3--------------------------*)

type Title = string

type Section = Title * Elem list
and Elem = Par of string | Sub of Section

type Chapter = Title * Section list
type Book = Chapter list
let maxL ls = 
    match ls with
    |[] -> 0
    |ls -> ls |> List.sortDescending |> List.head

let overview (bk:Book) =
    let rec overviewA acc bk =
        match bk with
        |[] -> acc
        |x::xs ->
            match x with
            |(title, _) -> 
                overviewA (title::acc) xs
    overviewA [] bk

