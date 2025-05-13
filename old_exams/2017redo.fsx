(*-------------------------------------Question1--------------------------------------*)
type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>

//1.1
let psEx = PrioritySet ["a";"b";"c"]

let priSetEx= PrioritySet["a";"q";"b";"d"]
//type is priSetEx<string> because the list is comprised of strings

let empty = PrioritySet(List.empty)

//1.2
let isEmpty xs =
    xs=empty

let size (PrioritySet xs) =
    List.length xs

let contains e (PrioritySet ps)=
    List.contains e ps

let getPN e ps=
    let rec getPNI e (PrioritySet ps) i =
        match ps with 
        |[] -> failwith $"{e} does not exist in {ps}"
        |x::xs ->
            if x=e then i else getPNI e (PrioritySet xs) (i+1)
    getPNI e ps 1

//1.3
let remove e ps =
    let rec removeA e (PrioritySet ps) acc =
        match ps with
        |[] -> PrioritySet (List.rev acc)
        |x::xs ->
            if x=e then removeA e (PrioritySet xs) acc
            else removeA e (PrioritySet xs) (x::acc)
    removeA e ps []

remove "b" psEx

let add e (PrioritySet ps) =
    if List.contains e ps then (PrioritySet ps) 
    else PrioritySet([e]@ps)

//1.4
let map f (PrioritySet ps) =
    PrioritySet(List.map f ps)

let cp (PrioritySet ps1) (PrioritySet ps2) = 
    seq {
    for i in ps1 do
        for j in ps2 do
            yield (i,j)
    } |> Seq.toList



(*------------------------------------------------------------Question 2------------------------------------------------------------*)


let f curRow =
    let rec f' = function
        [] -> []
        | [_] -> [1]
        | xs -> 
            let (x1::x2::xs) = xs
            x1 + x2 :: f' (x2::xs)
    (1 :: f' curRow)

(*--------------------------------------------------------Question 3*--------------------------------------------------------*)
let mySeq s1 s2 =
    seq { for e1 in s1 do
            for e2 in s2 do
                yield! [e1;e2] }
(*
Returns a sequence with each element in s1 concatenated with each element in e2
*)

//mySeq seq['A'; 'B'] seq['D';'E';'F'] |> Seq.toList

//3.2
let mySeq2 s1 s2 = 
    seq { for e1 in s1 do
            for e2 in s2 do
                yield (e1, e2) }

//3.3
let mySeq3 n = Seq.initInfinite (fun i -> (n*n)-n*i)

(*---------------------------------------------------------Question 4---------------------------------------------------------*)
type DataSpec =
    RangeInt of int * int
    | ChoiceString of string list
    | StringSeq of string
    | Pair of DataSpec * DataSpec
    | Repeat of int * DataSpec
    | Pick of string
    | Label of string * DataSpec

let regEx = [("a1",("cheese",25));
    ("a2",("herring",4));
    ("a3",("soft drink",5))]

let reg =
    Repeat(3,
        Pair(StringSeq "a",
            Pair(
                ChoiceString["cheese";"herring";"soft drink"], RangeInt(1,100))))


let pur =
    Repeat(2, 
        Pair(RangeInt (1, 10), StringSeq "a"))

//4.2
let rand = System.Random()
let next(i1,i2) = rand.Next(i1,i2)
let numGen =
    let n = ref 0
    fun () -> n := !n+1; !n

let rec genValue = function
    | RangeInt (i1, i2) ->
        next(i1, i2).ToString()
    
    | ChoiceString xs ->
        let index = next(0, List.length xs)
        xs.[index]
    
    | StringSeq prefix ->
        let id = numGen()
        prefix + id.ToString()
    
    | Pair (ds1, ds2) ->
        let v1 = genValue ds1
        let v2 = genValue ds2
        $"({v1},{v2})"
    
    | Repeat (n, ds) ->
        let values = List.init n (fun _ -> genValue ds)
        "[" + (String.concat ";" values) + "]"



//4.3
let reg2 = Repeat(3,Pair(Label("articleCode",StringSeq "a"),
    Pair(ChoiceString["cheese";"herring";"soft drink"],
    RangeInt(1,100))))
let pur2 = Repeat(2,Pair(RangeInt(1,10), Pick "articleCode"))

