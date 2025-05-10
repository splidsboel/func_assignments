(*----------------------------------------Question 1----------------------------------------*)
type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>
//1.1
let psEx = PrioritySet ["a";"b";"c"]

let priSetEx = PrioritySet ["a"; "q"; "b"; "d"]
//type is PrioritySet<String>

let empty = PrioritySet (List.empty)

//1.2
let isEmpty (PrioritySet items) = 
    List.isEmpty items

let size (PrioritySet items) =
    List.length items

let contains e (PrioritySet ps) = 
    if (List.forall (fun x -> x<>e) ps) then false else true

let getPN e  (PrioritySet ps) =
    let rec getPNA e ps acc =
        match ps with
        |x::xs -> 
            if e =x then acc else getPNA e xs (acc+1)
        |_ -> failwith "did not find e"
    getPNA e ps 1


//1.3
let remove e (PrioritySet ps) =
    let rec removeA e ps acc =
        match (PrioritySet ps) with
        |PrioritySet [] -> List.rev acc
        |PrioritySet (x::xs) ->
            if e = x then (removeA e xs acc)
            else removeA e xs (x::acc)
    removeA e ps []

let add e (PrioritySet ps) =
    if not (contains e (PrioritySet ps)) then List.append ps [e] else ps


let map f (PrioritySet ps) = PrioritySet(List.map f ps)

let cp (PrioritySet ps1) (PrioritySet ps2) = PrioritySet(List.zip ps1 ps2)

(*------------------------------------------------Question 2------------------------------------------------*)
let f curRow =
    let rec f' = function
        [] -> []
        | [_] -> [1]
        | xs -> 
            let (x1::x2::xs) = xs
            x1 + x2 :: f' (x2::xs)
    (1 :: f' curRow)
//2.1
(*
The function computes a list of fibonacci numbers. When there is only one element left in the list,
1 is appended to the list. 1 is consed to the beginning of the function call to f', therefore 1 and 1 are always the 
beginning and end of the list
*)

//2.2
let fMatch curRow=
    let rec fMatch' = function
        | [] -> []
        | [_] -> [1]
        | x1 :: x2 :: xs -> x1 + x2 :: fMatch' (x2 :: xs)
    (1:: fMatch' curRow)
(*
Warning disappears for cases of xs with less than two elements because we include the
deconstruction of the list inside the pattern match
*)

//2.3
let rec fA' curRow acc =
    match curRow with
    |[] -> List.rev acc
    |[_] -> [1]
    |x1::x2::xs -> fA' xs ((x1+x2)::acc)



(*--------------------------------------------------Question 3--------------------------------------------------*)

let mySeq s1 s2 =
    seq { for e1 in s1 do
                        for e2 in s2 do
                            yield! [e1;e2] }


(*
In mySeq, for every iteration of the inner loop, it yields both e1 and e2 as separate elements into the sequence.
*)

let mySeq2 s1 s2 =
        seq { for e1 in s1 do
                        for e2 in s2 do
                            yield (e1,e2) }

mySeq2 [1;2] ['A';'B';'C']

let mySeq3 n = Seq.initInfinite (fun i -> (n*n)-n*i)


(*--------------------------------------------------------------Question 4*--------------------------------------------------------------*)

type DataSpec =
    RangeInt of int * int
    | ChoiceString of string list
    | StringSeq of string
    | Pair of DataSpec * DataSpec
    | Repeat of int * DataSpec

let reg =
    Repeat(3,Pair(StringSeq "a",
        Pair(ChoiceString["cheese";"herring";"soft drink"],
            RangeInt(1,100))))  

//4.1
let pur = Repeat(2, (Pair(RangeInt(1,10), StringSeq "a")))

//4.2
let rand = System.Random()
let next(i1,i2) = rand.Next(i1,i2)
let numGen =
    let n = ref 0
    fun () -> n := !n+1; !n

(* let rec genValue = function
    RangeInt(i1,i2) -> next(i1,i2).ToString()
    |ChoiceString xs -> xs.[(next (0, (List.length xs)))]
    |Pair(ds1, ds2) -> string ((genValue ds1), (genValue ds2))
    |Repeat(i, ds) -> 
        match i with
        |0 -> string ds
        |n -> 
            string ((genValue ds)::(genValue Repeat((n-1), ds)))

    |StringSeq(s) -> string s.[next(0, (String.length s-1))]

genValue reg *)