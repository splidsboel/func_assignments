type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>

let ex = MSet (Map.ofList [("a",1);("b",2);("c",1)])

let wrong = MSet (Map.ofList [("a",0);("b",2);("c",1)])

let diceSet =
    MSet ( Map.ofList [(1, 2);(2, 1);(3, 5);(5,2);(6,2)] )

//type of diceSet is Multiset<int>

(*
You cannot do a Multiset of mathematical functions, because you cannot use
functions as a key in a map*)

let newMultiset =
    MSet Map.empty

let isEmpty m = m=newMultiset

//inserting into map
let add k (MSet ms) = 
    MSet (Map.change k (function Some v -> Some (v + 1) | None -> Some 1) ms)

//delete from map. Map.remove
let del k (MSet ms) = MSet(Map.remove k ms)

let toList (MSet ms) = Map.toList ms

let fromList xs = 
    let rec fromListA xs acc =
        match xs with
        |[] -> acc
        |x'::xs' -> 
            fromListA xs' (add x' acc)
    fromListA xs newMultiset

//changing keys in a map. Map.fold
let map f (MSet ms) =
    ms |> 
        Map.fold (fun acc key value -> Map.add (f key) value acc) Map.empty |> MSet

//List.collect
let fold f acc (MSet ms) =
    ms 
    |> Map.toList
    |> List.collect (fun (key, count) -> List.init count (fun _ -> key))
    |> List.fold f acc
        
//union

(*--------------------------------------------------------------Question 2--------------------------------------------------------------*)

let rec f n =
    if n < 10 then "f" + g (n+1) else "f"
and g n =
    if n < 10 then "g" + f (n+1) else "g"

(*
all arguments n to f will generate a result string that starts and ends with f since
that for all n>= 10, it will hit the else statement, and "f" is always added to the start
for n<10
*)

g 4 //will return "gfgfgfg"

(*
Since the type of n for f and g is int, there is no way to start an infinite
computation. You can only give System.int32.MaxValue which is not infinite.
If the type was double, you could do System.Double.PositiveInfinity and get
an infinite computation*)

let rec fA n acc =
    if n<10 then gA (n+1) ("f"+acc) else "f"
and gA n acc =
    if n<10 then fA (n+1) ("g"+acc) else "g"

(*------------------------------------------Question 3------------------------------------------*)
//3.1
let myFinSeq (n,m) = 
    seq { 
        for i in [0 .. n] do
            yield! 
                seq { for j in [0 .. m] do yield j }}

myFinSeq (1,2) |>Seq.toList

(*
The sequence returned by myFinSeq yields the sequence 0..m repeated n+1 times
*)

(*
You can never get that answer because it repeats the same sequence 
(would be [0;1;2]), therefore you can never get a sequence without the last "2"
*)

//3.2
let myFinSeq2 (n,m) =
    seq{
        for i in [0..n] do
            yield 
                (i, seq { for j in [0 .. m] do yield j })
    }

myFinSeq2 (2,1) |> Seq.toList

(*------------------------------------Question 4------------------------------------*)
type Row = int
type Col = char
type CellAddr = Row * Col
type ArithOp = Add | Sub | Mul | Div
type RangeOp = Sum | Count
type CellDef =
    FCst of float
    | SCst of string
    | Ref of CellAddr
    | RangeOp of CellAddr * CellAddr * RangeOp
    | ArithOp of CellDef * ArithOp * CellDef
type CellValue =
    S of string
    | F of float
type Sheet = Map<CellAddr,CellDef>

let header = 
    [((1,'A'),SCst "#EYES");((1,'B'),SCst "1");((1,'C'),SCst "2");
    ((1,'D'),SCst "3");((1,'E'),SCst "4");((1,'F'),SCst "5");
    ((1,'G'),SCst "6");((1,'H'),SCst "Total")]
let result = 
    [((2,'A'),SCst "RESULT");((2,'B'),FCst 2.0);((2,'C'),FCst 1.0);
    ((2,'D'),FCst 5.0);((2,'E'),FCst 0.0);((2,'F'),FCst 2.0);
    ((2,'G'),FCst 2.0);((2,'H'),RangeOp((2,'B'),(2,'G'),Sum))]
let calcPct col = ArithOp(FCst 100.0, Mul, ArithOp(Ref(2,col),Div,Ref(2,'H')))
let pct = 
    [((3,'A'),SCst "PCT");((3,'B'),calcPct 'B');((3,'C'),calcPct 'C');
    ((3,'D'),calcPct 'D');((3,'E'),calcPct 'E');((3,'F'),calcPct 'F');
    ((3,'G'),calcPct 'G');((3,'H'),calcPct 'H')]
let dice = Map.ofList (header @ result @ pct)

let heights =
    let col1 =
        [
            ((4, 'B'), SCst "NAME");
            ((5,'B'), SCst "Hans"); 
            ((6, 'B'), SCst "Trine");
            ((7,'B'), SCst "Peter")
            ((9, 'B'), RangeOp((2, 'B'), (7, 'B'), Count))
        ]
    let col2 =
        [
            ((4, 'C'), SCst "HEIGHT");
            ((5, 'C'), FCst 167.40);
            ((6, 'C'), FCst 162.30);
            ((7, 'C'), FCst 179.70);
            ((9, 'C'), ArithOp((RangeOp((5, 'C'), (7, 'C'), Sum)), Div, Ref(9,'B')))
        ]
    
    Map.ofList(col1@col2)

let getF = function
    F f -> f
    | S s -> failwith "getF: expecting a float but got a string"


let evalRangeOp xs op =
    if op=Count then float (List.length xs)
    else 
        let rec sumList xs acc =
            match xs with
            |[] -> acc
            |x'::xs' ->
                sumList xs' (acc+(getF x'))
        sumList xs 0

let evalArithOp v1 v2 op =
    if op = Add then (getF v1) + (getF v2)
    else if op = Sub then (getF v1) - (getF v2)
    else if op = Mul then (getF v1) * (getF v2)
    else (getF v1)/(getF v2)


let rec evalValue v sheet =
    match v with
    FCst f -> F f
    | SCst s -> S s
    | Ref ca -> evalCell ca sheet
    | RangeOp ((r1,c1),(r2,c2),op) -> 
        let cellAddrList =
            [for r in [r1..r2] do
                for c in [c1..c2] do
                    yield (r, c)]
            |> List.map (fun cell -> evalCell cell sheet)
        F (evalRangeOp cellAddrList op)
    | ArithOp (v1,op,v2) -> 
        let ev1 = evalValue v1 sheet
        let ev2 = evalValue v2 sheet
        F(evalArithOp ev1 ev2 op)
and evalCell ca sheet =
    match Map.tryFind ca sheet with
    None -> S "" // We define an empty cell to be the empty string value.
    | Some v -> evalValue v sheet


//4.4
