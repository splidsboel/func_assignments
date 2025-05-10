type multimap<'a,'b when 'a: comparison> =
    MMap of Map<'a,list<'b>>

let ex = MMap (Map.ofList [("record",[50]);("ordering",[36;46;70])])

let studReg =
    MMap (Map.ofList [
            ("Grete", []);
            ("Hans", ["HOPS";  "TOPS"]);
            ("Peter", ["IFFY"]);
            ("Sine", ["BFNP"; "HOPS"; "IFFY"])
        ])

let studReg2 =
    MMap (Map.ofList [
            ("Hans", ["HOPS";  "TOPS"]);
            ("Grethe", []);
            ("Sine", ["BFNP"; "HOPS"; "IFFY"])
            ("Peter", ["IFFY"]);
    ])

(* Map comparison
You cannot declare another studReg2 with the same registrations that gives a false
comparison to studReg, because maps in F# compare the keys and values of the map, 
not by the ordering. Therefore, if the map has the same keys and values (which is 
required in the question), it is not possible
*)

//1.2

let canonical (MMap m) =
    MMap (Map.map (fun k v -> List.sort v) m)

//Map.toList
//Map.toList. Map.toList always returns the list of key-value pairs ordered by the keys.
let toOrderedList (MMap m) =
    Map.toList m

let studRegOrdered = toOrderedList studReg

//1.3
let newMultimap =
    MMap(Map.empty)

let sizeMultimap (MMap m) =
    let rec sizeMultiMap m kAcc vAcc =
        match Map.tryFindKey (fun _ _ -> true) m with
        | Some key ->
            let value = m.[key]
            let remainingMap = Map.remove key m
            sizeMultiMap remainingMap (kAcc+1) (vAcc+(List.length value))
        | None -> (kAcc, vAcc)
    sizeMultiMap m 0 0

//1.4
let addMultimap k v (MMap m) =
    match Map.tryFind k m with
    |Some value -> 
        if List.contains v value then MMap(m) 
        else MMap (Map.add k (v::value) m)
    |None -> MMap (Map.add k [v] m)

sizeMultimap (addMultimap "Grete" "TIPS" studReg) = (4,7)
sizeMultimap (addMultimap "Sine" "BFNP" studReg) = (4,6)
sizeMultimap (addMultimap "Pia" "" studReg) = (5,7)

let removeMultimap k vOpt (MMap m) =
    match vOpt with
    |None -> MMap(Map.remove k m)
    |Some v -> 
        match Map.tryFind k m with
        |Some vList -> 
            let newList = List.filter ((<>)v) vList
            MMap(Map.add k newList m)
        |None -> MMap m

sizeMultimap (removeMultimap "Sine" None studReg) = (3,3)
sizeMultimap (removeMultimap "Sine" (Some "PLUR") studReg) = (4,6)
sizeMultimap (removeMultimap "Kenneth" (Some "BLOB") studReg) = (4,6)
sizeMultimap (removeMultimap "Peter" (Some "IFFY") studReg) = (4,5)

//1.5
let mapMultimap f (MMap m) =
    MMap (Map.map (fun k vs -> List.map (fun v -> f k v) vs) m)


mapMultimap (fun k v -> v+"-F2015") studReg

//1.6



(*-------------------question2---------------------*)
//2.1
let rec f i j xs =
    if xs = [] then
        [i*j]
    else
        let (x::xs') = xs
        x*i :: f (i*j) (-1*j) xs'

f 10 1 [1..9]


(*------------------------------------Question 3------------------------------------*)
//3.1
let myFinSeq n m = seq { 
    for i in [n .. m] do
        yield [n .. i] }

myFinSeq 2 4 |> Seq.toList

(*
myFinSeq returns a sequence of lists all starting from n and going towards m 
increasing in length by one until the last element in the list is equal to m *)

myFinSeq 10 14 |> Seq.toList
//contains 3 12's

//3.2
let myFinSeq2 n m = seq{
    for i in [n..m] do
        yield! [n..i]
}

myFinSeq2 3 6 |> Seq.toList

//3.3
let sum xs = List.fold (fun r x -> r+x) 0 xs
let seq4000 = myFinSeq 10 4000
let array4000 = Array.ofSeq seq4000


array4000 |> Array.length //contains 3991 lists

let sums = Array.map sum array4000

let sumAlt = Array.Parallel.map sum array4000

(*--------------------------------------Question 4--------------------------------------*)
type JSONlite =
    Object of list<string * Value>
and Value =
    String of string
    | Record of JSONlite
    | Label of string * Value
    | Ref of string

let address = Object [("Street", String "Hansedalen");
    ("HouseNo", String "27")]
let person1 = Object [("Name", String "Hans Pedersen");
    ("Address", Label ("Addr1", Record address))]
let person2 = Object [("Name", String "Pia Pedersen");
    ("Address", Ref "Addr1")]
let persons = Object [("Person1", Record person1);
    ("Person2", Record person2)]

//4.1
let student =
    let course =Object [("BFNP", String "10"); ("BPRF", String "7")]
    Object[
        ("Name", String "Per Simonsen");
        ("Field", String "BSWU");
        ("Course", Record course)
    ]

//4.2
let nl = System.Environment.NewLine // New line
let space n = String.replicate n " " // Make n spaces
let ppQuote s = "\"" + s + "\"" // Put quotes around string s
let ppJSONlite json =
    let rec ppValue' indent = function
        String s -> ...
        | ...
and ppJSONlite' indent = function
    Object xs -> ...
    ppJSONlite' 0 json