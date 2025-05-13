type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>

let ex = MSet (Map.ofList [("a",1);("b",2);("c",1)])

let wrong = MSet (Map.ofList [("a",0);("b",2);("c",1)])

let diceSet =
    MSet (Map.ofList [
        (1, 2);
        (2, 1);
        (3, 5);
        (5,2);
        (6,6)
    ])

//type is MultiSet<int> because the keys in the map are ints

(*
You cannot do a Multiset of mathematical functions, because you cannot use
functions as a key in a map*)

//1.2
//unit function
let newMultiset<'a> () = MSet (Map.empty)

let isEmpty (MSet m) =
    if m=Map.empty then true else false

//1.3
let add k (MSet ms) =
    MSet (Map.change "a" (function Some v -> Some (v + 1) | None -> Some 1) ms)

let del k (MSet ms) =
    if not (Map.containsKey k ms) then MSet ms else
    MSet(Map.remove k ms)

//1.4
let toList (MSet ms) = 
    let list = Map.toList ms
    let rec toListA xs acc =
        match xs with
        |[] -> List.rev acc
        |(k,v)::xs ->
            toListA xs ((List.init v (fun _ -> k)) @ acc)
    toListA list []

let fromList xs = 
    let rec fromListA xs acc =
        match xs with
        |[] -> acc
        |x'::xs' -> 
            fromListA xs' (add x' acc)
    fromListA xs (newMultiset())
    

    



