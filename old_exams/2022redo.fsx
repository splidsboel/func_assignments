type TrashItem<'a> =
    Paper of string
    | Glass of string
    | Other of 'a

let item1 = Other ("Shirt", "Clothes")
let item2 = Paper "Newspaper"
let item3 = Other ("Sneakers", "Shoe")
let item4 = Glass "Wine glass"

//type of1 item is TrashItem<string * string> because its category is other. The first string
//in the tuple describes the name and the second the category

(*
Type of item2 is TrashItem<string> because the type for the category Paper can 
only be string as per the above type declaration
*)

(*
TrashItem is polymorphic because Other can contain any type*)

let items =[
    Paper "Magasine";
    Glass "Bottle";
    Glass "Jam";
    Other ("Beer can", "Aluminium");
    Other ("Bag", "Plastic")
]

let fnPP (n, t) = $"{t} ({n})"

let ppTrashItem fnPP item =
    match item with
    |Other (n, t) ->
        fnPP (n,t)
    |Glass s -> $"Glass ({s})"
    |Paper s -> $"Paper ({s})"

let isPaper item =
    match item with 
    |Paper s -> true
    |_ -> false

//1.2. 
type TrashCan<'a> =
    Empty
    | TrashItems of TrashItem<'a> * TrashCan<'a>

let rec addItem item tc =
    match tc with
    |Empty -> TrashItems (item, tc)
    |TrashItems (i, c) -> TrashItems (i, addItem item c)

let ofList ts =
    List.fold (fun acc x -> addItem x acc) Empty ts

let tcEx = ofList items
let rec forAll fnP tc =
    match tc with
    Empty -> true
    |TrashItems (i, c) -> 
        if fnP i then false else forAll fnP c

let isSameCategory item1 item2 =
    match item1, item2 with
    |Glass _, Glass _ -> true
    |Paper _, Paper _ -> true
    |Other _, Other _ -> true 
    |_ -> false

let isSorted tc =
    let peekTc tc =
        match tc with
        |TrashItems (i, c) -> Some i
        |Empty -> None
    ()


//1.3
let rec fold f e tc =
    match tc with
    |Empty -> e
    |TrashItems(i, c) ->
        let newAcc = f e i
        fold f newAcc c

let sort tc =
    let rec sortA tc (ppTc, glasstc, otherTC) =
        match tc with
        |Empty -> (ppTc, glasstc, otherTC)
        |TrashItems (Glass i, tcx) ->
            sortA tcx (ppTc, (addItem (Glass i) glasstc), otherTC)
        |TrashItems (Paper i, tcx)->
            sortA tcx ((addItem (Paper i) ppTc, glasstc, otherTC))
        |TrashItems (Other i, tcx) ->
            sortA tcx (ppTc, glasstc, (addItem (Other i) otherTC))
    sortA tc (Empty, Empty, Empty)


(*------------------------------------------Question 2------------------------------------------*)
type Node<'a> = 
    Root of 'a
    | Link of 'a * Node<'a> ref

let mkRootElem a = ref (Root a)
let mkLinkElem a e =
    ref (Link(a,e))

let elemA = mkRootElem 'A'
let elemB =
    mkLinkElem 'B' elemA
let elemC =
    mkLinkElem 'C' elemB
let elemM = mkRootElem 'M'
let elemN =
    mkLinkElem 'N' elemM

//2.1
let getVal e =
    match !e with
    |Root x -> x
    |Link (x, _)->
        x

let pathLength e =
    let rec pathLengthA e acc =
        match !e with
        |Root _ -> acc
        |Link (_, y) ->
            pathLengthA y (acc+1)
    pathLengthA e 0


//2.2
let rec find e =
    match !e with
    |Root _ -> e
    |Link (_, root)->
        find root


find elemA = find elemB 
//true because root of elemA is elemA and root of elemB is elemA

find elemA = elemA 
//true because elemA's root is a reference to itself


let union e1 e2 =
    let e1' = find e1
    let e2' = find e2
    match !e2' with
        Root v -> Link(v, e1')
        | Link _ ->
        failwith "union: got Link after find."

(*------------------------------------Question 3------------------------------------*)
let rec f x = function
    [] -> []
    | y::ys when x=y -> f x ys
    | y::ys when x <> y ->
    match f x ys with
    ys' -> y::ys'

(*
type is 'a -> 'a list -> 'a lest when 'a: equality
because it compares x to the elements in the list.

*)

