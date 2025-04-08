(*------------------------------Question 1------------------------------*)
//type declaration for question 1
type TrashItem<'a> =
    | Paper of string
    | Glass of string
    | Other of 'a


//1.1
//example types
let item1 = Other ("Shirt", "Clothes")
let item2 = Paper "Newspaper"
let item3 = Other ("Sneakers", "Shoe")
let item4 = Glass "Wine glass"

(* 
type of item1 is TrashItem<string * string> 
In the type declaration Other can be anything of a TrashItem, 
so since we in this case give it a tuple of strings,
the type is a TrashItem of a tuple of Strings
*)

(*
type of item2 is TrashItem<'a>, because the type cannot be inferred from 
the information given. A trashitem of a string could either be glass or paper.
*)

(* 
The TrashItem type is polymorphic because it can be either a string or anything else
depending on whether it is Paper/Glass or Other
*)

let items = [Paper("Magasine"); Glass("Bottle"); Glass("Jam"); Other("Beer can", "Aluminium"); Other("Bag", "Plastic")]

let fnPP (n,t) = $"{t} ({n})"

let ppTrashItem fnPP item = 
    match item with
    |Paper(s) -> $"Paper ({s})"
    |Glass(s) -> $"Glass ({s})"
    |Other(n,t) -> fnPP(n,t)

//tests. Output correctly 😎
ppTrashItem fnPP item1
ppTrashItem fnPP item2
ppTrashItem fnPP item4


let isPaper item = 
    match item with
    |Paper(s) -> true
    |_ -> false

//tests 
isPaper item1
isPaper item2


//1.2
type TrashCan<'a> =
    Empty
    | TrashItems of TrashItem<'a> * TrashCan<'a>


let addItem item tc = 
    TrashItems(item, tc)


let ofList ts = List.foldBack addItem ts Empty

let tcEx = ofList items

let rec forAll fnP tc = 
    match tc with
    |Empty -> true
    |TrashItems (ts, trashc) -> 
        if not (fnP ts) then false else forAll fnP trashc

//tests:
forAll isPaper Empty
forAll isPaper tcEx

let isSameCategory item1 item2 = 
    match item1, item2 with
    |Paper _, Paper _ -> true
    |Glass _, Glass _ -> true
    |Other _, Other _ -> true
    |_ -> false

let isSorted tc = 
    match tc with 
    | Empty -> true
    | TrashItems (first, rest) ->
        forAll (fun item -> isSameCategory first item) rest


//1.3
//slap af
(*------------------------------Question 2------------------------------*)

type Node<'a> = 
    | Root of 'a
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
let rec getVal e =
    match !e with
    |Root(a) -> a
    |Link(v, next) -> v


let rec pathLength e = 
    match !e with
    |Root(a) -> 0
    |Link(v, next) -> 1+ pathLength next

List.map pathLength [elemA; elemB; elemC]

//2.2
let rec find e = 
    match !e with
    |Root _  -> e
    |Link (_, next) -> find next

//re: the table
find elemA = find elemB //returns true because they are in the same "set" in the data structure. 
// in F# land, it is because elemB was made as a linkElem with elemA as its next node

find elemB = elemA //true for the same reason. "=" is symmetric. find elemA is just elemA

find elemA =  elemA //returns true because they have the same root node because they are root nodes themselves

find elemB = elemB //false because elemB is not the root of elemB

find elemC = find elemN //false because they are not in the same set, so don't share the same root node

let union e1 e2 = 
    let e1' = find e1
    let e2' = find e2
    match e2'.Value with
    |Root v -> 
        e2'.Value <- Link(v, e1')
        e1'
    |Link _ ->
        failwith "union: got Link after find. "


(*----------------------------------------Question 3----------------------------------------*)


//3.1
let rec f x = function
    [] -> []
    |y::ys when x=y -> f x ys
    |y::ys when x <> y ->
        match f x ys with
        ys' -> y::ys'


(*
type for f is:  'a -> 'a list -> 'a list when 'a: equality
f removes all x's from a list y
*)

//examples:
f 1 [1; 1; 2;3]
f 2 [1;2;3]
f 3 [1;2;3]

//appropriate name could be removeX, since it removes x's from a list

//3.2
(*
The pattern match warning comes from the 'match' expression because it is matching against
a function, not a constructor or wildcard. 
*)

let rec f2 x = function
    [] -> []
    |y::ys when x=y -> f x ys
    |y::ys -> y:: f x ys

f2 1 [1; 1; 2;3]
f2 2 [1;2;3]
f2 3 [1;2;3]



//3.3
// f is not tail-recursive because the recursive calls with the cons (::) operator would build up the stack

let fA x ls = 
    match ls with 
    |[] -> []
    |x::xs when x=y -> 