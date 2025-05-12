type OrderedList<'a when 'a : equality> =
    {front: 'a list;
    rear: 'a list}

let ex = {front = ['x']; rear = ['z';'y']}

//1.1
let ol1 = {front = ["Hans"; "Brian"; "Gudrun"]; rear = []}
let ol2 = {front = ["Hans"; "Brian"]; rear = ["Gudrun"]}
let ol3 = {front = ["Hans"]; rear = ["Gudrun"; "Brian"]}
//there exists four representations. Each of the previous examples and one with all in reverse order in the rear list


//1.2
let canonical {front = f; rear =r} =
    {front =List.sort(f@List.rev r); rear =[]}


let toList {front =f; rear =r} = f@List.rev r


//1.3
let newOL() = {front=[]; rear=[]}

let isEmpty {front=f;rear=r} =
    if List.isEmpty f && List.isEmpty r then true else false

//1.4
let addFront e {front=f; rear=r}=
    {front=e::f; rear=r}

let removeFront {front =f;rear=r}=
    let oldFront = f.[0]
    let newFront=
        match f with
        |x::xs -> xs
        |_ -> failwith "front is empty"
    (oldFront, {front=newFront;rear=r})

let peekFront {front=f;rear=r}=
    f.[0]

//1.5
let append {front=f1;rear=r1} {front=f2; rear=r2}=
    canonical {front=f1; rear=(r1@f2@r2)}

append ex ex


(*------------------------------------------Question 2------------------------------------------*)

let rec f i = function
    [] -> [i]
    | x::xs -> i+x :: f (i+1) xs


(*
f computes a list from a list where i has been added to the jth element and i increments after each element.
i is the appended to the list. *)

(*
calling f with any input can never be the empty list, because the base case in the function
is adding i to the end of the list. *)

(* for a call to f to go into an infinite loop, the list given as argument would have
to be infinitely long. No such list exists, therefore it is not possible*)

let rec fA i ls acc =
    match ls with
    |[] -> List.rev (i::acc)
    |x::xs -> fA (i+1) xs ((i+x)::acc)

let rec fC i ls c =
    match ls with
    |[] -> c [i]
    |x::xs -> fC (i+1) xs (fun acc -> c ((i+x)::acc))

(*----------------------------------------------------Question 3----------------------------------------------------*)

//3.1
let myFinSeq n M = Seq.map (fun m -> n+n*m) [0..M]
//returns a sequence of length M+1 with each element being increasing multiples of n


//3.2
let mySeq n = Seq.initInfinite (fun m -> n+n*m)

//3.3
let multTable N M = seq{
    for i in [0..N] do
        for j in [0..M] do
            yield (i, j, i*j)
} 

//3.4
let ppMultTable n m =
    let multTableSeq = multTable n m 
    multTableSeq |> Seq.map (fun (x,y,z)-> $"{x} * {y} is {z}")

(*--------------------------------------------Question 4--------------------------------------------*)
type opr =  MovePenUp
            | MovePenDown
            | TurnEast
            | TurnWest
            | TurnNorth
            | TurnSouth
            | Step

type plot = Opr of opr
            | Seq of plot * plot


let side = Seq(Opr MovePenDown, Seq(Opr Step, Seq(Opr Step, Opr Step)))
let rect = Seq(Seq(Opr TurnEast, side),
    Seq(Opr TurnNorth, Seq(side,
    Seq(Opr TurnWest, Seq(side,
    Seq(Opr TurnSouth, side))))))

//4.1
let rec ppOpr op =
    match op with
    |o -> $"{o}"


let rec ppOprPlot plot =
    match plot with
    |Opr o -> $"{o}"
    |Seq (p1, p2) -> $"{ppOprPlot p1} => {ppOprPlot p2}"

//4.2
type dir = North
            | South
            | West
            | East
type pen = PenUp
            | PenDown


type coord = int * int
type state = coord * dir * pen

let initialState = ((0,0),East,PenUp)

let goStep s =
    match s with
    |((x,y), di, pen) -> 
        match di with
        | North -> ((x, y + 1), di, pen)
        | South -> ((x, y - 1), di, pen)
        | East -> ((x + 1, y), di, pen)
        | West -> ((x - 1, y), di, pen)


let addDot s cList o =
    match s with
    |((x,y), di, pen)  ->
        match pen with
        |PenUp->
            match o with
            |MovePenDown ->
                (((x,y)::cList), ((x,y), di, PenDown))
            |Step -> (cList, (goStep s))
            |TurnEast -> (cList, ((x,y), East, pen))
            |TurnWest -> (cList, ((x,y), West, pen))
            |TurnNorth -> (cList, ((x,y), North, pen))
            |TurnSouth -> (cList, ((x,y), South, pen))
            |MovePenUp -> (cList, s)
        |PenDown ->
            match o with
            |MovePenUp ->
                (((x,y)::cList), ((x,y), di, PenUp))
            |Step -> 
                let newState = goStep s
                match newState with
                |((newX, newY), _, PenDown) -> (((newX, newY)::cList), newState)
                |_ -> (cList, newState)
            |TurnEast -> (((x,y)::cList), ((x,y), East, pen))
            |TurnWest -> (((x,y)::cList), ((x,y), West, pen))
            |TurnNorth -> (((x,y)::cList), ((x,y), North, pen))
            |TurnSouth -> (((x,y)::cList), ((x,y), South, pen))
            |MovePenDown -> (cList, s)




