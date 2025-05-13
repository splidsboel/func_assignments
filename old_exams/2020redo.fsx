type mymap<'a,'b> = MyMap of list<'a*'b>


let ex1 = MyMap [('A',65);('B',66);('C',67)]
let ex1' = MyMap [('C',67);('A',65);('B',66)]

let dice1  =
    MyMap [
        (1, 4);
        (2, 2);
        (3, 3);
        (4, 2);
        (5,2);
        (6, 2)
    ]

let dice2=
    [
        (1,4);
        (2,2);
        (3,3);
        (4,3);
        (5,5);
        (6,3)
    ]

let emptyMap<'a, 'b> () : mymap<char, int> =
    MyMap List.empty

emptyMap()

let size (MyMap m) =
    List.length m

//1.2
let isEmpty m =
    if (size m) >0 then false else true

let rec tryFind k (MyMap m) =
    match m with
    [] -> None
    |(i,j)::xs ->
        if i=k then Some (i, j) else tryFind k (MyMap xs)


let remove k (MyMap m)=
    let rec removeA k m acc=
        match m with
        |[] -> MyMap (List.rev acc)
        |(i,j)::xs ->
            if k=i then removeA k xs acc else removeA k xs ((i,j)::acc)
    removeA k m []

let add k v (MyMap m) =
    let (MyMap m') = remove k (MyMap m)
    MyMap ((k, v) :: m')


//1.3
let upd f k v (MyMap m)=
    match tryFind k (MyMap m) with
    |None -> add k v (MyMap m)
    |Some(i,j) ->
        let newValue = f v j
        add k newValue (MyMap m)


let map f (MyMap m) =
    let rec mapA f m acc=
        match m with
        |[] -> List.rev acc
        |(i,j)::xs->
            mapA f xs ((i,(f i j))::acc)
    MyMap(mapA f m [])


let rec fold f s (MyMap m)=
    match m with 
    |[] -> s
    |(i,j)::xs ->
        fold f (f s i j) (MyMap xs)

fold (fun s k v -> s+v) 0 dice1

let even n =
    if n%2=0 then true else false

let collatz n =
    match n with
    |n when even n -> n/2
    |n -> 3*n+1

let collatz' n =
    if n<= 0 then failwith "n not larger than 0" else collatz n

//2.2
let applyN f n N =
    let rec applyNAcc f n N acc =
        match N with
        |0 -> n::acc
        |_ -> applyNAcc f (f n) (N-1) (n::acc)
    List.rev (applyNAcc f n N [])

let applyUntilOne f n =
    let rec applyUntilOneA f n acc=
        match f n with
        |1 -> acc
        |_ -> applyUntilOneA f (f n) (acc+1)
    applyUntilOneA f n 1

//2.3
let rec mySeq f x =
    seq {
        yield x 
        yield! mySeq f (f x)}
(*
The function mySeq returns the sequence with x being the first number and the 
following numbers being the rest of the sequence with x being f applied to x*)

let rec g x = x*2

(*--------------------------------------Question 3--------------------------------------*)

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

let addTransToMap (t:trans) (m: Map<name, transData list>) =
    match t with
    |(k,v) -> 
        match (Map.tryFind k m) with
        |None -> Map.add k (v::[]) m
        |Some x -> Map.add k (v::x) m

let m1 = addTransToMap ("ISS", ((24,02,2014),100.0,218.99,Buy)) Map.empty
let m2 = addTransToMap ("ISS", ((22,05,2018),400.0,493.60,Buy)) m1

let shares = List.foldBack addTransToMap ts Map.empty


//3.2

let accTrans (tq:float,avg:float) ((d,q,p,tType):transData) =
    match tType with
    Buy -> (tq+q, (avg*tq+q*p)/(tq+q))
    | Sell -> (tq-q, avg)

let quantityAndAvgPrice ts =
    List.fold accTrans (0.0,0.0) ts

quantityAndAvgPrice [((24,02,2014),100.0,218.99,Buy);
    ((23,02,2016),825.0,280.23,Buy)]

let res =
    Map.map (fun k v -> quantityAndAvgPrice (Map.find k shares)) shares

(*----------------------------------------------Question 4----------------------------------------------*)
let rec dup = function
    [] -> []
    | x::xs -> x::x::dup xs

(*
The function duplicates each element in the list *)

let rec dupA list acc =
    match list with
    |[] -> List.rev acc
    |x::xs -> dupA xs (x::x::acc)

//4.2
let replicate2 i = seq{
    yield i
    yield i
}

let dupSeq = Seq.initInfinite (fun i-> i/2)


//4.3
let dupSeq2 s = seq{
    for i in s do 
        yield i
        yield i
}


