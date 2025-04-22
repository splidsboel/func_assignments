let infSeq3 = Seq.initInfinite (fun i -> i*3)

let finSeq3 n = Seq.init n (fun i -> i*3)

//sum sequence. Seq.fold
let sumSeq3 n = finSeq3 n |> Seq.fold (fun acc x -> acc+x) 0

let seqMap2 f s1 s2 =
    seq {for (x,y) in Seq.zip s1 s2 do
            yield f x y 
    }

//1.2
(* 
seqMap2 first zips the two sequences (yields tuples of the ith element 
from each sequence). Then puts the result of applying the function f to each 
element in the tuple in the sequence that is returned in the end.
*)

let swap (x,y) = (y,x)


//seqMap2 swap [1;3;3] [4;5;2]
(*
this does not work because swap takes a tuple as an argument, not two elements as arguments.
*)

//change argument type for function
let fix f = fun x y -> f(x,y)

seqMap2 (fix swap) [1;3;3] [4;5;2]


(*------------------------------Question 2------------------------------*)

//2.1
type TrieNode<'a when 'a:equality> = TN of 'a*bool * TrieNode<'a> list

let trie01 = TN('a', false, [TN('n', false, [TN('d', true,[])])])

let trie03 = TN('a', false, [TN('n', true, [TN('d', true, [])]); TN('d', false, [TN('d', true, [])]); TN('t', true, [])])

let trie04 = TN('a', false, [TN('n', true, [TN('d', true, [])]); TN('d', false, [TN('d', true, [])]); TN('t', true, [TN('x', false, [])])])

exception TrieError of string 

//2.2
let numLetters trie = 
    let rec countLetters acc (TN(_, _, children)) = 
        List.fold (fun acc child -> countLetters acc child) (acc+1) children
    countLetters 0 trie

let numWords trie =
    let rec countWords acc (TN(_, bool, children)) = 
        List.fold (fun acc  child -> countWords acc child) (if bool then (acc+1) else acc) children
    countWords 0 trie


//traverse two collections at the same time (pseudo-tree and list here)
let rec exists ls (TN(letter, isWord, children)) =
    match ls with
    | [] -> isWord
    | [x] -> x=letter && isWord
    | x::xs when x= letter->
        List.exists (fun child -> exists xs child) children
    |_ -> false


(* let chkTrie t = 
    let rec buildList acc tr = 
        
 *)

(*------------------------------------------------------Question 3------------------------------------------------------*)

//3.1
let rec F m i n k =
    match k with 
    |0 -> m
    |x -> (F m i n (x-1)) * (1.0 + (i/n))

(*
F is not tail-recursive because the recursive call is not the last thing that happens.
*)

let rec FA m i n k =
    let rec Facc acc m i n k =
        match k with
        |0 -> m * acc
        |x -> Facc (acc*(1.0+i/n)) m i n (x-1)
    Facc 1.0 m i n k



//3.2
//make sequence with parameters. start step stop. yield for. computational expression
let tabulate f start step stop =
    seq{for x in start..step..stop do
        yield (x, f x) }


//List.iter printfn
let prettyPrint xs =
    printfn "   x   |   f(x)    "
    printfn "-------+-----------"
    List.iter (fun (x, fx ) ->
        printfn $"   {x}   |   {fx}    ") xs

prettyPrint [(0, 100.0); (2, 121.0); (4, 146.41)]


(*----------------------------------------Question 4----------------------------------------*)
//4.1
let dt ((d:int), m , y) :System.DateTime = System.DateTime (y, m, d)
exception Error of string

type Position =
    | Stock of string
    | Cash of float

type Action =
    | Aquire of System.DateTime * Position
    | Give of System.DateTime * Position
    | Scale of int * Action
    | All of Action list

let ex1 =
    Scale(100,All[Aquire (dt(1,2,2018),Stock "APPLE");
        Give (dt(1,2,2018),Cash 300.3)])
    
let sellApple =
    Scale(100, All[Aquire (dt(1, 3, 2018), Cash 400.4);
        Give (dt(1,3,2018), Stock "APPLE" )])

let price (s, (d:System.DateTime)) =
    let day = d.Day
    let month = d.Month
    let year = d.Year
    match s with
    |"APPLE" ->
        match month with
        |2 -> 300.3
        |3 -> 400.4
        |_ -> failwith "no price available"
    |"ISS" ->
        match month with
        |2 -> 150.0
        |3 -> 200.2
        |_ -> failwith "no price available"
    |"TIVOLI" ->
        match month with
        |2 -> 212.0
        |3 -> 215.2
        |_ -> failwith "no price available"
    |_ -> failwith "stock not in table"

//4.2

//let ex1 =
 //   Scale(100,All[Aquire (dt(1,2,2018),Stock "APPLE");
   //     Give (dt(1,2,2018),Cash 300.3)])

let buyStock n s d =
    Scale(n, All[Aquire(d, Stock s);
        Give (d, Cash (price (s,d)))])

let recieveCash c d = Aquire (d, Cash c)

let actions =
    let d1 = dt(1,2,2018)
    let d2 = dt(1,3,2018)
    All [recieveCash 100000.0 d1;
        buyStock 100 "APPLE" d1;
        buyStock 200 "ISS" d1;
        buyStock 50 "TIVOLI" d2]

type stockEnv = Map<string,int>
let updStock s n m =
    match Map.tryFind s m with
    None -> Map.add s n m
    | Some n1 -> Map.add s (n+n1) m

type env = float * stockEnv
let emptyEnv = (0.0,Map.empty)

let updEnv scaling ((cash, stockEnv):env) pos =
    match pos with
    |Stock symbol -> 
        env(cash, Map.add symbol scaling stockEnv)
    |Cash amount -> 
        let scalefloat = (scaling:float)
        env((amount+scalefloat), stockEnv)


