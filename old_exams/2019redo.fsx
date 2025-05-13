let infSeq3 = Seq.initInfinite (fun i -> i*3)

let finSeq3 n =
    Seq.take n infSeq3

let sumSeq3 n =
    finSeq3 n |> Seq.sum

//1.2
let seqMap2 f s1 s2 =
    seq { for (x,y) in Seq.zip s1 s2 do
            yield f x y }

(*
the sequence returned by seqMap2 is the result of applying f on the tuple consisting
of the ith element from s1 and the ith element from s2*)

let swap (x,y) = (y,x)

//seqMap2 swap [1;3;3] [4;5;2]
(*
This does not work because swap needs a tuple as argument, but
in the sequence expression, they are applied as two separate arguments *)

//change arguments for functions
//fix function
//rearrange arguments in function
let fix f =
    fun x y -> f(x,y)

(*------------------------------------------------Question 2------------------------------------------------*)
type TrieNode<'a when 'a : equality> = TN of 'a * bool * TrieNode<'a> list

let trie01 = TN('a',false,[TN('n',false,[TN('d',true,[])])])

let trie03 = TN('a', false, [
    TN('n', true, [
        TN('d', true, [])
    ]);
    TN('d', false, [
        TN('d', true, [])
    ]);
    TN('t', true, [])
])

let trie04 = TN('a', false, [
    TN('n', true, [
        TN('d', true, [])
    ]);
    TN('d', false, [
        TN('d', true, [])
    ]);
    TN('t', true, [
        TN('x', false, [])
    ])
])
//type of trie04 is TrieNode<char>. It is monomorphic because it can only hold chars

exception TrieError of string

let numLetters t = 
    let rec countLetters acc (TN(_, _, children)) =
        List.fold (fun acc child -> countLetters acc child) (acc+1) children
    countLetters 0 t

let numWords t =
    let rec countWords acc (TN(_, p, children))=
        List.fold (fun acc child -> countWords acc child) (if p then (acc+1) else acc) children
    countWords 0 t


let rec exists ls (TN(letter, isWord, children)) =
    match ls with
    | [] -> isWord
    | [x] -> x=letter && isWord
    | x::xs when x= letter->
        List.exists (fun child -> exists xs child) children
    |_ -> false


    
let rec chkTrie (TN(_, isWord, children)) =
    match children with
    | [] -> isWord  
    | _ -> List.forall chkTrie children


//2.3
let rec map f (TN(value, isWord, children)) =
    TN(f value, isWord, List.map (map f) children)

map (string) trie03

(*------------------------------------------Question 3------------------------------------------*)

//3.1
let rec F m i n k =
    match k with
    |k when k<=0 -> m
    |k -> (F m i n (k-1))*(1.0+i/n)


(*
F is not tail-recursive because (1.0+i/n) is multiplied with the recursive call*)

let rec FA m i n k =
    let rec Facc acc m i n k =
        match k with
        |0 -> m * acc
        |x -> Facc (acc*(1.0+i/n)) m i n (x-1)
    Facc 1.0 m i n k

let tabulate f start step stop =
    let s =seq{for x in start..step..stop do
                yield (x, f x) }
    Seq.toList s


let rec prettyPrint xs =
    printfn $"  x   |  f(x)  "
    printfn $"------+---------"
    List.iter (fun (x, fx ) ->
        printfn $"   {x}  |   {fx}    ") xs

prettyPrint [(0, 100.0); (2, 121.0); (4, 146.41)]

(*----------------------------------------Question 4----------------------------------------*)
let dt (d:int, m, y) = System.DateTime(y, m, d)

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
    Scale(100, All[
        Aquire (dt(1,3,2018), Cash 400.4);
        Give (dt(1,3,2018), Stock "Apple")
    ])

let price (s,d:System.DateTime)=
    match d.Month with
    |2 ->
        match s with
        |"APPLE" -> 300.3
        |"ISS" -> 150.0
        |"TIVOLI" -> 212.0
        |_ -> failwith "stock not in price table"
    |3 ->
        match s with
        |"APPLE" -> 400.4
        |"ISS" -> 200.2
        |"TIVOLI" -> 215.2
        |_ -> failwith "stock not in price table"
    |_ -> failwith "date not in price table"


//4.2
let buyStock n s d =
    Scale(n, All[
        Aquire(d, Stock s);
        Give(d, Cash (price (s, d)))
    ])

let receiveCash c d =
    Aquire(d, Cash c)


//4.3
let actions =
    let d1 = dt(1,2,2018)
    let d2 = dt(1,3,2018)
    All [receiveCash 100000.0 d1;
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

let execA action env =
    let rec exec scaling env = function
        | Aquire(d, p) -> updEnv scaling env p
        | Give(d, p) -> updEnv (-scaling) env p
        | Scale(n, a) -> exec (scaling * n) env a
        | All(actions) -> List.fold (fun accEnv a -> exec scaling accEnv a) env actions
    exec 1 env action

