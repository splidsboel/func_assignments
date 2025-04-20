
type Name = String
type Score = int
type Result = Name * Score

//test data
let test1 = [ ("Alice", 75); ("Bob", 88); ("Charlie", 90); ("Dana", 60) ]
    //avg = 78

let test4 = [ ("Eva", 50); ("Frank", 50); ("George", 50) ]
    //avg = 50

//1.1
let rec legalResults (rls: List<Result>) = 
    match rls with
    |[] -> true
    |x::xs -> 
        match x with
        |_, score -> if not (score>= 0 && score<= 100) then false else legalResults xs


//1.2
let rec maxScore (rls: List<Result>) acc = 
    match rls with
    |[] -> acc
    |x::xs -> 
        match x with
        |_, score -> if score > acc then maxScore xs score else maxScore xs acc

//1.3
let rec best rls = 
    let max = maxScore rls 0
    match rls with
    |[] -> failwith "error"
    |x::xs ->
        match x with
        |_, score -> if score = max then x else best xs 

//1.4

let average rls = 
    let cum = 0
    let rec avgcalc acc rls =
        match rls with
        |[] -> acc
        |x::xs -> 
            match x with
            |_, score -> 
                let newcum = acc + score
                avgcalc newcum xs
    (avgcalc cum rls) / List.length rls

//1.5
//delete item from list
let delete rs rsl = List.filter (fun x -> x<> rs) rsl 

//take best n elements from list
//1.6
let rec bestN (rs: Result list) n = 
    match rs with
    |[] -> failwith "empty result list"
    |x::xs -> 
        if List.length rs < n then failwith "rs has fewer than n elements"
        else 
            let sortedRs = List.sortByDescending (fun (_, score) -> score) rs
            List.take n rs



(*---------------------------------Problem 2---------------------------------*)
type Typ = Integer|Boolean|Ft of Typ list * Typ

type Decl = string * Typ

//example data
let xDecl: Decl = ("x", Integer)
let yDecl: Decl = ("y", Boolean)
let addDecl: Decl = ("add", Ft([Integer; Integer], Integer))  // function: int -> int -> int
let isEvenDecl: Decl = ("isEven", Ft([Integer], Boolean))    // function: int -> bool
let composeDecl: Decl = 
    ("compose", Ft([Ft([Boolean], Integer); Ft([Integer], Boolean)], Ft([Integer], Integer)))
    // compose : (bool -> int) -> (int -> bool) -> int -> int

let decls: Decl list = 
    [ xDecl
      yDecl
      addDecl
      isEvenDecl
      composeDecl ]

//check duplicate values in a list of tuples
//list pattern matching with 3 elements of list
let rec distinctVars (decls: Decl list) =
    match decls with
    |[] -> true
    |[_] -> true
    |(_, var1)::rest ->
        match rest with
        |(_, var2)::xs ->
            if var1=var2 then false 
            else distinctVars rest
        |_ -> true 

type SymbolTable = Map<string,Typ>

//convert List to Map
//Map.ofList
let toSymbolTable (decls: Decl list) = Map.ofList decls

//add items to Map
//Map.add
let rec extendST sym decls =
    match decls with
    |[] -> sym
    |(s, v)::xs -> extendST (Map.add s v sym) xs

type Exp = V of string | A of string * Exp list 

//Map.containsKey List.forall
let rec symbolsDefined (sym: SymbolTable) e =
    match e with 
    |V(s) -> Map.containsKey s sym
    |A(s, exp) -> 
        Map.containsKey s sym &&
        List.forall (symbolsDefined sym) exp

let typOf sym e = 
    match e with 
    | V(s) -> if Map.containsKey s sym then Map.find s sym
                        else failwith $"Variable {s} not found"
    | A(f, args) -> if Map.containsKey f sym then Map.find f sym
                                    else failwith $"Function {f} not found"


(*--------------------------------------Problem 3--------------------------------------*)
let rec h a b = 
    match a with
    |[] -> b
    |c::d -> c::(h d b)
(* 1.
the function h takes two lists as arguments and concatenates them. Basically the same as List.append
The type is: 'a list -> 'a list -> 'a list
*)

//2.
type T<'a, 'b> =
    | A of 'a
    | B of 'b
    | C of T<'a, 'b> * T<'a, 'b>

let answerTwo = C (A 42, B true)

//option - Some/None
let answerThree = C(A [1;2;3], B (Some true))


//4.
(* f1
The function takes a tree as argument and computes the height of the tree. 
The type is thus T<'a, 'b> -> int
*)

(* f2
f2 takes all the elements in a tree and puts them into a list. It collects the elements from left to right
the type is T<'a, 'a> -> 'a list
*)

(* f3
f3 traverses the tree (t) dependant on a predicate (b). If b is true, it applies the function recursively
on the left side of the tree. When it reaches a leaf, it inserts the element (e) on the left if b is true, 
and on the right if b is false. 
The type is 'a -> bool -> T<'a, 'a> -> T<'a, 'a> 
*)