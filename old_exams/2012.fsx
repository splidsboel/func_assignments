
type Name = String
type Score = int
type Result = Name * Score
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


