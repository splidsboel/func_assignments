(*----------------------------------------Question 1----------------------------------------*)
type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>
//1.1
let psEx = PrioritySet ["a";"b";"c"]

let priSetEx = PrioritySet ["a"; "q"; "b"; "d"]
//type is PrioritySet<String>

let empty = PrioritySet List.empty

//1.2
let isEmpty (PrioritySet items) = 
    List.isEmpty items

let size (PrioritySet items) =
    List.length items

let contains e (PrioritySet ps) = 
    if (List.forall (fun x -> x<>e) ps) then false else true

let getPN e  (PrioritySet ps) =
    let rec getPNA e ps acc =
        match ps with
        |x::xs -> 
            if e =x then acc else getPNA e xs (acc+1)
        |_ -> failwith "did not find e"
    getPNA e ps 1



