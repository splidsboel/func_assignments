type Multiset<'a when 'a : equality> = ('a * int) list

//check if list contains duplicates. List.forall
let rec inv ms =
    match ms with
    |[] -> true
    |(e, _) :: xs ->
        if List.forall (fun (x, _) -> x <> e) xs then inv xs
        else false

