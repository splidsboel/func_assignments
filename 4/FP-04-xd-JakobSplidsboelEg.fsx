(* 4.1 *)
let explode (s: string) = 
    let z = s.ToCharArray()
    List.ofArray z

let rec explode2 (x: string) = 
    match x with 
    |"" -> []
    |x -> x.Chars(0):: explode2 (x.Remove(0,1))

(* 4.2 *)

let implode x = 
    List.foldBack(fun acc elem -> string acc + elem ) x ""

let implodeRev x = 
    List.foldBack(fun acc elem ->  elem + string acc ) x ""


(* 4.3 *)

let toUpper s = implode (List.map System.Char.ToUpper (explode(s)))

let toUpper1 s = explode s |> List.map System.Char.ToUpper |> implode

let toUpper2 s = implode << List.map System.Char.ToUpper << explode


(* 4.4 *)
let palindrom (s: string) = 
    let z = s.ToCharArray()
    s = implodeRev( List.ofArray z)

(* 4.5 *)

let rec ack (m, n) = 
    match m, n with 
    |0, n -> n+1
    |m, 0 when m>0 -> ack ((m-1), 1)
    |m, n when n>0 && m>0 -> ack ((m-1), (ack (m, (n-1))))
    |_ -> failwith "incorrect input"

ack (3, 11) // = 16381

(* 4.6 *)
let time f = 
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start)

time(fun () -> ack(3,11))

let timeArg1 f a =  time (fun () -> f a) 

timeArg1 ack (3,11)

(* 4.7 *)

let rec downto1 f n e = 
    match n with
    |n when n<=0 -> e
    |n -> f n (downto1 f (n-1) e)

let fact  x= downto1 ( * ) x 1

let  buildList g n = downto1 (fun x acc -> g x :: acc) n []


