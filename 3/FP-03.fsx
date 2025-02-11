//____________________________3.1_____________________________________
let rec downTo n = 
   if n>0 then n :: downTo(n-1)
   else []


let rec downTo2 n = 
    match n with
    |0 -> []
    |_ -> n::downTo(n-1)


//____________________________3.2_____________________________________
let rec removeOddIdx  = function
   |[] -> []
   |[x] -> [x]
   |x0::x1::xs ->x0::removeOddIdx(xs)


//____________________________3.3_____________________________________
let rec combinePair = function
    |[] -> []
    |[x] -> []
    |x0::x1::xs -> (x0,x1)::combinePair(xs)

(* let list50 = downTo2(50)
combinePair(list50) *)

//____________________________3.4_____________________________________

type OldBritishCurrency = {Pounds :int; Shillings: int; Pence: int}

let addOldBritishCurrency c1 c2 = 
    match c1, c2 with 
    |{Pounds = a1; Shillings=b1; Pence=c1}, {Pounds = a2; Shillings=b2; Pence = c2} -> 
        let totalPence = c1+c2
        let totalShillings = b1+b2 + totalPence/12
        let totalPounds = a1+a2 + totalShillings/20
        {Pounds = totalPounds; Shillings = totalShillings; Pence=totalPence}


let subtractOldBritishCurrency c1 c2 =
   match c1, c2 with 
   |{Pounds = a1; Shillings=b1; Pence=c1}, {Pounds = a2; Shillings=b2; Pence = c2} -> 
    let totalPence = c1-c2
    let totalShillings = b1-b2 + totalPence/12
    let totalPounds = a1-a2+totalShillings/12
    {Pounds = totalPounds; Shillings = totalShillings; Pence=totalPence}


let (+@) c1 c2 = addOldBritishCurrency c1 c2
let (-@) c1 c2 = subtractOldBritishCurrency c1 c2
(* 
let cur1 = {Pounds= 10; Shillings=10; Pence = 10}
let cur2 = {Pounds= 10; Shillings=10; Pence = 10}
cur1-@cur2 *)



//____________________________3.5_____________________________________

//1
type ComplexNumber = {first:float; second:float}
//adding complex numbers
let (+++) a b = 
   match a, b with 
   |{first = x1; second = y1}, {first = x2; second = y2} ->
   let ac = x1+x2
   let bd = y1+y2
   {first = ac; second = bd}
//multiplying complex numbers
let (+*+) a b = 
   match a, b with 
   |{first = x1; second = y1}, {first = x2; second = y2} ->
   let ac = x1*x2
   let bd = y1*y2
   let bc = y1*x2
   let ad = x1*y2
   {first = ac-bd;second = bc+ad}


//2
//subtracting complex numbers
let (+-+) a b = 
    match a, b with
    |{first = x1; second = y1}, {first = x2; second = y2} ->
    {first = x1-x2; second = y1-y2}

//dividing complex numbers
let (+/+) a b = 
    match a, b with
    |{first = x1; second = y1}, {first = x2; second = y2} ->
    let aSquared = x2*x2
    let bSquared = y2*y2
    let denominator = aSquared + bSquared
    let multiplicativeInverse = {first = x2/denominator; second = -y2/denominator}
    a+*+multiplicativeInverse

(* Test data:
let a = { first = 3; second = 2 }    
let b = { first = 1; second = -1 }   
let c = { first = 0; second = 1 }    
let d = { first = -2; second = 5 }   

a+++b
a+-+b
a+*+b
a+/+b *)


//___________________________3.6_________________________________
let rec altsumShort = function
    | [] -> 0
    | x0::xs -> x0 - altsumShort xs
