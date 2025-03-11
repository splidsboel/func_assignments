(*----------------6.1 (HR 6.2)--------------------*)

//type declaration for fexpr
type Fexpr = | Const of float
            | X
            | Add of Fexpr * Fexpr
            | Sub of Fexpr * Fexpr
            | Mul of Fexpr * Fexpr
            | Div of Fexpr * Fexpr
            | Sin of Fexpr
            | Cos of Fexpr
            | Log of Fexpr
            | Exp of Fexpr

//function for postfix representation of arithmetic expressions in string
let FexprPostfix fexp = 
    match fexp with
    |X -> $"{fexp}"
    |Add(x1,x2) -> $"{x1} {x2} +"
    |Sub(x1,x2) -> $"{x1} {x2} -"
    |Mul(x1,x2) -> $"{x1} {x2} *"
    |Div(x1,x2) -> $"{x1} {x2} /"
    |Sin(x1) -> $"{x1} sin"
    |Cos(x1) -> $"{x1} cos"
    |Log(x1) -> $"{x1} log"
    |Exp(x1) -> $"{x1} Exp"
    |_ -> failwith "invalid argument"




(*------------6.2 (HR 6.8)-----------*)

//type declaration for instruction
type Instruction = | ADD | SUB | MULT | DIV | SIN
                    | COS | LOG | EXP | PUSH of float

//type declaration for a stack. I'm assuming floats are only valid arguments for the calculator. Also assuming that this definition doesn't have to
//work for any of the standard uses of a stack, just for the calculator purpose. 
type Stack = float list 

//1.
//function for interpreting single Instruction on stack of floats. I added the explicit declaration of sta being a Stack and the return type 
//being a Stack to get vscode to understand that the argument and return type is a stack and not a float list. Which I guess doesn't matter
//since it is a float list either way. 😵‍💫
let intpInstr (sta: Stack) inst :Stack =
    match inst, sta with
    |ADD, x1::x2::rest -> (x1+x2)::rest
    |SUB, x1::x2::rest -> (x2-x1)::rest
    |MULT, x1::x2::rest -> (x1*x2)::rest
    |DIV, x1::x2::rest -> (x2/x1)::rest
    |SIN, x1::rest -> (sin x1)::rest
    |COS, x1::rest -> (cos x1)::rest
    |LOG, x1::rest -> (log x1)::rest
    |EXP, x1::rest -> (exp x1)::rest
    |PUSH r, x -> r::x
    |_, sta -> failwith "no match for instruction"

//2. 
let rec intpProgram l sta =
    match l, sta with
    |[], x1::rest -> x1
    |x1::rest, sta -> intpProgram rest (intpInstr sta x1)
    |_,[] -> failwith "stack is empty"


(* let trans fe x =
    match fe, x with
    |
 *)


(*------------6.3 (HR 7.2)---------------*)
//see qnum.fsi and qnum.fs