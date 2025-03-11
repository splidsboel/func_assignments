module qnum 

type ComplexNumber = {first:float; second:float}

val (+) : ComplexNumber -> ComplexNumber -> ComplexNumber
val (-) : ComplexNumber -> ComplexNumber -> ComplexNumber
val (*) : ComplexNumber -> ComplexNumber -> ComplexNumber
val (/) : ComplexNumber -> ComplexNumber -> ComplexNumber

