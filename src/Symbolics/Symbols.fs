namespace MathNet.Symbolics

type Symbol = Symbol of string

type Function =
    | Abs
    | Ln | Log | Exp
    | Sin | Cos | Tan | Csc | Sec | Cot
    | Asin | Acos | Atan | Acsc | Asec | Acot
    | Cosh | Sinh | Tanh | Csch | Sech | Coth
    | Acosh | Asinh | Atanh | Acsch | Asech | Acoth

type Constant =
    | E
    | Pi
    | I
