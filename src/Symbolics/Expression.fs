namespace MathNet.Symbolics

open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

[<StructuralEquality;NoComparison>]
type Expression =
    | Number of BigRational
    | Approximation of Approximation
    | Identifier of Symbol
    | Constant of Constant
    | Sum of Expression list
    | Product of Expression list
    | Power of Expression * Expression
    | Function of Function * Expression
    | FunctionN of Function * (Expression list)
    | ComplexInfinity
    | PositiveInfinity
    | NegativeInfinity
    | Undefined

[<RequireQualifiedAccess>]
module Values =

    let (|Value|_|) = function
        | Number n -> Some (Value.Number n)
        | Approximation a -> Some (Value.Approximation a)
        | ComplexInfinity -> Some Value.ComplexInfinity
        | PositiveInfinity -> Some Value.PositiveInfinity
        | NegativeInfinity -> Some Value.NegativeInfinity
//        | Undefined -> Some Value.Undefined
        | _ -> None

    let unpack = function
        | Value.Number n -> Number n
        | Value.Approximation a -> Approximation a
        | Value.ComplexInfinity -> ComplexInfinity
        | Value.PositiveInfinity -> PositiveInfinity
        | Value.NegativeInfinity -> NegativeInfinity
        | Value.Undefined -> Undefined

    let real (x:float) = Value.real x |> unpack
    let complex (x:Complex) = Value.complex x |> unpack
    let rational (x:BigRational) = Number x

    let negate a = Value.negate a |> unpack
    let abs a = Value.abs a |> unpack

    let sum (a, b) = Value.sum (a, b) |> unpack
    let product (a, b) = Value.product (a, b) |> unpack
    let invert a = Value.invert a |> unpack
    let power (a, b) = Value.power (a, b) |> unpack

    let apply f x = Value.apply f x |> unpack


module ExpressionPatterns =

    let (|Zero|_|) = function
        | Number n when n.IsZero -> Some Zero
        | Approximation x when Approximation.isZero x -> Some Zero
        | _ -> None

    let (|One|_|) = function
        | Number n when n.IsOne -> Some One
        | Approximation x when Approximation.isOne x -> Some One
        | _ -> None

    let (|MinusOne|_|) = function
        | Number n when n.IsInteger && n.Numerator = BigInteger.MinusOne -> Some MinusOne
        | Approximation x when Approximation.isMinusOne x -> Some MinusOne
        | _ -> None

    let (|Negative|_|) = function
        | Number n when n.IsNegative -> Some Negative
        | Approximation x when Approximation.isNegative x -> Some Negative
        | NegativeInfinity -> Some Negative
        | _ -> None

    let (|Positive|_|) = function
        | Number n when n.IsPositive -> Some Positive
        | Constant E | Constant Pi -> Some Positive
        | Approximation x when Approximation.isPositive x -> Some Positive
        | PositiveInfinity -> Some Positive
        | _ -> None

    let (|Integer|_|) = function
        | Number n when n.IsInteger -> Some (n)
        | _ -> None

    let (|PosIntPower|_|) = function
        | Power (r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | _ -> None

    let (|NegIntPower|_|) = function
        | Power (r, (Number n as p)) when n.IsInteger && n.IsNegative -> Some (r, p)
        | _ -> None

    let (|NegRationalPower|_|) = function
        | Power (r, (Number n as p)) when n.IsNegative -> Some (r, p)
        | _ -> None

    let (|NegPower|_|) = function
        | Power (r, (Negative _ as p))-> Some (r, p)
        | _ -> None

    /// Terminal node, either a number, identifier/symbol or constant (including infinity).
    /// Warning: Undefined is *not* included.
    let (|Terminal|_|) = function
        | Number _ | Identifier _ | Constant _ as t -> Some t
        | _ -> None

    /// Recognizes a sin or cos expression
    let (|SinCos|_|) = function
        | Function (Sin, _) | Function (Cos, _) as t -> Some t
        | Function (Sinh, _) | Function (Cosh, _) as t -> Some t
        | _ -> None

    let (|SinCosPosIntPower|_|) = function
        | Function (Sin, _) | Function (Cos, _) as r -> Some (r, Number BigRational.One)
        | Function (Sinh, _) | Function (Cosh, _) as r -> Some (r, Number BigRational.One)
        | Power (Function (Sin, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | Power (Function (Cos, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)        
        | Power (Function (Sinh, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | Power (Function (Cosh, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | _ -> None


module Operators =

    open ExpressionPatterns

    let zero = Number BigRational.Zero
    let one = Number BigRational.One
    let two = Number (BigRational.FromInt 2)
    let three = Number (BigRational.FromInt 3)
    let four = Number (BigRational.FromInt 4)
    let five = Number (BigRational.FromInt 5)
    let six = Number (BigRational.FromInt 6)
    let seven = Number (BigRational.FromInt 7)
    let eight = Number (BigRational.FromInt 8)
    let nine = Number (BigRational.FromInt 9)
    let ten = Number (BigRational.FromInt 10)
    let minusOne = Number (BigRational.FromInt -1)
    let pi = Constant Pi

    let symbol (name:string) = Identifier (Symbol name)

    let undefined = Expression.Undefined
    let infinity = Expression.PositiveInfinity
    let complexInfinity = Expression.ComplexInfinity
    let negativeInfinity = Expression.NegativeInfinity

    let real floatingPoint = Values.real floatingPoint

    let fromInt32 (x:int) = Number (BigRational.FromInt x)
    let fromInt64 (x:int64) = Number (BigRational.FromBigInt (BigInteger(x)))
    let fromInteger (x:BigInteger) = Number (BigRational.FromBigInt x)
    let fromIntegerFraction (n:BigInteger) (d:BigInteger) = Number (BigRational.FromBigIntFraction (n, d))
    let fromRational (x:BigRational) = Number x

    let number = fromInt32

    let isZero = function | Zero -> true | _ -> false
    let isOne = function | One -> true | _ -> false
    let isMinusOne = function | MinusOne -> true | _ -> false
    let isPositive = function | Positive -> true | _ -> false
    let isNegative = function | Negative -> true | _ -> false
    let isPositiveInfinity = function | PositiveInfinity -> true | _ -> false
    let isNegativeInfinity = function | NegativeInfinity -> true | _ -> false
    let isComplexInfinity = function | ComplexInfinity -> true | _ -> false
    let isInfinity = function | PositiveInfinity | ComplexInfinity | NegativeInfinity -> true | _ -> false

    let internal orderRelation (x:Expression) (y:Expression) =
        let rec compare a b =
            match a, b with
            | Number x, Number y -> x < y
            | Approximation x, Approximation y -> Approximation.orderRelation x y
            | Identifier x, Identifier y -> x < y
            | Constant x, Constant y -> x < y
            | Sum xs, Sum ys | Product xs, Product ys -> compareZip (List.rev xs) (List.rev ys)
            | Power (xr,xp), Power (yr,yp) -> if xr <> yr then compare xr yr else compare xp yp
            | Function (xf, x), Function (yf, y) -> if xf <> yf then xf < yf else compare x y
            | FunctionN (xf, xs), FunctionN (yf, ys) -> if xf <> yf then xf < yf else compareZip (List.rev xs) (List.rev ys)
            | Number _, _ -> true
            | _, Number _ -> false
            | Approximation _, _ -> true
            | _, Approximation _ -> false
            | Constant _, _ -> true
            | _, Constant _ -> false
            | Product xs, y -> compareZip (List.rev xs) [y]
            | x, Product ys -> compareZip [x] (List.rev ys)
            | Power (xr, xp), y -> if xr <> y then compare xr y else compare xp one
            | x, Power (yr, yp) -> if x <> yr then compare x yr else compare one yp
            | Sum xs, y -> compareZip (List.rev xs) [y]
            | x, Sum ys -> compareZip [x] (List.rev ys)
            | Function (xf, x), FunctionN (yf, ys) -> if xf <> yf then xf < yf else compareZip [x] (List.rev ys)
            | FunctionN (xf, xs), Function (yf, y) -> if xf <> yf then xf < yf else compareZip (List.rev xs) [y]
            | Identifier _, _ -> true
            | _, Identifier _ -> false
            | ComplexInfinity, _ -> true
            | _, ComplexInfinity -> false
            | PositiveInfinity, _ -> true
            | _, PositiveInfinity -> false
            | NegativeInfinity, _ -> true
            | _, NegativeInfinity -> false
            | Undefined, _ -> false
            | _, Undefined -> true
        and compareZip a b =
            match a, b with
            | x::xs, y::ys when x <> y -> compare x y
            | x::xs, y::ys -> compareZip xs ys
            | [], y::ys -> true
            | _, [] -> false
        compare x y

    let rec add x y =
        // none of the summands is allowed to be a sum
        // only the first summand is allowed to be a number

        /// Recognize terms of the form a*x -> (v,x) where a is a value
        let (|Term|_|) = function
            | Number _ -> None
            | Approximation _ -> None
            | Product [(Values.Value v); b] -> Some (v, b)
            | Product ((Values.Value v)::xs) -> Some (v, Product xs)
            | x -> Some (Value.one, x)

        let merge (xs:Expression list) (ys:Expression list) =
            let rec gen acc u v =
                match acc, u, v with
                | Zero::cc, _, _ -> gen cc u v
                | Term(ac,at)::cc, Term(xc,xt)::xs, y | Term(ac,at)::cc, y, Term(xc,xt)::xs when at = xt ->
                    gen ((multiply (Value.sum(ac,xc) |> Values.unpack) at)::cc) xs y
                | _, Term(xc,xt)::xs, Term(yc,yt)::ys when xt = yt ->
                    gen ((multiply (Value.sum(xc,yc) |> Values.unpack) xt)::acc) xs ys
                | _, x::xs, y::ys ->
                    if orderRelation x y then gen (x::acc) xs v
                    else gen (y::acc) u ys
                | _, x::xs, [] | _, [], x::xs -> gen (x::acc) xs []
                | _, [], [] -> acc
            match gen [] xs ys with
            | [x] -> x
            | [] -> zero
            | x -> Sum (List.rev x)

        let rec valueAdd (v:Value) x =
            match x with
            | Values.Value a | Sum [Values.Value a] -> Values.sum (v, a)
            | Sum [] -> Values.unpack v
            | Sum [a] -> if Value.isZero v then a else Sum [Values.unpack v; a]
            | Sum ((Values.Value a)::ax) -> valueAdd (Value.sum (a,v)) (Sum ax)
            | Sum ax -> if Value.isZero v then x else Sum (Values.unpack v::ax)
            | x -> if Value.isZero v then x else Sum [Values.unpack v; x]

        match x, y with
        | Undefined, _ | _, Undefined -> undefined
        | Zero, b | b, Zero -> b
        | ComplexInfinity, oo | oo, ComplexInfinity when isInfinity oo -> undefined
        | ComplexInfinity, _ | _, ComplexInfinity -> complexInfinity
        | PositiveInfinity, PositiveInfinity -> infinity
        | PositiveInfinity, oo | oo, PositiveInfinity when isInfinity oo -> undefined
        | PositiveInfinity, _ | _, PositiveInfinity -> infinity
        | NegativeInfinity, NegativeInfinity -> negativeInfinity
        | NegativeInfinity, _ | _, NegativeInfinity -> negativeInfinity
        | Values.Value a, Values.Value b -> Values.sum (a, b)
        | Values.Value a, b | b, Values.Value a -> valueAdd a b
        | Sum ((Values.Value a)::ax), Sum ((Values.Value b)::bx) -> valueAdd (Value.sum (a, b)) (merge ax bx)
        | Sum ((Values.Value a)::ax), Sum bx | Sum bx, Sum ((Values.Value a)::ax) -> valueAdd a (merge ax bx)
        | Sum ((Values.Value a)::ax), b | b, Sum ((Values.Value a)::ax) -> valueAdd a (merge ax [b])
        | Sum ax, Sum bx -> merge ax bx
        | Sum ax, b -> merge ax [b]
        | a, Sum bx -> merge [a] bx
        | a, b -> merge [a] [b]

    and multiply x y =
        // none of the factors is allowed to be a product
        // only the first factor is allowed to be a number

        /// Recognize terms of the form r^p -> (r,p)
        let (|Term|_|) = function
            | Number _ -> None
            | Approximation _ -> None
            | Power (r,p) -> Some (r, p)
            | x -> Some (x, one)

        let merge (xs:Expression list) (ys:Expression list) =
            let rec gen acc u v =
                match acc, u, v with
                | One::cc, _, _ -> gen cc u v
                | Term(ab,ae)::cc, Term(xb,xe)::xs, y | Term(ab,ae)::cc, y, Term(xb,xe)::xs when ab = xb ->
                    gen ((pow ab (add ae xe))::cc) xs y
                | _, Term(xb,xe)::xs, Term(yb,ye)::ys when xb = yb ->
                    gen ((pow xb (add xe ye))::acc) xs ys
                | _, x::xs, y::ys ->
                    if orderRelation x y then gen (x::acc) xs v
                    else gen (y::acc) u ys
                | _, x::xs, y -> gen (x::acc) xs y
                | _, [], y::ys -> gen (y::acc) ys []
                | _, [], [] -> acc
            match gen [] xs ys with
            | [x] -> x
            | [] -> one
            | x -> Product (List.rev x)

        /// Multiply a number with an expression (potentially a denormalized product)
        let rec valueMul (v:Value) x =
            if Value.isZero v then zero else
            match x with
            | Values.Value a | Product [Values.Value a] -> Values.product (v, a)
            | Product [] -> Values.unpack v
            | Product [a] -> if Value.isOne v then a else Product [Values.unpack v; a]
            | Product ((Values.Value a)::ax) -> valueMul (Value.product (a,v)) (Product ax)
            | Product ax -> if Value.isOne v then x else Product (Values.unpack v::ax)
            | x -> if Value.isOne v then x else Product [Values.unpack v; x]

        match x, y with
        | Undefined, _ | _, Undefined -> undefined
        | One, b | b, One -> b
        | Zero, oo | oo, Zero when isInfinity oo -> undefined
        | Zero, _ | _, Zero -> zero
        | ComplexInfinity, _ | _, ComplexInfinity -> complexInfinity
        | PositiveInfinity, Positive | Positive, PositiveInfinity -> infinity
        | PositiveInfinity, Negative | Negative, PositiveInfinity -> negativeInfinity
        | PositiveInfinity, _ | _, PositiveInfinity -> infinity
        | NegativeInfinity, Positive | Positive, NegativeInfinity -> negativeInfinity
        | NegativeInfinity, Negative | Negative, NegativeInfinity -> infinity
        | NegativeInfinity, _ | _, NegativeInfinity -> negativeInfinity
        | Values.Value a, Values.Value b -> Values.product (a, b)
        | Values.Value a, b | b, Values.Value a -> valueMul a b
        | Product ((Values.Value a)::ax), Product ((Values.Value b)::bx) -> valueMul (Value.product (a, b)) (merge ax bx)
        | Product ((Values.Value a)::ax), Product bx | Product bx, Product ((Values.Value a)::ax) -> valueMul a (merge ax bx)
        | Product ((Values.Value a)::ax), b | b, Product ((Values.Value a)::ax) -> valueMul a (merge ax [b])
        | Product ax, Product bx -> merge ax bx
        | Product ax, b -> merge ax [b]
        | a, Product bx -> merge [a] bx
        | a, b -> merge [a] [b]

    and pow x y =
        // if power is a number, radix must not be an integer, fraction, product or power
        match x, y with
        | Undefined, _ | _, Undefined -> undefined
        | Zero, Zero -> undefined
        | Zero, (ComplexInfinity | PositiveInfinity) -> zero
        | Zero, NegativeInfinity -> complexInfinity
        | Zero, Positive -> zero
        | Zero, Negative -> complexInfinity
        | oo, Zero when isInfinity oo -> undefined
        | oo, PositiveInfinity when isInfinity oo -> complexInfinity
        | oo, Number b when isInfinity oo && b.IsNegative -> zero
        | ComplexInfinity, Positive -> complexInfinity                
        | PositiveInfinity, Positive -> infinity
        | NegativeInfinity, Number b when b.IsPositive && b.IsInteger ->
            if (b.Numerator % 2I).IsZero then infinity else negativeInfinity
        | One, oo | MinusOne, oo when isInfinity oo -> undefined
        | _, Zero | One, _ -> one
        | a, One -> a
        | Positive, PositiveInfinity -> infinity
        | Negative, PositiveInfinity -> complexInfinity 
        | _, NegativeInfinity -> zero
        | _, ComplexInfinity -> undefined
        | Number a, Number b when not (b.IsInteger) -> Power (x,y)
        | Values.Value a, Values.Value b -> Values.power (a, b)
        | Product ax, Number b when b.IsInteger -> Product (ax |> List.map (fun z -> pow z y))
        | Power (r, p), Number b when b.IsInteger -> pow r (multiply p y)
        | a, b -> Power(a, b)

    let plus x = x
    let negate x = multiply minusOne x
    let subtract x y = add x (negate y)

    let rec invert = function
        | Undefined -> undefined
        | Zero -> complexInfinity
        | oo when isInfinity oo -> zero 
        | Values.Value v -> Values.invert v
        | Product ax -> Product (ax |> List.map invert)
        | Power (r, p) -> pow r (negate p)
        | x -> Power (x, minusOne)

    let divide x y = multiply x (invert y)

    let sum (xs:Expression list) = if List.isEmpty xs then zero else List.reduce add xs
    let sumSeq (xs:Expression seq) = Seq.fold add zero xs
    let product (xs:Expression list) = if List.isEmpty xs then one else List.reduce multiply xs
    let productSeq (xs:Expression seq) = Seq.fold multiply one xs

    let root n x = pow x (pow n minusOne)
    let sqrt x = root two x

    // algebraically expand the given argument
    let rec private expandArg (x:Expression) = 
        let rec expandProduct x y =
            match x, y with
            | Sum ax, b | b, Sum ax -> sum <| List.map (expandProduct b) ax
            | a, b -> multiply a b
        let rec expandPower x (y:int) =
            match x, y with
            | Sum [a], b -> pow a (number b)
            | Sum (a::ax), n when n > 1 ->
                let e = int n
                [for k in 0 .. e -> (k, int <| SpecialFunctions.Binomial(e, k))]
                |> List.map (fun (k,c) -> expandProduct (multiply (fromInt32 c) (pow a (number(e-k)))) (expandPower (Sum ax) k))
                |> sum
            | a, b -> pow a (number b)
        match x with
        | Sum ax -> sum <| List.map expandArg ax
        | Product ax -> List.map expandArg ax |> List.reduce expandProduct
        | PosIntPower (r, Number n) -> expandPower (expandArg r) (int n)
        | x -> x
    
    let abs x =
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> infinity
        | Constant I -> one
        | Values.Value v -> Values.abs v
        | Product ((Values.Value v)::ax) when Value.isNegative v -> Function (Abs, multiply (Values.abs v) (Product ax))
        | _ as x' -> Function (Abs, x')

    let exp x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | Zero -> one
        | One -> Constant E
        | PositiveInfinity -> infinity
        | NegativeInfinity -> zero
        | _ as x' -> Function (Exp, x')
    let ln x =
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> infinity
        | Zero -> negativeInfinity
        | One -> zero
        | Constant E -> one
        | Constant I -> divide (multiply pi (Constant I)) two // ln(j) = pi*j/2
        | _ as x' -> Function (Ln, x)
    let log10 x =
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> infinity
        | Zero -> negativeInfinity
        | One -> zero
        | Number n when n.Equals(10N) -> one        
        | _ as x' -> Function (Log, x')
    let log basis x = FunctionN (Log, [basis; x])

    // return a number normalized in [0, 2]. n = p/q = 2*m + p'/q, so p' = p % 2*q
    let internal modTwo (n:BigRational) =
        if n >= 0N && n <= 2N then n
        else let divisor = (n.Denominator * 2I)
             let p' = n.Numerator % divisor
             if sign p' >= 0 then BigRational.FromBigIntFraction(p', n.Denominator)
             else BigRational.FromBigIntFraction(p' + divisor, n.Denominator)
    // [a; n*pi; b;] returns (n, [a; b;])
    let internal findFirstPiTerm ax = 
        let rec pick acc = function
            | [] -> 0N, acc
            | x::xs -> match x with
                        | Constant Pi -> 1N, acc@xs
                        | Product [(Number n);(Constant Pi);] -> n, acc@xs
                        | _ -> pick (acc@[x]) xs
        pick [] ax
    // [a; n*pi*j; b;] returns (n, sum [a; b;])
    let internal findFirstPiJTerm ax = 
        let rec pick acc = function
            | [] -> 0N, acc
            | x::xs -> match x with
                        | Product [(Constant Pi);(Constant I);] -> 1N, acc@xs
                        | Product [(Number n);(Constant Pi);(Constant I);] -> n, acc@xs
                        | _ -> pick (acc@[x]) xs
        pick [] ax    
    // knwon algebraic returns for sin(m/n*pi)(with m, n integers) 
    let internal sineTable =
        dict[(0N), zero; // sin(0*pi) = 0
             (1N/2N), one; // sin(1/2*pi) = 1
             (1N/3N), divide (sqrt three) two; // sqrt(3)/2
             (1N/4N), divide one (sqrt two); // 1/sqrt(2)
             (1N/5N), sqrt (subtract (divide five eight) (divide (sqrt five) eight)); // sqrt(5/8 - sqrt(5)/8)
             (2N/5N), sqrt (add (divide five eight) (divide (sqrt five) eight)); // sqrt(5/8 + sqrt(5)/8)
             (1N/6N), divide one two; // 1/2
             (1N/8N), divide (sqrt (subtract two (sqrt two))) two; // sqrt(2 - sqrt(2))/2
             (3N/8N), divide (sqrt (add two (sqrt two))) two; // sqrt(2 + sqrt(2))/2
             (1N/10N), divide (subtract (sqrt five) one) four; // (sqrt(5) - 1)/4
             (3N/10N), divide (add one (sqrt five)) four; // (1 + sqrt(5))/4
             (1N/12N), divide (subtract (sqrt three) one) (multiply two (sqrt two)); // (sqrt(3) - 1)/(2*sqrt(2))
             (5N/12N), divide (add one (sqrt three)) (multiply two (sqrt two)); // (1 + sqrt(3))/(2*sqrt(2))
            ]
    // knwon algebraic returns for tan(m/n*pi)(with m, n integers)
    let internal tangentTable =
        dict[(0N), zero; // 0
             (1N/2N), complexInfinity; // ⧝
             (1N/3N), sqrt three; // sqrt(3)
             (1N/4N), one; // 1
             (1N/5N), sqrt (subtract five (multiply two (sqrt five))) // sqrt(5 - 2*sqrt(5))
             (2N/5N), sqrt (add five (multiply two (sqrt five))) // sqrt(5 + 2*sqrt(5))
             (1N/6N), divide one (sqrt three); // 1/sqrt(3)
             (1N/8N), subtract (sqrt two) one // sqrt(2) - 1
             (3N/8N), add one (sqrt two); // 1 + sqrt(2)
             (1N/10N), sqrt (subtract one (divide two (sqrt five))) // sqrt(1 - 2/sqrt(5))
             (3N/10N), sqrt (add one (divide two (sqrt five))) // sqrt(1 + 2/sqrt(5))
             (1N/12N), subtract two (sqrt three) // 2 - sqrt(3)
             (5N/12N), add two (sqrt three); // 2 + sqrt(3)
            ]

    let rec sin x =
        let rec exactValue (n:BigRational) =
            let n' = modTwo n // wrap n into [0, 2]
            match n' with
            | n' when n'.IsInteger -> zero
            | n' when n' > 1N -> exactValue (n' - 1N) |> negate // shift by pi, sin(x) = -sin(x + pi)
            | n' when n' > 1N/2N -> exactValue (1N - n') // reflection at pi/2, sin(x) = sin(pi - x)
            | n' when sineTable.ContainsKey(n') -> sineTable.[n']
            | _ -> Function (Sin, multiply (Number n') pi)
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined
        | Zero -> zero // sin(0) = 0        
        | Number n when n.IsNegative -> sin (Number -n) |> negate // sin(-x) = -sin(x)
        | Product ((Number n)::ax) when n.IsNegative -> sin (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> sin (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> sin (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate 

        | Constant I -> sinh one |> multiply (Constant I) // sin(j*x) = j*sinh(x)
        | Product [Number n; Constant I] -> sinh (Number n) |> multiply (Constant I)
        | Product ((Number n)::(Constant I)::ax) -> sinh (multiply (Number n) (product ax)) |> multiply (Constant I)
        | Product ((Constant I)::ax) -> sinh (product ax) |> multiply (Constant I)

        | Sum ax -> let (m, others) = findFirstPiTerm ax // sin(x) has period 2*pi
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> cos (sum others) // sin(z + 1/2*pi) = cos(z)
                    | m' when m'.Equals(1N) -> sin (sum others) |> negate // sin(z + pi) = -sin(z)
                    | m' when m'.Equals(3N/2N) -> cos (sum others) |> negate // sin(z + 3/2*pi) = -cos(z)
                    | m' when m'.Equals(2N) -> sin (sum others) // sin(z + 2*pi) = sin(z)
                    | _ -> Function (Sin, sum ax)

        | Product [Number n; Constant Pi] -> exactValue n // sin(m/n*pi)

        | _ as x'-> Function (Sin, x')
    and cos x =
        let rec exactValue (n:BigRational) =
            let n' = modTwo n // make sure n in [0, 2]
            match n' with
            | n' when (n' + 1N/2N).IsInteger -> zero
            | n' when n' > 1N -> exactValue (n' - 1N) |> negate // shift by pi, cos(x) = -cos(x + pi)
            | n' when n' > 1N/2N -> exactValue (1N - n') |> negate // reflection at pi/2, cos(x) = -cos(pi - x)
            | n' when sineTable.ContainsKey(1N/2N - n') -> sineTable.[1N/2N - n'] // reflection at pi/4, cos(x) = sin(pi/2 - x)
            | _ -> Function (Cos, multiply (Number n') pi)
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined
        | Zero -> one // cos(0) = 1

        | Number n when n.IsNegative -> cos (Number -n) // cos(-x) = cos(x)
        | Product ((Number n)::ax) when n.IsNegative -> cos (multiply (Number -n) (Product ax))
        | Sum ((Number n)::ax) when n.IsNegative -> cos (add (Number -n) (Sum (ax |> List.map negate)))
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> cos (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate)))

        | Constant I -> cosh one // cos(j*x) = cosh(x)
        | Product [Number n; Constant I] -> cosh (Number n)
        | Product ((Number n)::(Constant I)::ax) -> cosh (multiply (Number n) (product ax))
        | Product ((Constant I)::ax) -> cosh (product ax)

        | Sum ax -> let (m, others) = findFirstPiTerm ax // cos(x) has period 2*pi
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> sin (sum others) |> negate // cos(z + 1/2*pi) = -sin(z)
                    | m' when m'.Equals(1N) -> cos (sum others) |> negate // cos(z + pi) = -cos(z)
                    | m' when m'.Equals(3N/2N) -> sin (sum others) |> negate // cos(z + 3/2*pi) = sin(z)
                    | m' when m'.Equals(2N) -> sin (sum others) // cos(z + 2*pi) = sin(z)
                    | _ -> Function (Cos, sum ax)

        | Product [Number n; Constant Pi] -> exactValue n  // cos(m/n*pi)

        | _ as x' -> Function (Cos, x')
    and sinh x =
        let rec exactValue (n:BigRational) =
            match n with
            | n when n.Equals(1N/2N) -> Constant I // sinh(1/2*pi*j) = j
            | n when n.Equals(1N) -> zero // sinh(pi*j) = 0
            | n when n.Equals(3N/2N) -> Constant I |> negate // sinh(3/2*pi*j) = -j
            | _ -> Function (Sinh, product [(Number n); pi; Constant I;])
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> infinity // sinh(oo) = oo, sinh(-oo) = -oo

        | Zero -> zero

        | Number n when n.IsNegative -> sinh (Number -n) |> negate // sinh(-x) = -sinh(x)
        | Product ((Number n)::ax) when n.IsNegative -> sinh (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> sinh (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> sinh (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> sin one |> multiply (Constant I) // sinh(j*x) = j*sin(x)
        | Product [Number n; Constant I] -> sin (Number n) |> multiply (Constant I)
        | Product ((Number n)::(Constant I)::ax) -> sin (multiply (Number n) (product ax)) |> multiply (Constant I)
        | Product ((Constant I)::ax) -> sin (product ax) |> multiply (Constant I)

        | Sum ax -> let (m, others) = findFirstPiJTerm ax // sinh(x) has period 2*pi*j
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with 
                    | m' when m'.Equals(1N/2N) -> cosh (sum others) |> multiply (Constant I) // sinh(z + 1/2*pi*j) = j*cosh(z)
                    | m' when m'.Equals(1N) -> sinh (sum others) |> negate // sinh(z + pi*j) = -sinh(z)
                    | m' when m'.Equals(3N/2N) -> cosh (sum others) |> multiply (Constant I) |> negate // sinh(z + 3/2*pi*j) = -j*cosh(z)
                    | m' when m'.Equals(2N) -> sinh (sum others) // sinh(z + 2*pi*j) = sinh(z)
                    | _ -> Function (Sinh, Sum ax)

        | Product [(Constant Pi); (Constant I);] -> exactValue 1N // sinh(m/n*pi*j)
        | Product [(Number n); (Constant Pi); (Constant I);] -> exactValue n

        | _ as x' -> Function (Sinh, x')
    and cosh x =
        let rec exactValue (n:BigRational) =
            match n with
            | n when n = (1N/2N) -> zero // cosh(1/2*pi*j) = 0
            | n when n = (1N) -> minusOne //cosh(pi*j) = -1
            | n when n = (3N/2N) -> zero // cosh(3/2*pi*j) = 0
            | _ -> Function (Cosh, product [(Number n); pi; Constant I;])
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | oo when isInfinity oo -> infinity // cosh(oo) = cosh(-oo) = oo

        | Zero -> one // cosh(0) = 1
        
        | Number n when n.IsNegative -> sinh (Number -n) // cosh(-x) = cosh(x)
        | Product ((Number n)::ax) when n.IsNegative -> sinh (multiply (Number -n) (Product ax)) 
        | Sum ((Number n)::ax) when n.IsNegative -> sinh (add (Number -n) (Sum (ax |> List.map negate))) 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> sinh (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate)))

        | Constant I -> cos one // cosh(j*x) = cos(x)
        | Product [Number n; Constant I] -> cos (Number n)
        | Product ((Number n)::(Constant I)::ax) -> cos (multiply (Number n) (product ax))
        | Product ((Constant I)::ax) -> cos (product ax)

        | Sum ax -> let (m, others) = findFirstPiJTerm ax // cosh(x) has period 2*pi*j
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> sinh (sum others) |> multiply (Constant I) // cosh(z + 1/2*pi*j) = j*sinh(z)
                    | m' when m'.Equals(1N) -> cosh (sum others) |> negate // cosh(z + pi*j) = -cosh(z)
                    | m' when m'.Equals(3N/2N) -> sinh (sum others) |> multiply (Constant I) |> negate // cosh(z + 3/2*pi*j) = -j*sinh(z)
                    | m' when m'.Equals(2N) -> cosh (sum others) // cosh(z + 2*pi*j) = cosh(z)
                    | _ -> Function (Cosh, sum ax)

        | Product [(Constant Pi); (Constant I);] -> exactValue 1N // cosh(m/n*pi*j)
        | Product [(Number n); (Constant Pi); (Constant I);] -> exactValue n

        | _ as x' -> Function (Cosh, x')
    let rec tan x =
        let rec exactValue (n:BigRational) =
            let n' = modTwo n // make sure n in [0, 2]
            match n' with
            | n' when n'.IsInteger -> zero
            | n' when n' > 1N -> exactValue (n' - 1N) // shift by pi, tan(x) = tan(x + pi)
            | n' when n' > 1N/2N -> exactValue (1N - n') |> negate // reflection at pi/2, tan(x) = -tan(pi - x)
            | n' when tangentTable.ContainsKey(n') -> tangentTable.[n']
            | _ -> Function (Tan, multiply (Number n') pi)
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined

        | Zero -> zero // tan(0) = 0

        | Number n when n.IsNegative -> tan (Number -n) |> negate // tan(-x) = -tan(x)
        | Product ((Number n)::ax) when n.IsNegative -> tan (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> tan (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> tan (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> tanh one |> multiply (Constant I) // tan(j*x) = j*tanh(x)
        | Product [Number n; Constant I] -> tanh (Number n) |> multiply (Constant I)
        | Product ((Number n)::(Constant I)::ax) -> tanh (multiply (Number n) (product ax)) |> multiply (Constant I)
        | Product ((Constant I)::ax) -> tanh (product ax) |> multiply (Constant I)

        | Sum ax -> let (m, others) = findFirstPiTerm ax // tan(x) has period pi
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> cot (sum others) |> negate // tan(z + 1/2*pi) = -cot(z)
                    | m' when m'.Equals(1N) -> tan (sum others) // tan(z + pi) = tan(z)                    
                    | m' when m'.Equals(3N/2N) -> cot (sum others) |> negate // tan(z + 3/2*pi) = -cot(z)
                    | m' when m'.Equals(2N) -> tan (sum others) // tan(z + 2*pi) = tan(z)
                    | _ -> Function (Tan, sum ax)

        | Product [Number n; Constant Pi] -> exactValue n // tan(m/n*pi)

        | _ as x' -> Function (Tan, x')
    and cot x =
        let rec exactValue (n:BigRational) =
            let n' = modTwo n // make sure n in [0, 2]
            match n' with
            | n' when n'.IsInteger -> complexInfinity
            | n' when n' > 1N -> exactValue (n' - 1N) // shift by pi, cot(x) = cot(x + pi)
            | n' when n' > 1N/2N -> exactValue (1N - n') |> negate // reflection at pi/2, cot(x) = -cot(pi - x)
            | n' when tangentTable.ContainsKey(n') -> tangentTable.[n'] |> invert
            | _ -> Function (Cot, multiply (Number n') pi)
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined

        | Zero | Constant Pi -> complexInfinity // cot(0) = cot(pi) = ⧝
        
        | Number n when n.IsNegative -> cot (Number -n) |> negate // cot(-x) = -cot(x)
        | Product ((Number n)::ax) when n.IsNegative -> cot (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> cot (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> cot (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> coth one |> multiply (Constant I) |> negate // cot(j*x) = -j*coth(x)
        | Product [Number n; Constant I] -> coth (Number n) |> multiply (Constant I) |> negate
        | Product ((Number n)::(Constant I)::ax) -> coth (multiply (Number n) (product ax)) |> multiply (Constant I) |> negate
        | Product ((Constant I)::ax) -> coth (product ax) |> multiply (Constant I) |> negate

        | Sum ax -> let (m, others) = findFirstPiTerm ax // cot(x) has period pi
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> tan (sum others) |> negate // cot(z + 1/2*pi) = -tan(z)
                    | m' when m'.Equals(1N) -> cot (sum others) // cot(z + pi) = cot(z)
                    | m' when m'.Equals(3N/2N) -> tan (sum others) |> negate // cot(z + 3/2*pi) = -tan(z)
                    | m' when m'.Equals(2N) -> cot (sum others) // cot(z + 2*pi) = cot(z)
                    | _ -> Function (Cot, sum ax)

        | Product [Number n; Constant Pi] -> exactValue n // cot(m/n*pi)

        | _ as x' -> Function (Cot, x')    
    and tanh x =
        let rec exactValue (n:BigRational) =
            match n with
            | n when n = (1N/2N) -> ComplexInfinity // tanh(1/2*pi*j) = ?
            | n when n = (1N) -> zero // tanh(pi*j) = 0
            | n when n = (3N/2N) -> ComplexInfinity // tanh(3/2*pi*j) = ?
            | _ -> Function (Tanh, product [(Number n); pi; Constant I;])
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> one // tanh(oo) = 1

        | Zero -> zero // tanh(0) = 0

        | Number n when n.IsNegative -> tan (Number -n) |> negate // tanh(-x) = -tanh(x)
        | Product ((Number n)::ax) when n.IsNegative -> tanh (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> tanh (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> tanh (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate 

        | Constant I -> tan one |> multiply (Constant I) // tanh(j*x) = j*tan(x)
        | Product [Number n; Constant I] -> tan (Number n) |> multiply (Constant I)
        | Product ((Number n)::(Constant I)::ax) -> tan (multiply (Number n) (product ax)) |> multiply (Constant I)
        | Product ((Constant I)::ax) -> tan (product ax) |> multiply (Constant I)

        | Sum ax -> let (m, others) = findFirstPiJTerm ax // tanh(x) has period 2*pi*j
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> coth (sum others) // tanh(z + 1/2*pi*j) = coth(z)
                    | m' when m'.Equals(1N) -> tanh (sum others) // tanh(z + pi*j) = tanh(z)
                    | m' when m'.Equals(3N/2N) -> coth (sum others) // tanh(z + 3/2*pi*j) = coth(z)
                    | m' when m'.Equals(2N) -> tanh (sum others) // tanh(z + 2*pi*j) = tanh(z)
                    | _ -> Function (Tanh, sum ax)

        | Product [(Constant Pi); (Constant I);] -> exactValue 1N // tanh(m/n*pi*j)
        | Product [(Number n); (Constant Pi); (Constant I);] -> exactValue n

        | _ as x' -> Function (Tanh, x')
    and coth x =
        let rec exactValue (n:BigRational) =
            match n with
            | n when n = (1N/2N) -> zero // coth(1/2*pi*j) = 0
            | n when n = (1N) -> ComplexInfinity // coth(pi*j) = ?
            | n when n = (3N/2N) -> zero // coth(3/2*pi*j) = 0
            | _ -> Function (Coth, product [(Number n); pi; Constant I;])
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> one

        | Zero -> complexInfinity    
        
        | Number n when n.IsNegative -> coth (Number -n) |> negate // coth(-x) = -coth(x)
        | Product ((Number n)::ax) when n.IsNegative -> coth (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> coth (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> coth (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate 

        | Constant I -> cot one |> multiply (Constant I) |> negate // coth(j*x) = -j*cot(x)
        | Product [Number n; Constant I] -> cot (Number n) |> multiply (Constant I) |> negate
        | Product ((Number n)::(Constant I)::ax) -> cot (multiply (Number n) (product ax)) |> multiply (Constant I) |> negate
        | Product ((Constant I)::ax) -> cot (product ax) |> multiply (Constant I) |> negate

        | Sum ax -> let (m, others) = findFirstPiJTerm ax // coth(x) has period pi*j
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N) -> coth (sum others) // coth(z + pi*j) = coth(z)
                    | m' when m'.Equals(1N/2N) -> tanh (sum others) // coth(z + 1/2*pi*j) = tanh(z)
                    | m' when m'.Equals(3N/2N) -> tanh (sum others) // coth(z + 3/2*pi*j) = tanh(z)
                    | m' when m'.Equals(2N) -> coth (sum others) // coth(z + 2*pi*j) = coth(z)
                    | _ -> Function (Coth, sum ax)

        | Product [(Constant Pi); (Constant I);] -> exactValue 1N // coth(m/n*pi*j)
        | Product [(Number n); (Constant Pi); (Constant I);] -> exactValue n

        | _ as x' -> Function (Coth, x')
    let rec csc x =
        let rec exactValue (n:BigRational) =
            let n' = modTwo n // wrap n into [0, 2]
            match n' with
            | n' when n'.IsInteger -> complexInfinity
            | n' when n' > 1N -> negate (exactValue (n' - 1N)) // shift by pi, csc(x) = -csc(x + pi)
            | n' when n' > 1N/2N -> exactValue (1N - n') // reflection at pi/2, csc(x) = csc(pi - x)
            | n' when sineTable.ContainsKey(n') -> sineTable.[n'] |> invert
            | _ -> Function (Csc, multiply (Number n') pi)
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined

        | Zero | Constant Pi-> complexInfinity // csc(0) = csc(pi) = ⧝

        | Number n when n.IsNegative -> csc (Number -n) |> negate // csc(-x) = -csc(x)
        | Product ((Number n)::ax) when n.IsNegative -> csc (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> csc (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> csc (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> csch one |> multiply (Constant I) |> negate // csc(j*x) = -j*csch(x)
        | Product [Number n; Constant I] -> csch (Number n) |> multiply (Constant I) |> negate
        | Product ((Number n)::(Constant I)::ax) -> csch (multiply (Number n) (product ax)) |> multiply (Constant I) |> negate
        | Product ((Constant I)::ax) -> csch (product ax) |> multiply (Constant I) |> negate

        | Sum ax -> let (m, others) = findFirstPiTerm ax // csc(x) has period 2*pi
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> sec (sum others) // csc(z + 1/2*pi) = sec(z)
                    | m' when m'.Equals(1N) -> csc (sum others) |> negate // csc(z + pi) = -csc(z)
                    | m' when m'.Equals(3N/2N) -> sec (sum others) |> negate // csc(z + 3/2*pi) = -sec(z)
                    | m' when m'.Equals(2N) -> csc (sum others) // csc(z + 2*pi) = csc(z)
                    | _ -> Function (Csc, sum ax)

        | Product [Number n; Constant Pi] -> exactValue n // csc(m/n*pi)

        | _ as x' -> Function (Csc, x')
    and sec x =
        let rec exactValue (n:BigRational) =
            let n' = modTwo n // make sure n in [0, 2]
            match n' with
            | n' when (n' + 1N/2N).IsInteger -> complexInfinity
            | n' when n' > 1N -> exactValue (n' - 1N) |> negate // shift by pi, sec(x) = -sec(x + pi)
            | n' when n' > 1N/2N -> exactValue (1N - n') |> negate // reflection at pi/2, sec(x) = -sec(pi - x)
            | n' when sineTable.ContainsKey(1N/2N - n') -> sineTable.[1N/2N - n'] |> invert // reflection at pi/4, sec(x) = 1/sin(pi/2 - x)
            | _ -> Function (Sec, multiply (Number n') pi)
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined

        | Zero -> one // sec(0) = 1
        | Constant Pi -> minusOne // sec(pi) = -1
        
        | Number n when n.IsNegative -> sec (Number -n) // sec(-x) = sec(x)
        | Product ((Number n)::ax) when n.IsNegative -> sec (multiply (Number -n) (Product ax)) 
        | Sum ((Number n)::ax) when n.IsNegative -> sec (add (Number -n) (Sum (ax |> List.map negate))) 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> sec (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate)))

        | Constant I -> sech one // sec(j*x) = sech(x)
        | Product [Number n; Constant I] -> sech (Number n)
        | Product ((Number n)::(Constant I)::ax) -> sech (multiply (Number n) (product ax))
        | Product ((Constant I)::ax) -> sech (product ax)

        | Sum ax -> let (m, others) = findFirstPiTerm ax // sec(x) has period 2*pi
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> csc (sum others) |> negate // sec(z + 1/2*pi) = -csc(z)
                    | m' when m'.Equals(1N) -> sec (sum others) |> negate // sec(z + pi) = -sec(z)
                    | m' when m'.Equals(3N/2N) -> csc (sum others) // sec(z + 3/2*pi) = csc(z)
                    | m' when m'.Equals(2N) -> csc (sum others) // sec(z + 2*pi) = -sec(z)
                    | _ -> Function (Sec, sum ax)

        | Product [Number n; Constant Pi] -> exactValue n // sec(m/n*pi)

        | _ as x' -> Function (Sec, x')
    and csch x =
        let rec exactValue (n:BigRational) =
            match n with
            | n when n = (1N/2N) -> Constant I |> negate // csch(1/2*pi*j) = -j
            | n when n = (1N) -> ComplexInfinity // csch(pi*j) = ?
            | n when n = (3N/2N) -> Constant I// csch(3/2*pi*j) = j
            | _ -> Function (Csch, product [(Number n); pi; Constant I;])
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | oo when isInfinity oo -> zero

        | Number n when n.IsNegative -> csch (Number -n) |> negate // csch(-x) = -csch(x)
        | Product ((Number n)::ax) when n.IsNegative -> csch (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> csch (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> csch (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> csc one |> multiply (Constant I) |> negate // csch(j*x) = -j*csc(x)
        | Product [Number n; Constant I] -> csc (Number n) |> multiply (Constant I) |> negate
        | Product ((Number n)::(Constant I)::ax) -> csc (multiply (Number n) (product ax)) |> multiply (Constant I) |> negate
        | Product ((Constant I)::ax) -> csc (product ax) |> multiply (Constant I) |> negate

        | Sum ax -> let (m, others) = findFirstPiJTerm ax 
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> sech (sum others) |> multiply (Constant I) // csch(z + 1/2*pi*j) = j*sech(z)
                    | m' when m'.Equals(1N) -> csch (sum others) |> negate // csch(z + pi*j) = -csch(z)                    
                    | m' when m'.Equals(3N/2N) -> sech (sum others) |> multiply (Constant I) |> negate // csch(z + 3/2*pi*j) = j*sech(z)
                    | m' when m'.Equals(2N) -> csch (sum others) // csch(z + 2*pi*j) = csch(z)
                    | _ -> Function (Csch, sum ax)

        | Product [(Constant Pi); (Constant I);] -> exactValue 1N // csch(m/n*pi*j)
        | Product [(Number n); (Constant Pi); (Constant I);] -> exactValue n

        | _ as x' -> Function (Csch, x')    
    and sech x =
        let rec exactValue (n:BigRational) =
            match n with
            | n when n = (1N/2N) -> ComplexInfinity // sech(1/2*pi*j) = ?
            | n when n = (1N) -> minusOne // sech(pi*j) = -1
            | n when n = (3N/2N) -> ComplexInfinity // sech(3/2*pi*j) = ?
            | _ -> Function (Sech, product [(Number n); pi; Constant I;])
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | oo when isInfinity oo -> zero

        | Zero -> one
        
        | Number n when n.IsNegative -> sech (Number -n) // sech(-x) = sech(x)
        | Product ((Number n)::ax) when n.IsNegative -> sech (multiply (Number -n) (Product ax)) 
        | Sum ((Number n)::ax) when n.IsNegative -> sech (add (Number -n) (Sum (ax |> List.map negate))) 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> sech (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate)))

        | Constant I -> sec one // sech(j*x) = sec(x)
        | Product [Number n; Constant I] -> sec (Number n)
        | Product ((Number n)::(Constant I)::ax) -> sec (multiply (Number n) (product ax))
        | Product ((Constant I)::ax) -> sec (product ax)

        | Sum ax -> let (m, others) = findFirstPiJTerm ax 
                    let m' = modTwo(m) // wrap in [0, 2]
                    match m' with
                    | m' when m'.Equals(1N/2N) -> csch (sum others) |> multiply (Constant I) |> negate // sech(z + 1/2*pi*j) = -j*csch(z)
                    | m' when m'.Equals(1N) -> csch (sum others) |> negate // sech(z + pi*j) = -sech(z)
                    | m' when m'.Equals(3N/2N) -> csch (sum others) |> multiply (Constant I) // sech(z + 3/2*pi*j) = j*csch(z)
                    | m' when m'.Equals(2N) -> csch (sum others) // sech(z + 2*pi*j) = sech(z)
                    | _ -> Function (Sech, sum ax)

        | Product [(Constant Pi); (Constant I);] -> exactValue 1N // sech(m/n*pi*j)
        | Product [(Number n); (Constant Pi); (Constant I);] -> exactValue n

        | _ as x' -> Function (Sech, x')    
    let rec arcsin x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | Zero -> zero
        | One -> divide pi two
        
        | Number n when n.IsNegative -> arcsin (Number -n) |> negate // arcsin(-x) = -arcsin(x)
        | Product ((Number n)::ax) when n.IsNegative -> arcsin (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> arcsin (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> arcsin (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate 

        | Constant I -> arcsinh one |> multiply (Constant I)  // asin(j*x) = j*asinh(x)
        | Product [Number n; Constant I] -> arcsinh (Number n) |> multiply (Constant I)
        | Product ((Number n)::(Constant I)::ax) -> arcsinh (multiply (Number n) (product ax)) |> multiply (Constant I)
        | Product ((Constant I)::ax) -> arcsinh (product ax) |> multiply (Constant I)

        | _ as x' -> Function (Asin, x')
    and arccos x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> multiply infinity (Constant I) // acos(oo) = oo*j

        | Zero -> divide pi two // acos(0) = pi/2
        | One -> zero // acos(1) = 0

        | Number n when n.IsNegative -> subtract (pi) (arccos (Number -n)) // acos(-x) = pi - acos(x)
        | Product ((Number n)::ax) when n.IsNegative -> subtract (pi) (arccos (multiply (Number -n) (Product ax)))
        | Sum ((Number n)::ax) when n.IsNegative -> subtract (pi) (arccos (add (Number -n) (Sum (ax |> List.map negate))))
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> subtract (pi) (arccos (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))))

        | Constant I -> subtract (divide pi two |> negate) (arcsinh one |> multiply (Constant I)) // acos(j) = pi/2 - j*asinh(1)
        
        | _ as x' -> Function (Acos, x')
    and arcsinh x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> infinity // asinh(oo) = oo

        | Zero -> zero // asinh(0) = 0
        
        | Number n when n.IsNegative -> arcsinh (Number -n) |> negate // asinh(-x) = -asinh(x)
        | Product ((Number n)::ax) when n.IsNegative -> arcsinh (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> arcsinh (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> arcsinh (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate 

        | Constant I -> divide (multiply pi (Constant I)) two |> multiply (Constant I) // asinh(j*x) = j*asin(x)
        | Product [Number n; Constant I] -> arcsin (Number n) |> multiply (Constant I)
        | Product ((Number n)::(Constant I)::ax) -> arcsin (multiply (Number n) (product ax)) |> multiply (Constant I)
        | Product ((Constant I)::ax) -> arcsin (product ax) |> multiply (Constant I)

        | _ as x' -> Function (Asinh, x')    
    let rec arctan x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> divide pi two // atan(oo) = pi/2, atan(-oo) = -pi/2

        | Zero -> zero // atan(0) = 0
        | One -> divide pi four // atan(1) = pi/4, atan(-1) = -pi/4

        | Number n when n.IsNegative -> arctan (Number -n) |> negate // atan(-x) = -atan(x)
        | Product ((Number n)::ax) when n.IsNegative -> arctan (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> arctan (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> arctan (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> multiply infinity (Constant I) // atan(j) = j*oo, atan(j*x) = j*atanh(x)
        | Product [Number n; Constant I] -> arctanh (Number n) |> multiply (Constant I)
        | Product ((Number n)::(Constant I)::ax) -> arctanh (multiply (Number n) (product ax)) |> multiply (Constant I)
        | Product ((Constant I)::ax) -> arctanh (product ax) |> multiply (Constant I)

        | _ as x' -> Function (Atan, x')
    and arctanh x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> divide pi two |> multiply (Constant I) |> negate // atanh(oo) = - pi*j/2, atanh(-oo) = pi*j/2

        | Zero -> zero // atanh(0) = 0
        | One -> infinity // atanh(1) = oo, atanh(-1) = -oo
        
        | Number n when n.IsNegative -> arctanh (Number -n) |> negate // atanh(-x) = -atanh(x)
        | Product ((Number n)::ax) when n.IsNegative -> arctanh (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> arctanh (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> arctanh (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> divide (multiply pi (Constant I)) four // atanh(j*x) = j*atan(x), atanh(j) = pi*j/4
        | Product [Number n; Constant I] -> arctan (Number n) |> multiply (Constant I)
        | Product ((Number n)::(Constant I)::ax) -> arctan (multiply (Number n) (product ax)) |> multiply (Constant I)
        | Product ((Constant I)::ax) -> arctan (product ax) |> multiply (Constant I)

        | _ as x' -> Function (Atanh, x')
    let arctan2 x y = FunctionN (Atan, [x;y])
    let rec arccsc x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> zero

        | Zero -> complexInfinity
        | One -> divide pi two        
        | Number n when n = (2N) -> divide pi six

        | Number n when n.IsNegative -> arccsc (Number -n) |> negate // acsc(-x) = -acsc(x)
        | Product ((Number n)::ax) when n.IsNegative -> arccsc (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> arccsc (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> arccsc (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | _ as x' -> Function (Acsc, x')
    let rec arcsec x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> divide pi two // asec(oo) = pi/2

        | Zero -> complexInfinity // asec(0) = ⧝
        | One -> zero // asec(1) = 0        
        | Number n when n = (2N) -> divide pi three
        
        | Number n when n.IsNegative -> subtract (pi) (arcsec (Number -n)) // asec(-x) = pi - asec(x)
        | Product ((Number n)::ax) when n.IsNegative -> subtract (pi) (arcsec (multiply (Number -n) (Product ax)))
        | Sum ((Number n)::ax) when n.IsNegative -> subtract (pi) (arcsec (add (Number -n) (Sum (ax |> List.map negate))))
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> subtract (pi) (arcsec (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))))

        | _ as x' -> Function (Asec, x')
    let rec arccot x =
        match expandArg x with
        | Undefined -> undefined
        | oo when isInfinity oo -> zero // acot(⧝) = acot(oo) = acot(-oo) = 0

        | Zero -> divide pi two // acot(0) = pi/2
        | One -> divide pi four // acot(1) = pi/4        

        | Number n when n.IsNegative -> arccot (Number -n) |> negate // acot(-x) = -acot(x)
        | Product ((Number n)::ax) when n.IsNegative -> arccot (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> arccot (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> arccot (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | _ as x' -> Function (Acot, x')    
    let rec arccosh x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | oo when isInfinity oo -> infinity // acosh(oo) = oo, acosh(-oo) = oo

        | Zero -> divide (multiply pi (Constant I)) two // acosh(0) = pi*j/2
        | One -> zero // acosh(1) = 0
        | MinusOne -> multiply pi (Constant I) // acosh(-1) = pi*j

        | x' -> Function (Acosh, x')    
    let rec arccsch x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> zero // acsch(oo) = 0

        | Zero -> complexInfinity // acsch(0) = ⧝
        | One -> complexInfinity // acsch(1) = ⧝
        
        | Number n when n.IsNegative -> arccsch (Number -n) |> negate // acsch(-x) = -acsch(x)
        | Product ((Number n)::ax) when n.IsNegative -> arccsch (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> arccsch (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> arccsch (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> negate (divide (multiply pi (Constant I)) two) // acsch(j) = - pi*j/2

        | _ as x' -> Function (Acsch, x')
    let rec arcsech x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> divide (multiply pi (Constant I)) two // asech(oo) = pi*j/2
        | NegativeInfinity -> divide (multiply pi (Constant I)) two // asech(-oo) = pi*j/2

        | Zero -> infinity // asech(0) = ∞
        | One -> zero // asech(1) = 0
        | MinusOne -> multiply pi (Constant I) // asech(-1) = pi*j

        | x' -> Function (Asech, x')
    let rec arccoth x =
        match expandArg x with
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> zero // acoth(oo) = 0
        | Zero -> divide (multiply pi (Constant I)) two // acoth(0) = pi*j/2
        | One -> infinity // acoth(1) = oo
        
        | Number n when n.IsNegative -> arccoth (Number -n) |> negate // acoth(-x) = -acoth(x)
        | Product ((Number n)::ax) when n.IsNegative -> arccoth (multiply (Number -n) (Product ax)) |> negate 
        | Sum ((Number n)::ax) when n.IsNegative -> arccoth (add (Number -n) (Sum (ax |> List.map negate))) |> negate 
        | Sum (Product ((Number n)::ax)::bx) when n.IsNegative -> arccoth (add (multiply (Number -n) (Product ax)) (Sum (bx |> List.map negate))) |> negate

        | Constant I -> divide (multiply pi (Constant I)) four |> negate // acoth(j*x) = -j*cot(x), acoth(j) = -pi*j/4
        | Product [Number n; Constant I] -> arccot (Number n) |> multiply (Constant I) |> negate
        | Product ((Number n)::(Constant I)::ax) -> arccot (multiply (Number n) (product ax)) |> multiply (Constant I) |> negate
        | Product ((Constant I)::ax) -> arccot (product ax) |> multiply (Constant I) |> negate
        | _ as x' -> Function (Acoth, x')

    let apply f x =
        match f with
        | Abs -> abs x
        | Exp -> exp x
        | Ln -> ln x
        | Log -> log10 x
        | Sin -> sin x
        | Cos -> cos x
        | Tan -> tan x
        | Csc -> csc x
        | Cot -> cot x
        | Sec -> sec x
        | Sinh -> sinh x
        | Cosh -> cosh x
        | Tanh -> tanh x
        | Csch -> csch x
        | Sech -> sech x
        | Coth -> coth x   
        | Asin -> arcsin x
        | Acos -> arccos x
        | Atan -> arctan x
        | Acsc -> arccsc x
        | Asec -> arcsec x
        | Acot -> arccot x
        | Asinh -> arcsinh x
        | Acosh -> arccosh x
        | Atanh -> arctanh x
        | Acsch -> arccsch x
        | Asech -> arcsech x
        | Acoth -> arccoth x
        

    let applyN (f: Function) (xs: Expression list) =
        match f, xs with
        | Atan, [x;y] -> arctan2 x y
        | Log, [b; x] -> log b x
        | _ -> failwith "not supported"


type Expression with

    static member Zero = Operators.zero
    static member One = Operators.one
    static member Two = Operators.two
    static member MinusOne = Operators.minusOne
    static member FromInt32 (x:int) = Operators.fromInt32 x
    static member FromInt64 (x:int64) = Operators.fromInt64 x
    static member FromInteger (x:BigInteger) = Operators.fromInteger x
    static member FromIntegerFraction (n:BigInteger, d:BigInteger) = Operators.fromIntegerFraction n d
    static member FromRational (x:BigRational) = Operators.fromRational x
    static member Symbol (name:string) = Operators.symbol name
    static member Real (floatingPoint:float) = Operators.real floatingPoint

    static member I = Constant I
    static member E = Constant E
    static member Pi = Operators.pi

    static member ( ~+ ) (x:Expression) = Operators.plus x
    static member ( ~- ) (x:Expression) = Operators.negate x
    static member ( + ) ((x:Expression), (y:Expression)) = Operators.add x y
    static member ( - ) ((x:Expression), (y:Expression)) = Operators.subtract x y
    static member ( * ) ((x:Expression), (y:Expression)) = Operators.multiply x y
    static member ( / ) ((x:Expression), (y:Expression)) = Operators.divide x y

    static member Pow (x, y) = Operators.pow x y
    static member Invert (x) = Operators.invert x

    static member Abs (x) = Operators.abs x

    static member Root (n, x) = Operators.root n x
    static member Sqrt (x) = Operators.sqrt x

    static member Exp (x) = Operators.exp x
    static member Ln (x) = Operators.ln x
    static member Log(x) = Operators.log10 x
    static member Log (basis, x) = Operators.log basis x

    static member Sin (x) = Operators.sin x
    static member Cos (x) = Operators.cos x
    static member Tan (x) = Operators.tan x
    static member Csc (x) = Operators.csc x
    static member Sec (x) = Operators.sec x
    static member Cot (x) = Operators.cot x

    static member Sinh (x) = Operators.sinh x
    static member Cosh (x) = Operators.cosh x    
    static member Tanh (x) = Operators.tanh x
    static member Coth (x) = Operators.coth x
    static member Csch (x) = Operators.csch x
    static member Sech (x) = Operators.sech x

    static member ArcSin (x) = Operators.arcsin x
    static member ArcCos (x) = Operators.arccos x
    static member ArcTan (x) = Operators.arctan x
    static member ArcCsc (x) = Operators.arccsc x
    static member ArcSec (x) = Operators.arcsec x
    static member ArcCot (x) = Operators.arccot x

    static member ArcSinh (x) = Operators.arcsinh x
    static member ArcCosh (x) = Operators.arccosh x
    static member ArcTanh (x) = Operators.arctanh x
    static member ArcCsch (x) = Operators.arccsch x
    static member ArcSech (x) = Operators.arcsech x
    static member ArcCoth (x) = Operators.arccoth x

    static member Apply (f, x) = Operators.apply f x
    static member ApplyN (f, xs) = Operators.applyN f xs

    // Simpler usage - numbers
    static member ( + ) (x, (y:int)) = x + (Operators.number y)
    static member ( + ) ((x:int), y) = (Operators.number x) + y
    static member ( - ) (x, (y:int)) = x - (Operators.number y)
    static member ( - ) ((x:int), y) = (Operators.number x) - y
    static member ( * ) (x, (y:int)) = x * (Operators.number y)
    static member ( * ) ((x:int), y) = (Operators.number x) * y
    static member ( / ) (x, (y:int)) = x / (Operators.number y)
    static member ( / ) ((x:int), y) = (Operators.number x) / y
    static member Pow (x, (y:int)) = Operators.pow x (Operators.number y)

    // Simpler usage - approximations
    static member ( + ) (x, (y:float)) = x + (Operators.real y)
    static member ( + ) ((x:float), y) = (Operators.real x) + y
    static member ( - ) (x, (y:float)) = x - (Operators.real y)
    static member ( - ) ((x:float), y) = (Operators.real x) - y
    static member ( * ) (x, (y:float)) = x * (Operators.real y)
    static member ( * ) ((x:float), y) = (Operators.real x) * y
    static member ( / ) (x, (y:float)) = x / (Operators.real y)
    static member ( / ) ((x:float), y) = (Operators.real x) / y

    // Simpler usage in C#
    static member op_Implicit (x:int) = Operators.fromInt32(x)
    static member op_Implicit (x:int64) = Operators.fromInt64(x)
    static member op_Implicit (x:BigInteger) = Operators.fromInteger(x)
    static member op_Implicit (x:BigRational) = Operators.fromRational(x)
    static member op_Implicit (x:float) = Operators.real x


[<RequireQualifiedAccess>]
module NumericLiteralQ =
    let FromZero () = Expression.Zero
    let FromOne () = Expression.One
    let FromInt32 (x:int) = Expression.FromInt32 x
    let FromInt64 (x:int64) = Expression.FromInt64 x
    let FromString str = Expression.FromRational (BigRational.Parse str)
