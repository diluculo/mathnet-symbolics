module SpecialFunctions

open Expecto
open MathNet.Symbolics
open Operators

let tests =
    testList "SpecialFunctions" [
    
        testList "ChebyshevT" [
            test "Special Values" {
                chebyshevt -2Q x ==> "-1 + 2*x^2"
                chebyshevt -1Q x ==> "x"
                chebyshevt 0Q x ==> "1"
                chebyshevt 1Q x ==> "x"
                chebyshevt 2Q x ==> "-1 + 2*x^2"
                chebyshevt 3Q x |> Algebraic.expand ==> "-3*x + 4*x^3"
                chebyshevt 10Q x |> Algebraic.expand ==> "-1 + 50*x^2 - 400*x^4 + 1120*x^6 - 1280*x^8 + 512*x^10"

                chebyshevt n -1Q ==> "(-1)^n"
                chebyshevt n 1Q ==> "1"

                chebyshevt -n x ==> "chebyshevt(n,x)"
                chebyshevt n -x ==> "(-1)^n*chebyshevt(n,x)"

                chebyshevt 6Q (Constant I) |> Algebraic.expand ==> "-1 + 18*j^2 - 48*j^4 + 32*j^6" // "-99"
            }

            test "Calculus" {
                Calculus.differentiate x (chebyshevt n x) ==> "n*chebyshevu(-1 + n,x)"
            }
        ]

        testList "ChebyshevU" [
            test "Special Values" {
                chebyshevu -1Q x ==> "0"
                chebyshevu 0Q x ==> "1"
                chebyshevu 1Q x ==> "2*x"
                chebyshevu 2Q x ==> "-1 + 4*x^2"
                chebyshevu 3Q x |> Algebraic.expand ==> "-4*x + 8*x^3"
                chebyshevu 10Q x |> Algebraic.expand ==> "-1 + 60*x^2 - 560*x^4 + 1792*x^6 - 2304*x^8 + 1024*x^10"

                chebyshevu n -1Q ==> "(-1)^n*(1 + n)"
                chebyshevu n 1Q ==> "1 + n"

                chebyshevu -n x ==> "-chebyshevu(-2 + n,x)"
                chebyshevu n -x ==> "(-1)^n*chebyshevu(n,x)"

                chebyshevu 4Q (Constant I) |> Algebraic.expand ==> "1 - 12*j^2 + 16*j^4" // "29"
            }

            test "Calculus" {
                Calculus.differentiate x (chebyshevu 0Q x) ==> "0"
                Calculus.differentiate x (chebyshevu 1Q x) ==> "2"
                Calculus.differentiate x (chebyshevu n x) ==> "((1 + n)*chebyshevt(1 + n,x) - x*chebyshevu(n,x))/(-1 + x^2)"
            }
        ]
        
    ]
