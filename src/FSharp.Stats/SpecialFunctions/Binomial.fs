namespace FSharp.Stats.SpecialFunctions

open System

/// Special mathematical functions
module Binomial =
    
    // Returns the binomial coeffcient(n | k) as a ﬂoating-point number
    let coeffcient (n:int) (k:int) = 
        if ( n < 0 || k < 0 || k > n) then invalidArg "Binomial.coeffcient" ""
        if (n < 171) then 
            //floor (0.5 + Factorial.factorial n / ((Factorial.factorial k) * (Factorial.factorial (n-k))))
            Factorial.factorial n / ((Factorial.factorial k) * (Factorial.factorial (n-k)))
        else
            floor (0.5 + exp ((Factorial.factorialLn n) - (Factorial.factorialLn k) - (Factorial.factorialLn (n-k))))
 
    // Returns the natural logarithm of the binomial coefficient(n | k) as a ﬂoating-point number
    let coeffcientLn (n:int) (k:int) = 
        if ( n < 0 || k < 0 || k > n) then invalidArg "Binomial.coeffcient" ""
        (Factorial.factorialLn n) - (Factorial.factorialLn k) - (Factorial.factorialLn (n-k))

