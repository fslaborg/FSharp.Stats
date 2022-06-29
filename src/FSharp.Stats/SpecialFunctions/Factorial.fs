namespace FSharp.Stats.SpecialFunctions

open System

/// Functions for computing the factorial of a number.
///
/// The factorial of a non-negative integer n, denoted by n!, is the product of all positive integers less than or equal to n:
///
/// n! = n * (n-1) * (n-2) * ... * 2 * 1
///
/// For example, 5! = 5 * 4 * 3 * 2 * 1 = 120
///
/// The value of 0! is 1.
module Factorial =

    // This is the largest integer value for which the factorial function doesn't overflow the floating point format.
    let private FactorialMax = 170

    //cache of all factorials in [0..170]
    let private FactorialCache =
        //let cache =  [| 0 .. FactorialMax |] |> Array.map (fun a -> float a)
        let cache = Array.zeroCreate (FactorialMax + 1)
        cache.[0] <- 1.0
        for i=1 to FactorialMax do
            cache.[i] <- cache.[i-1] * (float i)
        cache

    // The size of the table of factorials in logarithm. The value FactorialLnNTop D 2 should be increased if your integer arguments are often larger
    let private FactorialLnNTop = 2000

    let private FactorialLnCache =
        let cache = Array.zeroCreate (FactorialLnNTop + 1)
        for i=0 to FactorialLnNTop do
            cache.[i] <- Gamma._gammaLn ((float i) + 1.0)
        cache

    ///<summary>
    /// Computes the factorial of integers <= 170.
    ///
    /// The factorial functions takes an int x and returns x!. This function will not overflow
    ///
    /// the floating point format as long as x is at most 170, and will return +infinity for all values > 170
    ///<summary>
    ///<param name="x">The input to compute x!<param>
    let factorial (x:int) =    
        if x <= FactorialMax then
            FactorialCache.[x]
        else
            System.Double.PositiveInfinity
        
    ///<summary>
    /// Computes the natural logarithm of the factorial.
    ///
    /// Values will be approximated for x > 170, as the approximated _gammaLn function is used to obtain the results for large inputs.
    ///
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///<summary>
    ///<param name="x">The input to compute x!<param>
    let _factorialLn (x: int) : float =
        if x < 0 then failwith "Log factorial not defined for n < 0"
        //if x <= 1 then 0.0 else Gamma.gammaLn ((float x) + 1.0)
        if x <= FactorialLnNTop then 
            FactorialLnCache.[x]
        else 
            Gamma._gammaLn ((float x) + 1.0)

    ///<summary>
    /// Computes the natural logarithm of the factorial.
    ///
    /// Values will be approximated for x > 170, as the approximated _gammaLn function is used to obtain the results for large inputs.
    ///
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    ///
    /// This might be slower than the unchecked version `_factorialLn` but does not require input sanitation to get expected results for these cases.
    ///<summary>
    ///<param name="x">The input to compute x!<param>
    let factorialLn (x: int) : float =
        if x < 0 then failwith "Log factorial not defined for n < 0"
        //if x <= 1 then 0.0 else Gamma.gammaLn ((float x) + 1.0)
        if x <= FactorialLnNTop then 
            FactorialLnCache.[x]
        else 
            Gamma.gammaLn ((float x) + 1.0)