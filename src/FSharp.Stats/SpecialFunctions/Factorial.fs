namespace FSharp.Stats.SpecialFunctions

open System

/// Special mathematical functions
module Factorial =

    // This is the largest integer value for which the factorial function doesn't overflow the floating point format.
    let private FactorialMax = 170

    //let inline facti n =
    //    let rec loop acc n = 
    //       if n<=LanguagePrimitives.GenericOne 
    //        then acc 
    //        else loop (n*acc) (n-LanguagePrimitives.GenericOne)
    //    loop LanguagePrimitives.GenericOne n

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
            cache.[i] <-Gamma.gammaLn ((float i) + 1.0)
        cache

    /// The factorial functions takes an int x and computes x!. This function will not overflow
    /// the floating point format as long as x is at most 170.
    let factorial (x:int) =    
        if x <= FactorialMax then
            FactorialCache.[x]
        else
            System.Double.PositiveInfinity
        
    /// Computes the natural logarithm of the factorial function.
    let factorialLn (x: int) : float =
        if x < 0 then failwith "Log factorial not defined for n < 0"
        //if x <= 1 then 0.0 else Gamma.gammaLn ((float x) + 1.0)
        if x <= FactorialLnNTop then 
            FactorialLnCache.[x]
        else 
            Gamma.gammaLn ((float x) + 1.0)



//    let cacheFactorial size f = 
//        let cache = Array.zeroCreate size
//        (fun x ->  
//            match cache.[x] with
//            | 0 -> 
//                let v = f(x)
//                cache.[x] <- v
//                v
//            | y -> y ) 
//                
//
//    let rec factorial =
//        cacheFactorial FactorialMax (
//            fun x -> 
//                if (x = 0) then 
//                    1 
//                else 
//                    x * factorial(x - 1))
//
//    let rec factorialLn =
//        cacheFactorial FactorialMax (
//            fun x -> 
//                if (x = 0) then 
//                    1 
//                else 
//                    x * factorial(x - 1))
//
//    let inline facti n =
//        let rec loop acc n = 
//           if n<=LanguagePrimitives.GenericOne 
//            then acc 
//            else loop (n*acc) (n-LanguagePrimitives.GenericOne)
//        loop LanguagePrimitives.GenericOne n


 