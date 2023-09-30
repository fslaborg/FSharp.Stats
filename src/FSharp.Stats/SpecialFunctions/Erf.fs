namespace FSharp.Stats.SpecialFunctions

open System

/// Error function (erf) and related functions.
///
/// the error function (also called the Gauss error function), often denoted by erf, is a complex function of a complex variable defined as:
///
/// erf (z) = 2/√π * \int e^(-t²) dt from 0 to z
///
/// This integral is a special (non-elementary) sigmoid function that occurs often in probability, statistics, and partial differential equations. In many of these applications, the function argument is a real number. If the function argument is real, then the function value is also real.
/// 
/// In statistics, for non-negative values of x, the error function has the following interpretation: for a random variable Y that is normally distributed with mean 0 and standard deviation 
/// 1/√2 , erf x is the probability that Y falls in the range [−x, x].
module Errorfunction =

    open System

    /// <summary>Computes the error function. Note that this implementation has only been verified to have a relative error of around 1e-5.</summary>
    /// <remarks></remarks>
    /// <param name="Erf"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let rec Erf x =
        // Reference: Abramowitz and Stegun - Handbook of Mathematical Functions, p299.
        if x < 0.0 then
            - (Erf (-x))
        else
            let p  =  0.3275911
            let a1 =  0.254829592
            let a2 = -0.284496736
            let a3 =  1.421413741
            let a4 = -1.453152027
            let a5 =  1.061405429
            let t = 1.0 / (1.0 + p*x)
            1.0 - (exp (-(x*x))) * t * (a1 + t*(a2 + t*(a3 + t*(a4 + t*a5))))

    /// <summary>Computes the complement of the error function. Note that this implementation has only been verified to have a relative error of around 1e-4.</summary>
    /// <remarks></remarks>
    /// <param name="Erfc"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let rec Erfc x =
        // Reference: Abramowitz and Stegun - Handbook of Mathematical Functions, p299.
        if x < 0.0 then
            2.0 - (Erfc (-x))
        else
            let p  =  0.3275911
            let a1 =  0.254829592
            let a2 = -0.284496736
            let a3 =  1.421413741
            let a4 = -1.453152027
            let a5 =  1.061405429
            let t = 1.0 / (1.0 + p*x)
            (exp (-(x*x))) * t * (a1 + t*(a2 + t*(a3 + t*(a4 + t*a5))))

    /// <summary>
    /// Scaled complementary error function, exp(x**2) * erfc(x).
    ///
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</summary>
    ///<param name="x">Input to compute exp(x**2) * erfc(x)</param>
    let _erfcx x =
        if x < 25. then
            Erfc(x) * exp(x*x)
        else
            let y = 1. / x
            let z = y * y
            let s = y*(1.+z*(-0.5+z*(0.75+z*(-1.875+z*(6.5625-29.53125*z)))))
            s * 0.564189583547756287
            
    /// <summary>
    /// Scaled complementary error function, exp(x**2) * erfc(x).
    ///
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    ///
    /// This might be slower than the unchecked version `_erfcx` but does not require input sanitation to get expected results for these cases.
    ///</summary>
    ///<param name="x">Input to compute exp(x**2) * erfc(x)</param>
    let erfcx x =
        match x with
        | x when (infinity.Equals(x)) -> nan
        | x when ((-infinity).Equals(x)) -> infinity
        | _ -> _erfcx x

    //helper arrays for inverse error function
    let private a = [|0.886226899; -1.645349621; 0.914624893; -0.140543331|]
    let private b = [|1.; -2.118377725; 1.442710462; -0.329097515; 0.012229801|]
    let private c = [|-1.970840454; -1.62490649; 3.429567803; 1.641345311|]
    let private d = [|1.; 3.543889200; 1.637067800|]
 
    /// <summary>inverse of error function. uses newton refinement; from https://libit.sourceforge.net/<br />accuracy to the fifth digit</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inverf x = 
        match x with
        | x when x = -1. -> -infinity
        | x when x = 1. -> infinity
        | _ -> 
            let sqrtPi = sqrt Math.PI

            let z = abs x
    
            let r =
                if z <= 0.7 then
                    let x2 = x * x
                    let r = z * (((a.[3] * x2 + a.[2]) * x2 + a.[1]) * x2 + a.[0])
                    r / ((((b.[4] * x2 + b.[3]) * x2 + b.[2]) * x2 + b.[1]) * x2 + b.[0])
                else 
                    let y = sqrt( -log((1. - z)/2.))
                    let r = (((c.[3] * y + c.[2]) * y + c.[1]) * y + c.[0])
                    r / ((d.[2] * y + d.[1]) * y + d.[0])
   
 
            let r' = r * float (sign x)
            let z' = z * float (sign x)
 
            let r'' = r' - (Erf(r') - z')/(2./sqrtPi * exp(-r' * r'))
            r'' - (Erf(r'') - z')/(2./sqrtPi *exp(-r'' * r''))

    /// <summary>inverse of complementary error function<br />accuracy to the fifth digit</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inverfc x =
        inverf (1. - x)
