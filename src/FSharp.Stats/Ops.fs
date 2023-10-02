namespace FSharp.Stats

#nowarn "40"
#nowarn "42"

/// Operations module (automatically opened)
[<AutoOpen>]
module Ops =
    open System
    open System

    /// The constant pi = 3.141596...
    let pi = System.Math.PI

    ///// The constant pi = 3.141596...
    //let sqrt2pi = nan 
    
    /// Float positive infinity.
    let inf = System.Double.PositiveInfinity
    
    /// Float negative infinity.
    let infNeg = System.Double.NegativeInfinity    

    /// <summary>
    ///   Double-precision machine round-off error.
    /// </summary>
    /// 
    /// <remarks>
    ///   This value is actually different from Double.Epsilon. It
    ///   is defined as 1.11022302462515654042E-16.
    /// </remarks>
    let epsilon = 1.11022302462515654042e-16

    /// <summary>
    ///   Maximum log (def: 7.09782712893383996732E2).
    /// </summary>
    let logMax = 7.09782712893383996732E2

    /// <summary>
    ///   Minimum log (def: -7.451332191019412076235E2)
    /// </summary>
    let logMin = -7.451332191019412076235E2
    
    ///// Float NaN.
    //let NaN = System.Double.NaN
    
    /// <summary>Returns the logarithm for x in base 2.</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let log2 x = System.Math.Log(x, 2.0)
    
    /// <summary>Returns the logarithm for x in base 10.</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let log10 x = System.Math.Log10(x)    

    /// Returs true if x is nan (generics) equality
    //let inline isNan< ^T when ^T : equality > (num:^T) :  bool = num <> num
    let inline isNan num = num <> num

    /// <summary>Returs true if x is infinity (generics)</summary>
    /// <remarks></remarks>
    /// <param name="isInf"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline isInf< ^T when ^T : 
        (static member IsInfinity : ^T -> bool)> (num:^T) :bool =
      (^T : (static member IsInfinity : ^T -> bool) (num))

    /// <summary>Returs true if x is positive infinity (generics)</summary>
    /// <remarks></remarks>
    /// <param name="isPosInf"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline isPosInf< ^T when ^T : 
        (static member IsPositiveInfinity : ^T -> bool)> (num:^T) :bool =
      (^T : (static member IsPositiveInfinity : ^T -> bool) (num))
    
    /// <summary>Returs true if x is positive infinity (generics)</summary>
    /// <remarks></remarks>
    /// <param name="isNegInf"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline isNegInf< ^T when ^T : 
        (static member IsNegativeInfinity : ^T -> bool)> (num:^T) :bool =
      (^T : (static member IsNegativeInfinity : ^T -> bool) (num))

    /// <summary>Returns the reverted log2 (2^x)</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let revLog2 x = 2.**x

    /// <summary>Returns x squared (x^2)</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline square x = x * x

    ///
    let arsinh x =  
        x + sqrt(square x + 1.) |> log

    /// <summary>Rounds a double-precision floating-point value to a specified number of fractional digits.  </summary>
    /// <remarks></remarks>
    /// <param name="digits"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let round (digits:int) (x:float) =
        System.Math.Round(x, digits)
    
    /// <summary>Signum function, assigns a positive sign to a with respect to the signing of b. </summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let signum (a:float) (b:float) =
        if b >= 0. then 
            if a >= 0. then a else -a
        else
            if a >= 0. then -a else a 

    open Microsoft.FSharp.Reflection

    [<NoDynamicInvocation>]
    //let inline private retype (x : 'a) : 'b = (# "" x : 'b #)
    let inline retype<'T,'U> (x:'T) : 'U = (# "" x : 'U #)

    let inline multByInt32 (x:'T) (n:int) : 'T =
        x * (retype n)
        


    let inline nthroot n (A:'T) : 'U =
        let rec f x =
            let m = n - 1
            let m' = multByInt32 x m
            let x' = (m' + A / (pown x m))
            let x'' = LanguagePrimitives.DivideByInt< 'U > x' n
            if float (abs(x'' - x))  < 1e-9 then 
                x''
            else
                f x''
    
        f (LanguagePrimitives.DivideByInt< 'U > A n)
