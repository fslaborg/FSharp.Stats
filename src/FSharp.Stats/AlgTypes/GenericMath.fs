namespace FSharp.Stats

open System
open System.Runtime.InteropServices

module GenericMath =

    /// Generic Zero
    let inline zero<'T when 'T :> Numerics.INumber<'T>> = 
        'T.Zero

    /// Generic One
    let inline one<'T when 'T :> Numerics.INumber<'T>> = 
        'T.One
    
    /// Generic float conversion
    let inline T<'T when 'T :> Numerics.INumber<'T>> (x: float) : 'T =
        'T.CreateChecked x

    /// Generic square root
    let inline sqrt<'T when 'T :> Numerics.IRootFunctions<'T>> (x: 'T) : 'T =
        'T.Sqrt(x)

    /// Generic exponentiation (e^x)
    let inline exp<'T when 'T :> Numerics.IExponentialFunctions<'T>> (x: 'T) : 'T =
        'T.Exp(x)

    /// Generic power function (x^y)
    let inline pow<'T when 'T :> Numerics.IPowerFunctions<'T>> (x: 'T) (y: 'T) : 'T =
        'T.Pow(x, y)

    /// Generic natural logarithm
    let inline log<'T when 'T :> Numerics.ILogarithmicFunctions<'T>> (x: 'T) : 'T =
        'T.Log(x)

    /// Generic absolute value
    let inline abs<'T when 'T :> Numerics.INumber<'T>> (x: 'T) : 'T =
        'T.Abs(x)

    /// Generic sine
    let inline sin<'T when 'T :> Numerics.ITrigonometricFunctions<'T>> (x: 'T) : 'T =
        'T.Sin(x)

    /// Generic cosine
    let inline cos<'T when 'T :> Numerics.ITrigonometricFunctions<'T>> (x: 'T) : 'T =
        'T.Cos(x)

    /// Generic pi constant
    let inline pi<'T when 'T :> Numerics.IFloatingPointConstants<'T> and 'T :> Numerics.INumber<'T>> () : 'T =
         'T.Pi

    /// Generic e constant (Euler's number)
    let inline e<'T when 'T :> Numerics.IFloatingPointConstants<'T>> () : 'T =
        'T.E

    /// Generic tau constant (2 * pi)
    let inline tau<'T when 'T :> Numerics.IFloatingPointConstants<'T> and 'T :> Numerics.INumber<'T>> () : 'T =
         'T.Tau

    /// Generic floor function
    let inline floor<'T when 'T :> Numerics.IFloatingPoint<'T>> (x: 'T) : 'T =
        'T. Floor(x)

    /// Generic floor function
    let inline epsilon<'T when 'T :> Numerics.IFloatingPoint<'T>> () : 'T =
        'T.CreateTruncating System.Double.Epsilon
 
    // let inline min x y = if x < y then x else y
    // let inline max x y = if x > y then x else y

    // let inline clamp minVal maxVal x =
    //     if x < minVal then minVal elif x > maxVal then maxVal else x

