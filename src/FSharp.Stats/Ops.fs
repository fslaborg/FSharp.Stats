namespace FSharp.Stats

/// Operations module (automatically opened)
[<AutoOpen>]
module Ops =

    /// The constant pi = 3.141596...
    let pi = System.Math.PI
    
    /// Float infinity.
    let inf = System.Double.PositiveInfinity
    
    /// Float NaN.
    let NaN = System.Double.NaN
    
    /// Returns the logarithm for x in base 2.
    let log2 x = System.Math.Log(x, 2.0)
    
    /// Returns the logarithm for x in base 10.
    let log10 x = System.Math.Log10(x)    

    /// Returs true if x is Float NaN
    let isNan x = nan.Equals(x)

    /// Returs true if x is Float infinity
    let isInf x = System.Double.IsInfinity x  



    /// Returns the reverted log2 (2^x)
    let revLog2 x = 2.**x

    /// Returns x squared (x^2)
    let inline square x = x * x


    let arsinh x =  
        x + sqrt(square x + 1.) |> log


    /// Rounds a double-precision floating-point value to a specified number of fractional digits.  
    let round (digits:int) (x:float) =
        System.Math.Round(x, digits)
    
