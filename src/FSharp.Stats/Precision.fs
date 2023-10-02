namespace FSharp.Stats


/// Module to estimate different quantile measures
module Precision =
   
    /// <summary>Compares two float values and determines if they differ by no more than specified by the maximum error.</summary>
    /// <remarks></remarks>
    /// <param name="maximumError"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let almostEqualNormRelative maximumError a b  = 
        if a |> isInf || b |> isInf then
            a = b 
        elif a |> isNan || b |> isNan then
            false 
        elif ((a - b) |> abs) < maximumError  then 
            true
        else 
            false 

    /// <summary>Compares two float values and determines if they differ by no more than 10.*(2.**(-52.).</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let almostEqualNorm a b = 
        almostEqualNormRelative (10.*(2.**(-52.))) a b 
        
    
