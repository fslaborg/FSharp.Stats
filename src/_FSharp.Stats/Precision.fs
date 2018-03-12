namespace FSharp.Stats


/// Module to estimate different quantile measures
module Precision =
   
    /// Compares two float values and determines if they differ by no more than specified by the maximum error.
    let almostEqualNormRelative maximumError a b  = 
        if a |> isInf || b |> isInf then
            a = b 
        elif a |> isNan || b |> isNan then
            false 
        elif ((a - b) |> abs) < maximumError  then 
            true
        else 
            false 

    /// Compares two float values and determines if they differ by no more than 10.*(2.**(-52.).
    let almostEqualNorm a b = 
        almostEqualNormRelative (10.*(2.**(-52.))) a b 
        
    
