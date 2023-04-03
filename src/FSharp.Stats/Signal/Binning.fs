namespace FSharp.Stats.Signal


open FSharp.Stats
open System

module Binning = 

    /// <summary> 
    /// Sorts the data into bins with given bandwidth.
    /// </summary>
    /// <param name="projection">Function to isolate the feature to bin by.</param>
    /// <param name="bandwidth">Bandwidth of the resulting bins.</param>    
    /// <param name="data">Data of type 'a with a float field to bin by.</param>    
    /// <exception cref="System.OverflowException">Thrown when the input sequence contains nan.</exeption>
    /// <exception cref="System.OverflowException">Thrown when the input sequence contains nan.</exeption>
    let binBy (projection: 'a -> float) bandwidth (data: seq<'a>) =
        if bandwidth = 0. then raise (System.DivideByZeroException("Bandwidth cannot be 0."))
        let halfBw = bandwidth / 2.0
        let decBandwidth = decimal bandwidth
        let tmp = 
            data
            |> Seq.groupBy (fun x -> (decimal (projection x) / decBandwidth) |> float |> floor) 
            |> Seq.map (fun (k,values) -> 
                let count = (Seq.length(values)) |> float
                if k < 0. then
                    ((k  * bandwidth) + halfBw, values)   
                else
                    ((k + 1.) * bandwidth) - halfBw, values)
            |> Seq.sortBy fst
        tmp    
        |> Map.ofSeq
        
    /// <summary> 
    /// Sorts the data into bins with given bandwidth.
    /// </summary>
    /// <param name="bandwidth">Bandwidth of the resulting bins.</param>
    /// <param name="data">float data</param>
    /// <exception cref="System.OverflowException">Thrown when the input sequence contains nan.</exeption>
    let bin bandwidth data = 
        binBy id bandwidth data
