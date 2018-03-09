namespace FSharp.Stats.Distributions

/// Represents a histogram (map from values to integer frequencies).
module Frequency =

    /// Given the list [a,b,a,c,b,b], produce a map {a:2, b:3, c:1} which contains the count of each unique item in the list
    let createGeneric list = 
        let rec histogram' list' dict' =
            match list' with
            | []      -> dict'
            | x :: xs -> 
                match Map.tryFind x dict' with
                | Some(value) -> histogram' xs (Map.add x (value + 1) dict')
                | None        -> histogram' xs (Map.add x 1 dict')
        histogram' list Map.empty
        

    /// Creates probability mass function (histogram)    
    let create bandwidth data =            
        let halfBw = bandwidth / 2.0       
        data
        |> Seq.groupBy (fun x -> floor (x / bandwidth)) 
        |> Seq.map (fun (k,values) -> 
            let count = (Seq.length(values))                                         
            if k < 0. then
                ((k  * bandwidth) + halfBw, count)   
            else
                ((k + 1.) * bandwidth) - halfBw, count)  
        |> Map.ofSeq

       
    /// Returns tuple of (sorted value sequence, frequence sequence)
    let getZip (hist:Map<float,int>) =
        hist |> Seq.sortBy (fun kv -> kv.Key) |> Seq.map (fun kv -> (kv.Key,kv.Value))

    /// Returns the total of the frequencies in the map
    let sum (hist:Map<float,int>) =
        hist |> Seq.sumBy (fun kv -> kv.Value)
        
    /// Gets the largest frequency in the map.
    let maxLike (hist:Map<float,int>) =
        (hist |> Seq.maxBy (fun kv -> kv.Value)).Value
        

    /// Gets the frequency associated with the value x
    let frequencyAt (hist:Map<float,int>) (x:float) =        
        if hist.ContainsKey(x) then
            hist.[x]
        else
            0            
    
    /// Gets an unsorted sequence of frequencies
    let frequencies (hist:Map<float,int>) =         
        hist |> Seq.map (fun k -> k.Value)

    /// Checks whether the values in this histogram A are a subset of the values in the histogram B
    let isSubset (histA:Map<float,int>) (histB:Map<float,int>) =
        let rec issubset (histA:list<float*int>) (histB:Map<float,int>) =
            match histA with
            | head::rest -> let k,v = head
                            let y = frequencyAt histB k                              
                            if v > y then false else issubset rest histB
            | []         -> true
        issubset (histA |> Map.toList) histB

    ///// Subtracts the values histogramA from histogramB
    //let subtract (histA:Map<'a,int>) (histB:Map<'a,int>) =
    //    Map.merge histA histB (fun k (v, v') -> v - v')

    ////// Adds the values in histogramA to histogramB
    //let add (histA:Map<'a,int>) (histB:Map<'a,int>) =
    //    Map.merge histA histB (fun k (v, v') -> v + v')


