namespace FSharp.Stats.Distributions

/// Represents a histogram (map from values to integer frequencies).
module Frequency =
    open FSharp.Stats

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
    let getZip (hist:Map<_,int>) =
        hist |> Seq.sortBy (fun kv -> kv.Key) |> Seq.map (fun kv -> (kv.Key,kv.Value))

    /// Returns the total of the frequencies in the map
    let sum (hist:Map<_,int>) =
        hist |> Seq.sumBy (fun kv -> kv.Value)
    
    /// Returns the average of the frequencies in the map
    let average (hist:Map<_,int>) =
        hist
        |> Map.fold (fun (sum,count) k v -> sum + v, count + 1 ) (0,0)
        |> fun (sum,count) -> float sum / (float count)

    /// Gets the largest frequency in the map.
    let maxLike (hist:Map<_,int>) =
        (hist |> Seq.maxBy (fun kv -> kv.Value)).Value
        
    /// Gets the frequency associated with the value x
    let frequencyAt (hist:Map<'a,int>) (x:'a) =        
        if hist.ContainsKey(x) then
            hist.[x]
        else
            0            
    
    /// Gets an unsorted sequence of frequencies
    let frequencies (hist:Map<_,int>) =         
        hist |> Seq.map (fun k -> k.Value)

    /// Checks whether the values in this histogram A are a subset of the values in the histogram B
    let isSubset (histA:Map<_,int>) (histB:Map<_,int>) =
        let rec issubset (histA:list<float*int>) (histB:Map<float,int>) =
            match histA with
            | head::rest -> let k,v = head
                            let y = frequencyAt histB k                              
                            if v > y then false else issubset rest histB
            | []         -> true
        issubset (histA |> Map.toList) histB


    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value in histA is superseded by the value in histB.</summary>
    /// <param name="histA">Frequency map A</param>
    /// <param name="histB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (merge a b) is not equal to (merge b a)</remarks> 
    /// <returns>New frequency map that results from merged maps histA and histB.</returns> 
    let merge (histA: Map<_,'value>) (histB: Map<_,'value>) = 
        Map.merge histA histB

    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value from histB is subtracted from the value of histA.</summary>
    /// <param name="histA">Frequency map A</param>
    /// <param name="histB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (subtract a b) is not equal to (subtract b a)</remarks> 
    let inline subtract (histA: Map<_,'value>) (histB: Map<_,'value>) = 
        Map.mergeSubtract histA histB
    
    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value from mapB is added to the value of mapA.</summary>
    /// <param name="histA">Frequency map A</param>
    /// <param name="histB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (add a b) is not equal to (add b a)</remarks> 
    /// <returns>New frequency map that results from merged maps histA and histB. Values from keys that are present in both maps are handled by f</returns> 
    let inline add (histA: Map<_,'value>) (histB: Map<_,'value>) = 
        Map.mergeAdd histA histB

