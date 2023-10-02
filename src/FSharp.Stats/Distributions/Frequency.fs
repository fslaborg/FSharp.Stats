namespace FSharp.Stats.Distributions

/// Represents a histogram (map from values to integer frequencies).
module Frequency =
    open FSharp.Stats

    /// <summary>Given the list [a,b,a,c,b,b], produce a map {a:2, b:3, c:1} which contains the count of each unique item in the list</summary>
    /// <remarks></remarks>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let createGeneric list = 
        let rec histogram' list' dict' =
            match list' with
            | []      -> dict'
            | x :: xs -> 
                match Map.tryFind x dict' with
                | Some(value) -> histogram' xs (Map.add x (value + 1) dict')
                | None        -> histogram' xs (Map.add x 1 dict')
        histogram' list Map.empty
        
    /// <summary>Creates probability mass function (histogram)    </summary>
    /// <remarks></remarks>
    /// <param name="bandwidth"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
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
       
    /// <summary>Returns tuple of (sorted value sequence, frequence sequence)</summary>
    /// <remarks></remarks>
    /// <param name="hist"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getZip (hist:Map<_,int>) =
        hist |> Seq.sortBy (fun kv -> kv.Key) |> Seq.map (fun kv -> (kv.Key,kv.Value))

    /// <summary>Returns the total of the frequencies in the map</summary>
    /// <remarks></remarks>
    /// <param name="hist"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let sum (hist:Map<_,int>) =
        hist |> Seq.sumBy (fun kv -> kv.Value)
    
    /// <summary>Returns the average of the frequencies in the map</summary>
    /// <remarks></remarks>
    /// <param name="hist"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let average (hist:Map<_,int>) =
        hist
        |> Map.fold (fun (sum,count) k v -> sum + v, count + 1 ) (0,0)
        |> fun (sum,count) -> float sum / (float count)

    /// <summary>Gets the largest frequency in the map.</summary>
    /// <remarks></remarks>
    /// <param name="hist"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let maxLike (hist:Map<_,int>) =
        (hist |> Seq.maxBy (fun kv -> kv.Value)).Value
        
    /// <summary>Gets the frequency associated with the value x</summary>
    /// <remarks></remarks>
    /// <param name="hist"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let frequencyAt (hist:Map<'a,int>) (x:'a) =        
        if hist.ContainsKey(x) then
            hist.[x]
        else
            0
    
    /// <summary>Gets an unsorted sequence of frequencies</summary>
    /// <remarks></remarks>
    /// <param name="hist"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let frequencies (hist:Map<_,int>) =         
        hist |> Seq.map (fun k -> k.Value)

    /// <summary>Checks whether the values in this histogram A are a subset of the values in the histogram B</summary>
    /// <remarks></remarks>
    /// <param name="histA"></param>
    /// <param name="histB"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let isSubset (histA:Map<_,int>) (histB:Map<_,int>) =
        let rec issubset (histA:list<float*int>) (histB:Map<float,int>) =
            match histA with
            | head::rest -> 
                let k,v = head
                let y = frequencyAt histB k                              
                if v > y then 
                    false 
                else 
                    issubset rest histB
            | []         -> true
        issubset (histA |> Map.toList) histB


    /// <summary>Merges two histograms into a single histogram. If a key exists in both maps, the value is determined by f with the first value being from mapA and the second originating from mapB.</summary>
    /// <param name="equalBandwidthOrNominal">Is the binwidth equal for both frequencies? For nominal data set to true.</param>
    /// <param name="f">Function to transform values if key is present in both histograms. `mapA-value &#8594; mapB-value &#8594; newValue`</param>
    /// <param name="mapA">Frequency map A</param>
    /// <param name="mapB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (mergeBy f a b) is not equal to (mergeBy f b a)</remarks> 
    /// <returns>New frequency map that results from merged maps mapA and mapB. Values from keys that are present in both maps are handled by f</returns> 
    let mergeBy equalBandwidthOrNominal f (histA: Map<_,'value>) (histB: Map<_,'value>) =
        if equalBandwidthOrNominal then 
            Map.mergeBy f histA histB
        else 
            failwithf "Not implemented yet. If continuous data shall be merged, bandwidth must be equal. This does not matter for nominal data!"
            //ToDo:
            //    Dissect both frequencies and construct a new one based on given bandwidths
            //    New bandwidth might be double the largest observed bandwidth to not miss-sort any data. 

    /// <summary>Merges two histograms into a single histogram. If a key exists in both histograms, the value in histA is superseded by the value in histB.</summary>
    /// <param name="equalBandwidthOrNominal">Is the binwidth equal for both frequencies? For nominal data set to true.</param>
    /// <param name="histA">Frequency map A</param>
    /// <param name="histB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (merge a b) is not equal to (merge b a)</remarks> 
    /// <returns>New frequency map that results from merged maps histA and histB.</returns> 
    let merge equalBandwidthOrNominal (histA: Map<_,'value>) (histB: Map<_,'value>) = 
        mergeBy equalBandwidthOrNominal (fun a b -> b) histA histB

    /// <summary>Merges two histograms into a single histogram. If a key exists in both histograms, the value from histB is subtracted from the value of histA.</summary>
    /// <param name="equalBandwidthOrNominal">Is the binwidth equal for both frequencies? For nominal data set to true.</param>
    /// <param name="histA">Frequency map A</param>
    /// <param name="histB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (subtract a b) is not equal to (subtract b a)</remarks> 
    let inline subtract equalBandwidthOrNominal (histA: Map<_,'value>) (histB: Map<_,'value>) = 
        mergeBy equalBandwidthOrNominal (fun a b -> a - b) histA histB
    
    /// <summary>Merges two histograms into a single histogram. If a key exists in both histograms, the value from histA is added to the value of histB.</summary>
    /// <param name="equalBandwidthOrNominal">Is the binwidth equal for both frequencies? For nominal data set to true.</param>
    /// <param name="histA">Frequency map A</param>
    /// <param name="histB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <returns>New frequency map that results from merged maps histA and histB. Values from keys that are present in both maps are handled by f</returns> 
    let inline add equalBandwidthOrNominal (histA: Map<_,'value>) (histB: Map<_,'value>) = 
        mergeBy equalBandwidthOrNominal (fun a b -> a + b) histA histB

