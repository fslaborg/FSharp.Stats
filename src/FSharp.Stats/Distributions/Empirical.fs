namespace FSharp.Stats.Distributions

open FSharp.Stats

/// Represents a probability mass function (map from values to probabilities).
module Empirical =
    open System

    /// Creates Pmf of a Histogram (normalize by n)
    let ofHistogram (hist:Map<_,int>) =
        let n = float (hist |> Map.fold (fun state key value -> state + value) 0)
        hist |> Seq.map (fun kv -> (kv.Key,(float kv.Value) / n )) |> Map.ofSeq

    /// Returns: tuple of (sorted value sequence, probability sequence)
    let getZip (pmf:Map<_,float>) =
        pmf |> Seq.sortBy (fun kv -> kv.Key) |> Seq.map (fun kv -> (kv.Key,kv.Value))

    /// Returns the total of the probabilities in the map
    let sum (pmf:Map<_,float>) =
        pmf |> Seq.sumBy (fun kv -> kv.Value)
        
    /// Returns the largest probability in the map.
    let maxLike (pmf:Map<_,float>) =
        (pmf |> Seq.maxBy (fun kv -> kv.Value)).Value
    
    /// Returns distinct values from pmf
    let getXValues (pmf:Map<_,float>) =
        pmf |> Seq.map (fun k -> k.Key)

    /// Returns distinct values from pmf
    let getYValues (pmf:Map<_,float>) =
        pmf |> Seq.map (fun k -> k.Value)

    /// Gets the probability associated with the value x
    let probabilityAt  (pmf:Map<'a,float>) (x:'a) =        
        if pmf.ContainsKey(x) then
            pmf.[x]
        else
            0.            
    
    /// Gets an unsorted sequence of probabilities
    let probabilities  (pmf:Map<'a,float>) =         
        pmf |> Seq.map (fun k -> k.Value)


    ///// Subtracts the values pmfA from pmfB
    //let subtract (pmfA:Map<float,float>) (pmfB:Map<float,float>) =
    //    Map.merge pmfA pmfB (fun k (v, v') -> v - v')

    ///// Adds the values in pmfA to pmfB
    //let add (pmfA:Map<float,float>) (pmfB:Map<float,float>) =
    //    Map.merge pmfA pmfB (fun k (v, v') -> v + v')

    
    /// Normalizes this PMF so the sum of all probabilities equals fraction
    let normalizewith (fraction:float) (pmf:Map<_,float>) =
        let total = sum pmf
        let factor = if total <> 0. then (fraction / total) else raise (System.Exception("total probability is zero") )  
        pmf |> Seq.map (fun kv -> (kv.Key,kv.Value * factor)) |> Map.ofSeq

    /// Normalizes this PMF so the sum of all probabilities equals 1. 
    /// Discrete Probability Distribution
    let normalize (pmf:Map<_,float>) =
        let total = sum pmf         
        pmf |> Seq.map (fun kv -> (kv.Key,kv.Value / total)) |> Map.ofSeq


    /// Normalizes this PMF so the sum of all probabilities equals 100 percent 
    /// Discrete Percentage Probability Distribution
    let normalizePercentage (pmf:Map<_,float>) =
        let total = sum pmf         
        pmf |> Seq.map (fun kv -> (kv.Key,100. * kv.Value / total)) |> Map.ofSeq

    /// Normalizes this PMF by the bandwidth n/Δx
    /// Frequency Denisty Distribution
    let normalizeBandwidth bw (pmf:Map<_,float>) =
        pmf |> Seq.map (fun kv -> (kv.Key,kv.Value / bw)) |> Map.ofSeq

    /// Normalizes this PMF by the bandwidth to area equals 1.  (n/N)/Δx
    /// Probability Denisty Distribution
    let normalizePDD bw (pmf:Map<_,float>) =
        let total = sum pmf         
        pmf |> Seq.map (fun kv -> (kv.Key,(kv.Value / total) / bw)) |> Map.ofSeq


    /// Chooses a random element from this PMF
    let sampleFrom (pmf:Map<_,float>) = 
        if pmf.Count <= 0 then raise (System.Exception("Pmf contains no values") )  
        let target = FSharp.Stats.Random.rndgen.NextFloat()
        //pmf |> Seq.map (fun kv -> (kv.Key,kv.Value)) |> Seq.scan (fun state (k,v) -> (k, v + snd state)) (0.,0.)
        let x,y =
            pmf
            |> Seq.scan (fun state kv -> (kv.Key, kv.Value + snd state)) (0.,0.)
            |> Seq.find (fun (x,y) -> y >= target)
        x

    /// Chooses a random element from this PMF
    [<Obsolete("Use sampleFrom instead.")>]
    let random pmf = 
        sampleFrom pmf
    
    /// Computes the mean of a PMF
    let mean (pmf:Map<float,float>) =
        pmf |> Map.fold (fun state key value -> state + (key * value)) 0.

    /// Computes the variance of a PMF around mu
    let varAround (mu:float) (pmf:Map<float,float>) =
        pmf |> Map.fold (fun state key value -> state + (value * (key - mu)**2. )) 0.

    /// Computes the variance of a PMF
    let var (pmf:Map<float,float>) =
        let mu = mean pmf
        pmf |> Map.fold (fun state key value -> state + (value * (key - mu)**2. )) 0.

    /// Log transforms the probabilities
    let log (pmf:Map<_,float>) =
        let m = maxLike pmf
        pmf |> Seq.map (fun kv -> (kv.Key, log(kv.Value / m))) |> Map.ofSeq
         
    /// Exponentiates the probabilities
    let exp (pmf:Map<_,float>) =
        let m = maxLike pmf
        pmf |> Seq.map (fun kv -> (kv.Key, exp(kv.Value - m))) |> Map.ofSeq


    ///// Make a mixture distribution
    //let makeMixture (pmfs:seq<Map<float,float>>) = 
    //    pmfs |> Seq.fold (fun state elem -> Map.merge state elem (fun k (v, v') -> v * v')) Map.empty

    
    /// Creates probability mass function of the input sequence.
    /// The bandwidth defines the width of the bins the numbers are sorted into. 
    /// Bin intervals are half open excluding the upper border: [lower,upper)
    let create bandwidth data =
        let halfBw = bandwidth / 2.0
        let decBandwidth = decimal bandwidth
        let tmp = 
            data
            |> Seq.groupBy (fun x -> (decimal x / decBandwidth) |> float |> floor) 
            |> Seq.map (fun (k,values) -> 
                let count = (Seq.length(values)) |> float                                        
                if k < 0. then
                    ((k  * bandwidth) + halfBw, count)   
                else
                    ((k + 1.) * bandwidth) - halfBw, count)
            |> Seq.sortBy fst
        tmp    
        |> Map.ofSeq
        |> normalize
      
    /// Creates probability mass function of the categories in the input sequence.
    let inline createNominal (data: seq<'a>) =
        let tmp = 
            data
            |> Seq.groupBy id
            |> Seq.map (fun (k,values) -> 
                let count = (Seq.length(values)) |> float
                k, count)
            |> Seq.sortBy fst
        tmp    
        |> Map.ofSeq
        |> normalize

    /// Creates probability mass function of the categories in the input sequence.
    /// A template defines the search space to exclude certain elements or to include elements that are not in the input sequence.
    /// Frequencies are determined based only on the template set. 
    let inline createNominalWithTemplate (template: Set<'a>) (data: seq<'a>) =
        let tmp = 
            data
            |> Seq.groupBy id
            |> Seq.map (fun (k,values) -> 
                let count = (Seq.length(values)) |> float
                k, count)
            |> Seq.sortBy fst
        let currMap = 
            tmp    
            |> Map.ofSeq
        template
        |> Seq.map (fun x -> 
            // if the input data contains the category, add the respective frequency to the key
            if currMap.ContainsKey x then x,currMap.[x] 
            // if the input data does NOT contain the category, add a zero as frequency
            else
                x,0.
            )
        |> Map.ofSeq
        |> normalize
        
    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value is determined by f with the first value being from mapA and the second originating from mapB.</summary>
    /// <param name="equalBandwidthOrNominal">Is the binwidth equal for both distributions? For nominal data set to true.</param>
    /// <param name="f">Function to transform values if key is present in both histograms. `histA-value &#8594; histB-value &#8594; newValue`</param>
    /// <param name="mapA">Empirical distribution A</param>
    /// <param name="mapB">Empirical distribution B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (mergeBy f a b) is not equal to (mergeBy f b a)</remarks> 
    /// <returns>New frequency map that results from merged maps mapA and mapB. Values from keys that are present in both maps are handled by f</returns> 
    let mergeBy equalBandwidthOrNominal (f: 'value -> 'value -> 'value) (histA: Map<_,'value>) (histB: Map<_,'value>) =
        if equalBandwidthOrNominal then
            Map.mergeBy f histA histB
        else 
            failwithf "Not implemented yet. If continuous data shall be merged, bandwidth must be equal. This does not matter for nominal data!"
            //ToDo:
            //    Dissect both distributions and construct a new one based on given bandwidths
            //    New bandwidth might be double the largest observed bandwidth to not miss-sort any data. 

    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value in histA is superseded by the value in histB.</summary>
    /// <param name="equalBandwidthOrNominal">Is the binwidth equal for both distributions? For nominal data set to true.</param>
    /// <param name="histA">Empirical distribution A</param>
    /// <param name="histB">Empirical distribution B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (merge a b) is not equal to (merge b a)</remarks> 
    /// <returns>New frequency map that results from merged maps histA and histB.</returns> 
    let merge equalBandwidthOrNominal (histA: Map<_,'value>) (histB: Map<_,'value>) =
        mergeBy equalBandwidthOrNominal (fun a b -> b) histA histB

    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value from mapB is added to the value of mapA.</summary>
    /// <param name="equalBandwidthOrNominal">Is the binwidth equal for both distributions? For nominal data set to true.</param>
    /// <param name="histA">Empirical distribution A</param>
    /// <param name="histB">Empirical distribution B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (add a b) is not equal to (add b a)</remarks> 
    /// <returns>New frequency map that results from merged maps histA and histB. Values from keys that are present in both maps are handled by f</returns> 
    let inline add equalBandwidthOrNominal (histA: Map<_,'value>) (histB: Map<_,'value>) =
        mergeBy equalBandwidthOrNominal (fun a b -> a + b) histA histB
        

type EmpiricalDistribution() =
    
    /// Creates probability mass function of the input sequence.
    /// The bandwidth defines the width of the bins the numbers are sorted into. 
    /// Bin intervals are half open excluding the upper border: [lower,upper)
    static member create(bandwidth: float) =
        fun (data: seq<float>) -> 
            Empirical.create bandwidth data
    
    ///// <summary>Merges two maps into a single map. If a key exists in both maps, the value in histA is superseded by the value in histB.</summary>
    ///// <param name="histA">Empirical distribution A</param>
    ///// <param name="histB">Empirical distribution B</param>
    ///// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    ///// <remarks>This function is not commutative! (merge a b) is not equal to (merge b a)</remarks> 
    ///// <returns>New frequency map that results from merged maps histA and histB.</returns> 
    //static member merge: ((Map<_,float> -> Map<_,float> -> Map<_,float>)) =
    //    fun histA histB -> 
    //        Empirical.merge histA histB

    ///// <summary>Merges two maps into a single map. If a key exists in both maps, the value from mapB is added to the value of mapA.</summary>
    ///// <param name="histA">Empirical distribution A</param>
    ///// <param name="histB">Empirical distribution B</param>
    ///// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    ///// <remarks>This function is not commutative! (add a b) is not equal to (add b a)</remarks> 
    ///// <returns>New frequency map that results from merged maps histA and histB. Values from keys that are present in both maps are handled by f</returns> 
    //static member add: ((Map<_,float> -> Map<_,float> -> Map<_,float>)) =
    //    fun histA histB -> 
    //        Empirical.add histA histB

    /// Creates probability mass function of the categories in the input sequence.
    /// A template defines the search space to exclude certain elements or to include elements that are not in the input sequence.
    /// If a template is defined, frequencies are determined based only on the template set. 
    /// Transform can be used to e.g. round values or manipulating characters (System.Char.toUpper)
    static member createNominal(?Template: Set<'a>,?Transform: 'a -> 'a) = 

        if Template.IsNone then 
            fun (data: seq<'a>) -> 
                if Transform.IsNone then 
                    Empirical.createNominal data
                else 
                    Empirical.createNominal (data |> Seq.map Transform.Value)
                    
        else 
            fun (data: seq<'a>) -> 
                if Transform.IsNone then    
                    Empirical.createNominalWithTemplate Template.Value data
                else 
                    Empirical.createNominalWithTemplate Template.Value (data |> Seq.map Transform.Value)