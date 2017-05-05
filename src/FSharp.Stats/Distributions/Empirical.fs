namespace FSharp.Stats.Distributions

/// Represents a probability mass function (map from values to probabilities).
module Empirical =

    ///// probability mass function    
    //let create bandwidth data =        
    //    let n = float (Seq.length(data))
    //    data
    //    |> Seq.groupBy (fun x -> floor (x / bandwidth)) 
    //    |> Seq.map (fun (k,values) -> let mean = values |> StatisticalMeasure.mean
    //                                  let count = float (Seq.length(values))
    //                                  (mean,count / n) )  
    //    |> Map.ofSeq



    /// Creates Pmf of a Histogram (normalize by n)
    let ofHistogram (hist:Map<_,int>) =
        let n = float (hist |> Map.fold (fun state key value -> state + value) 0)
        hist |> Seq.map (fun kv -> (kv.Key,(float kv.Value) / n )) |> Map.ofSeq

    /// Returns: tuple of (sorted value sequence, probability sequence)
    let getZip (pmf:Map<float,float>) =
        pmf |> Seq.sortBy (fun kv -> kv.Key) |> Seq.map (fun kv -> (kv.Key,kv.Value))

    /// Returns the total of the probabilities in the map
    let sum (pmf:Map<float,float>) =
        pmf |> Seq.sumBy (fun kv -> kv.Value)
        
    /// Returns the largest probability in the map.
    let maxLike (pmf:Map<float,float>) =
        (pmf |> Seq.maxBy (fun kv -> kv.Value)).Value





    
    /// Returns distinct values from pmf
    let getXValues (pmf:Map<float,float>) =
        pmf |> Seq.map (fun k -> k.Key)

    ///// Returns values from pmf 
    ///// Attention: original values have been discretised
    //let getValues (pmf:Map<float,float>) =
    //    printfn "Attention: original values have been discretised"
    //    pmf |> Seq.map (fun k -> Seq.initRepeatValue (int k.Value) k.Key) |> Seq.concat


    /// Returns distinct values from pmf
    let getYValues (pmf:Map<float,float>) =
        pmf |> Seq.map (fun k -> k.Value)

    

    
    /// Gets the probability associated with the value x
    let probabilityAt  (pmf:Map<float,float>) (x:float) =        
        if pmf.ContainsKey(x) then
            pmf.[x]
        else
            0.            
    
    /// Gets an unsorted sequence of probabilities
    let probabilities  (pmf:Map<float,float>) =         
        pmf |> Seq.map (fun k -> k.Value)


    ///// Subtracts the values pmfA from pmfB
    //let subtract (pmfA:Map<float,float>) (pmfB:Map<float,float>) =
    //    Map.merge pmfA pmfB (fun k (v, v') -> v - v')

    ///// Adds the values in pmfA to pmfB
    //let add (pmfA:Map<float,float>) (pmfB:Map<float,float>) =
    //    Map.merge pmfA pmfB (fun k (v, v') -> v + v')

    
    /// Normalizes this PMF so the sum of all probabilities equals fraction
    let normalize (fraction:float) (pmf:Map<float,float>) =
        let total = sum pmf
        let factor = if total <> 0. then (fraction / total) else raise (System.Exception("total probability is zero") )  
        pmf |> Seq.map (fun kv -> (kv.Key,kv.Value * factor)) |> Map.ofSeq


    /// Chooses a random element from this PMF
    let random (pmf:Map<float,float>) = 
        if pmf.Count <= 0 then raise (System.Exception("Pmf contains no values") )  
        let target = System.Random().NextDouble()
        //pmf |> Seq.map (fun kv -> (kv.Key,kv.Value)) |> Seq.scan (fun state (k,v) -> (k, v + snd state)) (0.,0.)
        let x,y =
            pmf
            |> Seq.scan (fun state kv -> (kv.Key, kv.Value + snd state)) (0.,0.)
            |> Seq.find (fun (x,y) -> y >= target)
        x
        

    
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
    let log (pmf:Map<float,float>) =
        let m = maxLike pmf
        pmf |> Seq.map (fun kv -> (kv.Key, log(kv.Value / m))) |> Map.ofSeq
         
    /// Exponentiates the probabilities
    let exp (pmf:Map<float,float>) =
        let m = maxLike pmf
        pmf |> Seq.map (fun kv -> (kv.Key, exp(kv.Value - m))) |> Map.ofSeq


    ///// Make a mixture distribution
    //let makeMixture (pmfs:seq<Map<float,float>>) = 
    //    pmfs |> Seq.fold (fun state elem -> Map.merge state elem (fun k (v, v') -> v * v')) Map.empty


    /// Creates probability mass function    
    let create bandwidth data =            
        let halfBw = bandwidth / 2.0       
        data
        |> Seq.groupBy (fun x -> floor (x / bandwidth)) 
        |> Seq.map (fun (k,values) -> 
            let count = (Seq.length(values)) |> float                                        
            if k < 0. then
                ((k  * bandwidth) + halfBw, count)   
            else
                ((k + 1.) * bandwidth) - halfBw, count)  
        |> Map.ofSeq
        |> normalize 1.
