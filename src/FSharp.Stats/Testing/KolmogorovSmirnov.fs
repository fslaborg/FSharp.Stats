namespace FSharp.Stats.Testing

open FSharp.Stats.Distributions

module KolmogorovSmirnov =
    
    /// Tests the input values with the Kolmogorov-Smirnov Goodness of Fit-test against a normal distribution and returns a p-Value. 
    /// The p-value indicates how likely it is to observe such a distribution under the assumption that the null hypothesis is true (= sample distribution normally distributed).  
    /// In short: The lower the p-value, the more likely it is that the sample does NOT originate from a normal distribution.
    let goF values =
        let lVals = Array.length values |> float
        let rankedVals = Array.sort values
        let rankedNormVals = rankedVals |> Array.mapi (fun i _ -> float i / lVals)
        let meanVals = FSharp.Stats.Seq.mean values
        let stdVals = FSharp.Stats.Seq.stDev values
        let nD = FSharp.Stats.Distributions.Continuous.normal meanVals stdVals
        let cum = rankedVals |> Array.map (fun v -> nD.CDF v)
        let deltaCumVals = (cum, rankedNormVals) ||> Array.map2 (fun c v -> System.Math.Abs (c - v))
        let kS = Array.max deltaCumVals
        kS
        Continuous.kolmogorov 