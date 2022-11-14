namespace FSharp.Stats.Signal


open FSharp.Stats
open System
open FSharp.Stats.SpecialFunctions
open FSharp.Stats.Quantile

module QQPlot =
    
    /// computes the quantile quantile coordinates of two sample distributions. Uses default quantile (Quantile.mode)
    let fromTwoSamples sampleA sampleB =
            [0. .. 0.01 .. 1.]
            |> List.map (fun quantile -> 
                mode quantile sampleA,
                mode quantile sampleB
                )
    /// computes the quantile quantile coordinates of a sample distributions against a normal distribution. Uses default quantile (Quantile.mode)    
    let fromSampleToGauss sample =
        let standardizedData = 
            Signal.Normalization.zScoreTransformPopulation (vector sample)

        let inverseCDF x =
            sqrt 2. * Errorfunction.inverf (2. * x - 1.)

        [0. .. 0.01 .. 1.]
        |> List.map (fun quantile -> 
            inverseCDF quantile,
            Quantile.mode quantile standardizedData
            )
