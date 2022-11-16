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

    let fromTwoSamples' sampleA sampleB =
        1
        
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

    /// Computes the quantile quantile coordinates of a sample distributions against a normal distribution. 
    /// The sample is z transformed and for every data point the corresponding gauss-quantile is determined.
    let fromSampleToGauss' sample =

        let sampleLength = Seq.length sample

        let standardizedData = 
            Signal.Normalization.zScoreTransformPopulation (vector sample)
            |> Vector.toArray

        let inverseCDF x =
            sqrt 2. * Errorfunction.inverf (2. * x - 1.)

        standardizedData
        |> Seq.mapi (fun i zScore -> 
            let quantile = float (i + 1) / float sampleLength
            let quantileNormal = inverseCDF quantile
            quantileNormal,zScore
            )

    /// Computes the quantile quantile coordinates of a sample distributions against a normal distribution. 
    /// The sample is standardized between 0 and 1 and for every data point the corresponding quantile is determined.
    let fromSampleToUniform sample =

        let sampleLength = Seq.length sample
        let interval = Intervals.ofSeq sample

        let standardizedData = 
            sample
            |> Seq.map (fun x -> 
                (x - Intervals.getEnd interval) - Intervals.getSize interval
                )

        standardizedData
        |> Seq.mapi (fun i zScore -> 
            let quantileUniform = float (i + 1) / float sampleLength
            quantileUniform,zScore
            )