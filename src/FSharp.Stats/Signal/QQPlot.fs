namespace FSharp.Stats.Signal


open FSharp.Stats
open System
open FSharp.Stats.SpecialFunctions
open FSharp.Stats.Quantile

module QQPlot =

    type QuantileMethod =
        | Blom
        | Rankit
        | Tukey
        | VanDerWerden

    /// computes the quantile quantile coordinates of two sample distributions. Uses default quantile (Quantile.mode)
    // tested against R qqnorm
    let internal fromTwoSamples method sampleA sampleB =
        let sampleALength = Seq.length sampleA
        let sampleBLength = Seq.length sampleB
        let minSampleLength = float (min sampleALength sampleBLength)

        let (smallSet,bigSet) = if sampleALength <= sampleBLength then (sampleA,sampleB) else (sampleB,sampleA)

        let getQuantile rank = 
            match method with
            | Blom          -> (rank - 3. / 8.) / (minSampleLength + 1. / 4.)
            | Rankit        -> (rank - 1. / 2.) / minSampleLength
            | Tukey         -> (rank - 1. / 3.) / (minSampleLength + 1. / 3.)
            | VanDerWerden  -> rank / (minSampleLength + 1.)

        smallSet 
        |> Seq.sort
        |> Seq.mapi (fun i x -> 
            let rank = float (i + 1)
            let pi = getQuantile rank
            x,Quantile.mode pi bigSet
            )


    /// Computes the quantile quantile coordinates of a sample distributions against a normal distribution. 
    /// The sample can be z transformed. StandardMethod = Rankit
    // tested against R qqnorm
    let internal fromSampleToGauss method zTransformSample (sample:seq<float>) =

        let sampleLength = Seq.length sample |> float

        let standardizedData = 
            if zTransformSample then 
                Signal.Normalization.zScoreTransformPopulation (vector sample)
                |> Vector.toArray
            else
                sample |> Seq.toArray

        let getQuantile rank = 
            match method with
            | Blom          -> (rank - 3. / 8.) / (sampleLength + 1. / 4.)
            | Rankit        -> (rank - 1. / 2.) / sampleLength
            | Tukey         -> (rank - 1. / 3.) / (sampleLength + 1. / 3.)
            | VanDerWerden  -> rank / (sampleLength + 1.)

        let inverseCDF x =
            sqrt 2. * Errorfunction.inverf (2. * x - 1.)

        standardizedData 
        |> Seq.sort
        |> Seq.mapi (fun i x -> 
            let rank = float (i + 1)
            let pi = getQuantile rank
            inverseCDF pi,x
            )

    /// Computes the quantile quantile coordinates of a sample distributions against a normal distribution. 
    /// The sample can be standardized between 0 and 1.
    let internal fromSampleToUniform method standardizeSample (sample:seq<float>)  =

        let sampleLength = Seq.length sample |> float

        let standardizedSample = 
            if standardizeSample then 
                let min = Seq.min sample
                let max = Seq.max sample
                sample |> Seq.map (fun x -> (x-min) / (max-min))
            else
                sample

        let getQuantile rank = 
            match method with
            | Blom          -> (rank - 3. / 8.) / (sampleLength + 1. / 4.)
            | Rankit        -> (rank - 1. / 2.) / sampleLength
            | Tukey         -> (rank - 1. / 3.) / (sampleLength + 1. / 3.)
            | VanDerWerden  -> rank / (sampleLength + 1.)

        standardizedSample
        |> Seq.sort
        |> Seq.mapi (fun i x -> 
            let rank = float (i + 1)
            let pi = getQuantile rank
            pi,x
            )


type QQPlot() =

    

    /// Computes the quantile quantile coordinates of two sample distributions. 
    /// Uses default quantile (Quantile.mode) and Rankit method
    static member fromTwoSamples(?Method:QQPlot.QuantileMethod) = 

        let method = defaultArg Method QQPlot.QuantileMethod.Rankit

        fun (sampleA: seq<float>) (sampleB: seq<float>) -> 
            QQPlot.fromTwoSamples method sampleA sampleB
                        


    /// Computes the quantile quantile coordinates of a sample distributions against a normal distribution. 
    /// The sample can be z transformed. default = Rankit
    static member fromSampleToGauss(?Method:QQPlot.QuantileMethod,?ZTransform:bool) = 

        let standardize = defaultArg ZTransform false
        let method = defaultArg Method QQPlot.QuantileMethod.Rankit

        fun (sample: seq<float>) -> 
            QQPlot.fromSampleToGauss method standardize sample
                        



    /// Computes the quantile quantile coordinates of a sample distributions against a normal distribution. 
    /// The sample can be standardized to the range between 0 and 1. default = Rankit
    static member fromSampleToUniform(?Method:QQPlot.QuantileMethod,?Standardize:bool) = 

        let standardize = defaultArg Standardize false
        let method = defaultArg Method QQPlot.QuantileMethod.Rankit

        fun (sample: seq<float>) -> 
            QQPlot.fromSampleToUniform method standardize sample
                        






