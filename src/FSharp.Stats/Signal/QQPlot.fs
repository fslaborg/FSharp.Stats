namespace FSharp.Stats.Signal


open FSharp.Stats
open System
open FSharp.Stats.SpecialFunctions
open FSharp.Stats.Quantile
open FSharp.Stats.Interpolation

module QQPlot =

    type QuantileMethod =
        | Blom
        | Rankit
        | Tukey
        | VanDerWerden

    /// computes the quantile quantile coordinates of two sample distributions. Uses default quantile (Quantile.mode)
    // tested against R qqnorm
    //let internal fromTwoSamples method sampleA sampleB =
    //    let sampleALength = Seq.length sampleA
    //    let sampleBLength = Seq.length sampleB
    //    let minSampleLength = float (min sampleALength sampleBLength)
    //
    //    let (smallSet,bigSet) = if sampleALength <= sampleBLength then (sampleA,sampleB) else (sampleB,sampleA)
    //
    //    let getQuantile rank = 
    //        match method with
    //        | Blom          -> (rank - 3. / 8.) / (minSampleLength + 1. / 4.)
    //        | Rankit        -> (rank - 1. / 2.) / minSampleLength
    //        | Tukey         -> (rank - 1. / 3.) / (minSampleLength + 1. / 3.)
    //        | VanDerWerden  -> rank / (minSampleLength + 1.)
    //
    //    smallSet 
    //    |> Seq.sort
    //    |> Seq.mapi (fun i x -> 
    //        let rank = float (i + 1)
    //        let pi = getQuantile rank
    //        x,Quantile.mode pi bigSet
    //        )
    
    /// Computes the quantile quantile coordinates of two sample distributions.
    /// If samples are not the same size, the larger samples is interpolated to match the quantiles of the smaller.
    // tested against R qqplot
    let fromTwoSamples sampleA sampleB = 
        let sampleALength = Seq.length sampleA
        let sampleBLength = Seq.length sampleB

        let ((sN,smallSet),(bN,bigSet)) = 
            if sampleALength <= sampleBLength then 
                ((sampleALength,Seq.sort sampleA),(sampleBLength,Seq.sort sampleB)) 
            else ((sampleBLength,Seq.sort sampleB),(sampleALength,Seq.sort sampleA))

        let linearSpl = LinearSpline.initInterpolate [|0. .. float bN - 1.|] (Array.ofSeq bigSet)
        let stepwidth = float (bN-1) / float (sN-1)
        
        let approxbigSet = 
            Array.init sN (fun i -> 
                let xV = (float i) * stepwidth
                let yV = LinearSpline.interpolate linearSpl xV
                yV
            )
        
        if sampleALength <= sampleBLength then 
            Seq.zip smallSet approxbigSet
        else 
            Seq.zip approxbigSet smallSet


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

    /// Computes the quantile quantile coordinates of a sample distributions against a specified inverseCDF function. You can derive an inverse CDF of any statistical distribution.
    let internal fromSampleToInverseCDF method invCDF (sample:seq<float>)  =

        let sampleLength = Seq.length sample |> float

        let getQuantile rank = 
            match method with
            | Blom          -> (rank - 3. / 8.) / (sampleLength + 1. / 4.)
            | Rankit        -> (rank - 1. / 2.) / sampleLength
            | Tukey         -> (rank - 1. / 3.) / (sampleLength + 1. / 3.)
            | VanDerWerden  -> rank / (sampleLength + 1.)

        sample 
        |> Seq.sort
        |> Seq.mapi (fun i x -> 
            let rank = float (i + 1)
            let pi = getQuantile rank
            invCDF pi,x
            )


type QQPlot() =

    /// Computes the quantile quantile coordinates of a sample distributions against a normal distribution. 
    /// The sample can be z transformed. default = Rankit
    static member toGauss(?Method:QQPlot.QuantileMethod,?ZTransform:bool) = 

        let standardize = defaultArg ZTransform false
        let method = defaultArg Method QQPlot.QuantileMethod.Rankit

        fun (sample: seq<float>) -> 
            QQPlot.fromSampleToGauss method standardize sample

    /// Computes the quantile quantile coordinates of a sample distributions against a normal distribution. 
    /// The sample can be standardized to the range between 0 and 1. default = Rankit
    static member toUniform(?Method:QQPlot.QuantileMethod,?Standardize:bool) = 

        let standardize = defaultArg Standardize false
        let method = defaultArg Method QQPlot.QuantileMethod.Rankit

        fun (sample: seq<float>) -> 
            QQPlot.fromSampleToUniform method standardize sample
                        
    /// Computes the quantile quantile coordinates of a sample distributions against a specified inverseCDF function. You can derive an inverse CDF of any statistical distribution. 
    static member toInvCDF(inverseCDF:(float -> float),?Method:QQPlot.QuantileMethod) = 

        let method = defaultArg Method QQPlot.QuantileMethod.Rankit

        fun (sample: seq<float>) -> 
            QQPlot.fromSampleToInverseCDF method inverseCDF sample






