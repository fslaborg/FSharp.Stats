namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// Beta distribution
// ######


/// Beta distribution
type Beta =
    // Beta distribution helper functions.
    static member CheckParam alpha beta = 
        if alpha <= 0.0 || beta <= 0.0 then 
            failwith "Beta distribution should be parametrized by alpha > 0.0, beta > 0.0."    

    /// Computes the mode.
    static member Mode alpha beta =
        Beta.CheckParam alpha beta
        match alpha,beta with
        | 0.,0. -> 0.5
        | 0.,_ -> 0.    
        | _,0. -> 1.

        | a,b when Double.IsPositiveInfinity(a) && Double.IsPositiveInfinity(b) -> 0.5
        | a,_ when Double.IsPositiveInfinity(a) -> 1.
        | _,b when Double.IsPositiveInfinity(b) -> 0.

        | 1.,1. -> 0.
        | _ -> (alpha - 1.)/(alpha + beta - 2.)

    /// Computes the mean.
    static member Mean alpha beta =
        Beta.CheckParam alpha beta
        alpha / (alpha + beta)

    /// Computes the variance.
    static member Variance alpha beta =
        Beta.CheckParam alpha beta
        (alpha * beta) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0))

    /// Computes the standard deviation.
    static member StandardDeviation alpha beta =
        Beta.CheckParam alpha beta
        sqrt ((alpha * beta) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0)))

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample alpha beta = 
        // Source: fsmathtools
        Beta.CheckParam alpha beta
        let x = Gamma.Sample alpha 1.0
        let y = Gamma.Sample beta 1.0
        x / (x + y)

    /// Computes the probability density function.
    static member PDF alpha beta x = 
        Beta.CheckParam alpha beta
        if x >= 0.0 && x <= 1.0 then
            (x ** (alpha - 1.0)) * ((1.0 - x) ** (beta - 1.0)) / (SpecialFunctions.Beta._beta alpha beta)
        else 0.0          

    /// Computes the cumulative distribution function.
    static member CDF alpha beta x =
        Beta.CheckParam alpha beta
        if x < 0.0 then 0.0
        elif x > 1.0 then 1.0
        else failwith "Not implemented yet."

    /// Returns the support of the exponential distribution: [0.0, 1.0).
    static member Support alpha beta =
        Beta.CheckParam alpha beta
        (0.0, 1.0)
    
    /// A string representation of the distribution.
    static member ToString alpha beta =
        sprintf "Beta(α = %f, β = %f)" alpha beta

    /// Initializes a Beta distribution
    static member Init alpha beta =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = Beta.Mean alpha beta
            member d.StandardDeviation = Beta.StandardDeviation alpha beta   
            member d.Variance          = Beta.Variance alpha beta
            member d.CDF x             = Beta.CDF alpha beta x         
            //member d.CoVariance        = Beta.CoVariance alpha beta 
            member d.Mode              = Normal.Mode alpha beta
            member d.Sample ()         = Beta.Sample alpha beta
            member d.PDF x             = Beta.PDF alpha beta x
            override d.ToString()      = Normal.ToString alpha beta 
        }
