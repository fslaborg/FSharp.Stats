namespace FSharp.Stats.Distributions.Continuous

// Source: FSharp.MathTools
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// (Gaussian)- Normal distribution
// ######


    
/// Normal distribution.
type Normal =

    // Normal distribution helper functions.
    static member CheckParam mu sigma = 
        if System.Double.IsNaN(mu) || sigma < 0.0 then 
            failwith "Normal distribution should be parametrized by sigma > 0.0."

    /// Computes the mode.
    static member Mode mu sigma =
        Normal.CheckParam mu sigma
        mu

    /// Computes the mean.
    static member Mean mu sigma =
        Normal.CheckParam mu sigma
        mu

    /// Computes the variance.
    static member Variance mu sigma =
        Normal.CheckParam mu sigma
        sigma*sigma

    /// Computes the standard deviation.
    static member StandardDeviation mu sigma =
        Normal.CheckParam mu sigma
        sigma

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member SampleUnchecked mu sigma =
        // Source: fsmathtools
        let mutable v1 = 2.0 * Random.rndgen.NextFloat() - 1.0
        let mutable v2 = 2.0 * Random.rndgen.NextFloat() - 1.0
        let mutable r = v1 * v1 + v2 * v2
        while (r >= 1.0 || r = 0.0) do
            v1 <- 2.0 * Random.rndgen.NextFloat() - 1.0
            v2 <- 2.0 * Random.rndgen.NextFloat() - 1.0
            r <- v1 * v1 + v2 * v2
        let fac = sqrt(-2.0*(log r)/r)
        (sigma * v1 * fac + mu)
        //failwith "Not implemented yet."
    
    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample mu sigma =
        // Source: fsmathtools
        Normal.CheckParam mu sigma
        Normal.SampleUnchecked mu sigma
        
    /// Computes the probability density function.
    static member PDF mu sigma x =
        Normal.CheckParam mu sigma
        (exp (-0.5 * (x-mu)*(x-mu) / (sigma*sigma))) / (sqrt (2.0 * Ops.pi * (sigma*sigma)))

    /// Computes the cumulative distribution function.
    static member CDF mu sigma x =
        Normal.CheckParam mu sigma            
        0.5 * (1.0 + SpecialFunctions.Errorfunction.Erf((x - mu)/(sigma*(sqrt 2.0))))

    /// Returns the support of the exponential distribution: [0, Positive Infinity).
    static member Support mu sigma =
        Normal.CheckParam mu sigma
        (System.Double.NegativeInfinity, System.Double.PositiveInfinity)


    /// A string representation of the distribution.
    static member ToString mu sigma =
        sprintf "Normal(μ = %f, σ = %f)" mu sigma

    /// Initializes a Normal distribution        
    static member Init mu sigma =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = Normal.Mean mu sigma
            member d.StandardDeviation = Normal.StandardDeviation mu sigma
            member d.Variance          = Normal.Variance mu sigma
            member d.CDF x             = Normal.CDF mu sigma x

            member d.Mode              = Normal.Mode mu sigma         
            member d.Sample ()         = Normal.Sample mu sigma
            member d.PDF x             = Normal.PDF mu sigma x      
            override d.ToString()      = Normal.ToString mu sigma
        }

    /// Estimates the Normal distribution parameters from sample data with maximum-likelihood.
    static member Estimate samples =
        let s   = Seq.stats samples
        let mu  = SummaryStats.mean s
        let sigma = SummaryStats.stDev s
            
        Normal.Init mu sigma
