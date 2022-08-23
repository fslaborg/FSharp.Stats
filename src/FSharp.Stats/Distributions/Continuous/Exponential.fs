namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// Exponential distribution
// ######


    
/// Exponential distribution.
type Exponential =

    // Exponential distribution helper functions.
    static member CheckParam lambda = 
        if lambda <= 0.0 then 
            failwith "Exponential distribution should be parametrized by lambda > 0.0."


    /// Computes the mean.
    static member Mean lambda =
        Exponential.CheckParam lambda
        1.0 / lambda

    /// Computes the variance.
    static member Variance lambda =
        Exponential.CheckParam lambda
        1.0 / (lambda * lambda)

    /// Computes the standard deviation.
    static member StandardDeviation lambda =
        Exponential.CheckParam lambda
        1.0 / lambda

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample lambda = 
        // Source: fsmathtools
        Exponential.CheckParam lambda
        let mutable r = Random.rndgen.NextFloat()
        while (r = 0.0) do
            r <- Random.rndgen.NextFloat()
        done;
        (- log r)/lambda
            

    /// Computes the probability density function.
    static member PDF lambda x = 
        Exponential.CheckParam lambda
        if x >= 0.0 then
            lambda * exp(-lambda * x)
        else 0.0

    /// Computes the cumulative distribution function.
    static member CDF lambda x =
        Exponential.CheckParam lambda
        if x < 0.0 then 0.0
        else 1.0 - exp(-lambda * x)

    /// Returns the support of the exponential distribution: [0, Positive Infinity).
    static member Support lambda =
        Exponential.CheckParam lambda
        (0.0, System.Double.PositiveInfinity)

    /// Initializes a Exponential distribution    
    static member Init lambda = 
        { new Distribution<float,float> with
            member d.Mean              = Exponential.Mean lambda
            member d.StandardDeviation = Exponential.StandardDeviation lambda   
            member d.Variance          = Exponential.Variance lambda
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = Exponential.Sample lambda
            member d.PDF x             = Exponential.PDF lambda x           
            member d.CDF x             = Exponential.CDF lambda x         
        }  


 