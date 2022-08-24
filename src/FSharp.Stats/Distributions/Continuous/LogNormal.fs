namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// Log-Normal distribution
// ######


/// Log-Normal distribution.
type LogNormal =

    // Log-Normal distribution helper functions.
    static member CheckParam mu tau = 
        if System.Double.IsNaN(mu) || tau < 0.0 then 
            failwith "Log-Normal distribution should be parametrized by tau > 0.0."

    /// Computes the mode.
    static member Mode mu tau =
        LogNormal.CheckParam mu tau
        exp(mu - (tau*tau))
    
    /// Computes the mean.
    static member Mean mu tau =
        LogNormal.CheckParam mu tau
        exp(mu + (tau*tau/2.0))
            

    /// Computes the variance.
    static member Variance mu tau =
        LogNormal.CheckParam mu tau
        tau*tau

    /// Computes the standard deviation.
    static member StandardDeviation mu tau =
        LogNormal.CheckParam mu tau
        let tau2 =  tau * tau
        sqrt (exp(tau2) - 1.0) * exp(mu + mu + tau2)

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample mu tau =
        // Source: fsmathtools
        LogNormal.CheckParam mu tau
        let mutable v1 = 2.0 * Random.rndgen.NextFloat() - 1.0
        let mutable v2 = 2.0 * Random.rndgen.NextFloat() - 1.0
        let mutable r = v1 * v1 + v2 * v2
        while (r >= 1.0 || r = 0.0) do
            v1 <- 2.0 * Random.rndgen.NextFloat() - 1.0
            v2 <- 2.0 * Random.rndgen.NextFloat() - 1.0
            r <- v1 * v1 + v2 * v2
        let fac = sqrt(-2.0*(log r)/r)
        exp (tau * v1 * fac + mu)
        //failwith "Not implemented yet."

    /// Computes the probability density function.
    static member PDF mu tau x =
        LogNormal.CheckParam mu tau
        let a = (log x - mu) / tau
        //exp(-0.5*a*a)/(x * tau * Ops.Sqrt2Pi)
        failwith "Not implemented yet."
            

    /// Computes the cumulative distribution function.
    static member CDF mu tau x =
        LogNormal.CheckParam mu tau            
        //0.5 * (1.0 + SpecialFunctions.Errorfunction.Erf((x - mu)/(tau*(sqrt 2.0))))
        failwith "Not implemented yet."

    /// Returns the support of the exponential distribution: [0, Positive Infinity).
    static member Support mu tau =
        LogNormal.CheckParam mu tau
        (0., System.Double.PositiveInfinity)

    /// A string representation of the distribution.
    static member ToString mu tau =
        sprintf "Normal(μ = %f, σ = %f)" mu tau

    /// Initializes a Normal distribution 
    static member Init mu tau =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = LogNormal.Mean mu tau
            member d.StandardDeviation = LogNormal.StandardDeviation mu tau
            member d.Variance          = LogNormal.Variance mu tau
            member d.CDF x             = LogNormal.CDF mu tau x  
            //member d.CoVariance        = LogNormal.CoVariance  mu tau
            member d.Mode              = LogNormal.Mode mu tau
            member d.Sample ()         = LogNormal.Sample mu tau
            member d.PDF x             = LogNormal.PDF mu tau x      
            override d.ToString()      = LogNormal.ToString mu tau
        }

    /// Estimates the log-normal distribution parameters from sample data with maximum-likelihood.
    static member Estimate samples =
        let s = 
            samples
            |> Seq.map log
            |> Seq.stats
        let mu  = SummaryStats.mean s
        let tau = SummaryStats.stDev s
            
        LogNormal.Init mu tau

