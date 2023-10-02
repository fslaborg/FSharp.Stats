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
    static member CheckParam mu sigma = 
        if System.Double.IsNaN(mu) || sigma < 0.0 then 
            failwith "Log-Normal distribution should be parametrized by tau > 0.0."

    /// <summary>Computes the mode.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode mu sigma =
        LogNormal.CheckParam mu sigma
        exp(mu - (sigma*sigma))
    
    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean mu sigma =
        LogNormal.CheckParam mu sigma
        exp(mu + (sigma*sigma/2.0))

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance mu sigma =
        LogNormal.CheckParam mu sigma
        (exp(sigma * sigma) - 1.) * (exp(2. * mu + sigma * sigma))

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation mu sigma =
        LogNormal.CheckParam mu sigma
        let tau2 =  sigma * sigma
        sqrt (exp(tau2) - 1.0) * exp(mu + mu + tau2)

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample mu sigma =
        // Source: fsmathtools
        LogNormal.CheckParam mu sigma
        let mutable v1 = 2.0 * Random.rndgen.NextFloat() - 1.0
        let mutable v2 = 2.0 * Random.rndgen.NextFloat() - 1.0
        let mutable r = v1 * v1 + v2 * v2
        while (r >= 1.0 || r = 0.0) do
            v1 <- 2.0 * Random.rndgen.NextFloat() - 1.0
            v2 <- 2.0 * Random.rndgen.NextFloat() - 1.0
            r <- v1 * v1 + v2 * v2
        let fac = sqrt(-2.0*(log r)/r)
        exp (sigma * v1 * fac + mu)
        //failwith "Not implemented yet."

    /// <summary>Computes the probability density function.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member PDF mu sigma x =
        LogNormal.CheckParam mu sigma
        if x <= 0. then failwithf "x must by > 0."
        let a = 1. / (x * sigma * sqrt (2. * Math.PI))
        let b = - ((log x - mu)**2.) / (2. * sigma * sigma)
        a * exp b
        
    /// <summary>Computes the cumulative distribution function.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF mu sigma x =
        LogNormal.CheckParam mu sigma            
        0.5 * (1.0 + SpecialFunctions.Errorfunction.Erf((log x - mu)/(sigma*(sqrt 2.0))))

    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member InvCDF mu sigma x =
        LogNormal.CheckParam mu sigma            
        Math.Exp (Normal.InvCDF mu sigma x)
 
    /// <summary>Returns the support of the log normal distribution: [0, Positive Infinity).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support mu sigma =
        LogNormal.CheckParam mu sigma
        Interval.CreateOpen<float>(0., Double.PositiveInfinity)

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString mu sigma =
        sprintf "LogNormal(μ = %f, σ = %f)" mu sigma

    /// <summary>Initializes a Normal distribution </summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init mu sigma =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = LogNormal.Mean mu sigma
            member d.StandardDeviation = LogNormal.StandardDeviation mu sigma
            member d.Variance          = LogNormal.Variance mu sigma
            member d.CDF x             = LogNormal.CDF mu sigma x  
            member d.InvCDF x          = LogNormal.InvCDF mu sigma x  
            //member d.CoVariance        = LogNormal.CoVariance  mu tau
            member d.Mode              = LogNormal.Mode mu sigma
            member d.Parameters        = DistributionParameters.LogNormal {Mean=mu;StandardDeviation=sigma}
            member d.Sample ()         = LogNormal.Sample mu sigma
            member d.PDF x             = LogNormal.PDF mu sigma x      
            override d.ToString()      = LogNormal.ToString mu sigma
        }

    /// <summary>Estimates the log-normal distribution parameters from sample data with maximum-likelihood.</summary>
    /// <remarks></remarks>
    /// <param name="samples"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Estimate samples =
        let s = 
            samples
            |> Seq.map log
            |> Seq.stats
        let mu  = SummaryStats.mean s
        //n-1 is more stable
        let sigma = SummaryStats.stDevPopulation s
            
        LogNormal.Init mu sigma

