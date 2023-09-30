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

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode lambda =
        Exponential.CheckParam lambda
        0.0

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean lambda =
        Exponential.CheckParam lambda
        1.0 / lambda

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance lambda =
        Exponential.CheckParam lambda
        1.0 / (lambda * lambda)

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation lambda =
        Exponential.CheckParam lambda
        1.0 / lambda

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample lambda = 
        // Source: fsmathtools
        Exponential.CheckParam lambda
        let mutable r = Random.rndgen.NextFloat()
        while (r = 0.0) do
            r <- Random.rndgen.NextFloat()
        done;
        (- log r)/lambda
            

    /// <summary>Computes the probability density function.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member PDF lambda x = 
        Exponential.CheckParam lambda
        if x >= 0.0 then
            lambda * exp(-lambda * x)
        else 0.0
        
    /// <summary>Computes the cumulative distribution function.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF lambda x =
        Exponential.CheckParam lambda
        if x < 0.0 then 0.0
        else 1.0 - exp(-lambda * x)

    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member InvCDF lambda x =
        Exponential.CheckParam lambda
        failwithf "InvCDF not implemented yet"

    /// <summary>
    ///   Fits the underlying distribution to a given set of observations.
    /// </summary>
    static member Fit(observations:float[],?weights:float[]) =
        match weights with
        | None   -> observations |> Array.average
        | Some w -> observations |> Array.weightedMean w
        |> fun mean -> 1. / mean

    /// <summary>
    ///   Estimates a new Exponential distribution from a given set of observations.
    /// </summary>
    static member Estimate(observations:float[],?weights:float[]) =
        match weights with
        | None   -> observations |> Array.average
        | Some w -> observations |> Array.weightedMean w
        |> Exponential.Init  


    /// <summary>Returns the support of the exponential distribution: [0, Positive Infinity).</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support lambda =
        Exponential.CheckParam lambda
        Interval.CreateClosed<float> (0.0,System.Double.PositiveInfinity)


    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString lambda =
        sprintf "Exponential(λ = %f)" lambda

    /// <summary>Initializes a Exponential distribution    </summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init lambda = 
        { new ContinuousDistribution<float,float> with
            member d.Mean              = Exponential.Mean lambda
            member d.StandardDeviation = Exponential.StandardDeviation lambda   
            member d.Variance          = Exponential.Variance lambda
            member d.PDF x             = Exponential.PDF lambda x
            
            member d.Mode              = Exponential.Mode lambda
            member d.Sample ()         = Exponential.Sample lambda
            member d.Parameters        = DistributionParameters.Exponential {Lambda=lambda}
            member d.CDF x             = Exponential.CDF lambda x 
            member d.InvCDF x          = Exponential.InvCDF lambda x 
            override d.ToString()      = Exponential.ToString lambda
        }  


 