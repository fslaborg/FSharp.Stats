namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// Uniform distribution
// ######



/// Uniform distribution.
type Uniform =        

    // Uniform distribution helper functions.    
    static member CheckParam min max = 
        if System.Double.IsNaN(min) || System.Double.IsNaN(max) || min > max then 
            failwith "Uniform distribution should be parametrized by min < max in [-inf,inf]."    
   
    /// <summary>Computes the mode.</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode min max =
        Uniform.Mean min max   

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean min max =
        Uniform.CheckParam min max
        min + (max - min) / 2.0

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance min max =
        Uniform.CheckParam min max
        1.0/3.0 * (max*max + max * min + min*min)

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation min max =
        Uniform.CheckParam min max
        sqrt (1.0/3.0 * (max*max + max * min + min*min))

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample min max =
        // Source: fsmathtools
        Uniform.CheckParam min max            
        Random.rndgen.NextFloat() * (max - min) + min
            

    /// <summary>Computes the probability density function.</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member PDF min max x =
        Uniform.CheckParam min max
        if x <= max && x >= min then 1.0 / (max - min) else 0.0

    /// <summary>Computes the cumulative distribution function.</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF min max x =
        Uniform.CheckParam min max
        if x < min then 0.0
        elif x < max then (x - min) / (max - min)
        else 1.0

    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member InvCDF min max x =
        Uniform.CheckParam min max
        failwithf "InvCDF not implemented yet"
    
    /// <summary>
    ///   Fits the underlying distribution to a given set of observations.
    /// </summary>
    static member Fit(observations:float[]) =
        Array.min observations, Array.max observations 

    /// <summary>
    ///   Estimates a new Uniform distribution from a given set of observations.
    /// </summary>
    static member Estimate(observations:float[]) =
        Uniform.Fit observations
        |> fun (_min,_max) -> Uniform.Init _min _max  

    /// <summary>Returns the support of the exponential distribution: [0, Positive Infinity).</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support min max =
        Uniform.CheckParam min max
        Interval.CreateClosed<float> (min,max)

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString min max =
        sprintf "Uniform(Lower = %f, Upper = %f)" min max

    /// <summary>Initializes a uniform distribution</summary>
    /// <remarks></remarks>
    /// <param name="min"></param>
    /// <param name="max"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init min max =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = Uniform.Mean min max
            member d.StandardDeviation = Uniform.StandardDeviation min max   
            member d.Variance          = Uniform.Variance min max
            member d.CDF x             = Uniform.CDF min max x 
            member d.InvCDF x          = Uniform.InvCDF min max x 
            
            member d.Mode              = Uniform.Mode min max
            member d.Sample ()         = Uniform.Sample min max
            member d.PDF x             = Uniform.PDF min max x           
            member d.Parameters        = DistributionParameters.Uniform {Min=min;Max=max}
            override d.ToString()      = Uniform.ToString min max        
        }   

