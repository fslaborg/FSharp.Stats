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
   
    /// Computes the mode.
    static member Mode min max =
        Uniform.Mean min max   

    /// Computes the mean.
    static member Mean min max =
        Uniform.CheckParam min max
        min + (max - min) / 2.0

    /// Computes the variance.
    static member Variance min max =
        Uniform.CheckParam min max
        1.0/3.0 * (max*max + max * min + min*min)

    /// Computes the standard deviation.
    static member StandardDeviation min max =
        Uniform.CheckParam min max
        sqrt (1.0/3.0 * (max*max + max * min + min*min))

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample min max =
        // Source: fsmathtools
        Uniform.CheckParam min max            
        Random.rndgen.NextFloat() * (max - min) + min
            

    /// Computes the probability density function.
    static member PDF min max x =
        Uniform.CheckParam min max
        if x <= max && x >= min then 1.0 / (max - min) else 0.0

    /// Computes the cumulative distribution function.
    static member CDF min max x =
        Uniform.CheckParam min max
        if x < min then 0.0
        elif x < max then (x - min) / (max - min)
        else 1.0
    
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

    /// Returns the support of the exponential distribution: [0, Positive Infinity).
    static member Support min max =
        Uniform.CheckParam min max
        Intervals.create min max


    /// A string representation of the distribution.
    static member ToString min max =
        sprintf "Uniform(Lower = %f, Upper = %f)" min max

    /// Initializes a uniform distribution
    static member Init min max =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = Uniform.Mean min max
            member d.StandardDeviation = Uniform.StandardDeviation min max   
            member d.Variance          = Uniform.Variance min max
            member d.CDF x             = Uniform.CDF min max x 
            
            member d.Mode              = Uniform.Mode min max
            member d.Sample ()         = Uniform.Sample min max
            member d.PDF x             = Uniform.PDF min max x           
            override d.ToString()      = Uniform.ToString min max        
        }   

