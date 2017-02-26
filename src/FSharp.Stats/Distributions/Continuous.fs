namespace FSharp.Stats.Distributions

open FSharp.Stats.Ops

// Continuous probability distributions
module Continuous = 
    
    open FSharp.Stats.SpecialFunctions

    
// ######
// ChiSquared distribution
// ######


    // chisquared distribution helper functions.
    let chiSquaredCheckParam dof = if System.Double.IsNaN(dof)  || dof < 0. then failwith "ChiSquared distribution should be parametrized by degrees of Freedom in [0,inf]."
    
    /// ChiSquared distribution.
    type ChiSquared =        
        /// Computes the mean.
        static member Mean dof =
            chiSquaredCheckParam dof
            dof
        /// Computes the variance.
        static member Variance dof =
            chiSquaredCheckParam dof
            dof * 2.
        /// Computes the standard deviation.
        static member StandardDeviation dof =
            chiSquaredCheckParam dof
            sqrt (dof * 2.)
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample dof =
            chiSquaredCheckParam dof
            //rndgen.NextFloat() * (max - min) + min
            nan

        /// Computes the probability density function.
        static member PDF dof x =
            chiSquaredCheckParam dof
//            if x < 0.0 || dof < 1. then
//                0.0
//            else
//                let k = float dof * 0.5
//                let x = x * 0.5
//                if dof = 2. then
//                    exp (-1. * x)
//                else
//                    let pValue = SimpleApprox.incGamma k x // incGamma -> gamma lower incomplete
//                    if (isNan pValue) || (isInf pValue) ||  (pValue <= 1e-8) then
//                        1e-14
//                    else
//                        1.- pValue / (SimpleApprox.gamma k)  
            1.

        /// Computes the cumulative distribution function.
        static member CDF dof x =
            chiSquaredCheckParam dof
            failwith "Not implemented yet."

        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support dof =
            chiSquaredCheckParam dof
            (0., System.Double.PositiveInfinity)


    /// Initializes a uniform distribution        
    let chiSquared dof =
        { new Distribution<float,float> with
            member d.Mean              = ChiSquared.Mean dof
            member d.StandardDeviation = ChiSquared.StandardDeviation dof 
            member d.Variance          = ChiSquared.Variance dof
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = ChiSquared.Sample dof
            member d.PDF x             = ChiSquared.PDF dof x           
            member d.CDF x             = ChiSquared.CDF dof  x         
        }



// ######
// Uniform distribution
// ######


    // Uniform distribution helper functions.
    let uniformCheckParam min max = if System.Double.IsNaN(min) || System.Double.IsNaN(max) || min > max then failwith "Uniform distribution should be parametrized by min < max in [-inf,inf]."
    
    /// Uniform distribution.
    type Uniform =        
        /// Computes the mean.
        static member Mean min max =
            uniformCheckParam min max
            min + (max - min) / 2.0
        /// Computes the variance.
        static member Variance min max =
            uniformCheckParam min max
            1.0/3.0 * (max*max + max * min + min*min)
        /// Computes the standard deviation.
        static member StandardDeviation min max =
            uniformCheckParam min max
            sqrt (1.0/3.0 * (max*max + max * min + min*min))
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample min max =
            uniformCheckParam min max
            //rndgen.NextFloat() * (max - min) + min
            nan

        /// Computes the probability density function.
        static member PDF min max x =
            uniformCheckParam min max
            if x <= max && x >= min then 1.0 / (max - min) else 0.0
        /// Computes the cumulative distribution function.
        static member CDF min max x =
            uniformCheckParam min max
            if x < min then 0.0
            elif x < max then (x - min) / (max - min)
            else 1.0
        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support min max =
            uniformCheckParam min max
            (0., System.Double.PositiveInfinity)

    /// Initializes a uniform distribution        
    let uniform min max =
        { new Distribution<float,float> with
            member d.Mean              = Uniform.Mean min max
            member d.StandardDeviation = Uniform.StandardDeviation min max   
            member d.Variance          = Uniform.Variance min max
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = Uniform.Sample min max
            member d.PDF x             = Uniform.PDF min max x           
            member d.CDF x             = Uniform.CDF min max x         
        }   


// ######
// ... distribution
// ######
