﻿namespace FSharp.Stats.Distributions.Continuous

// Source: FSharp.MathTools
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops
open FSharp.Stats.SpecialFunctions

// ######
// ChiSquared distribution
// ######


/// ChiSquared distribution.
type ChiSquared =        

    // ChiSquared distribution helper functions.
    static member CheckParam dof = 
        if System.Double.IsNaN(dof)  || dof < 0. then 
            failwith "ChiSquared distribution should be parametrized by degrees of Freedom in [0,inf]."

    /// Computes the mean.
    static member Mean dof =
        ChiSquared.CheckParam dof
        dof
    /// Computes the variance.
    static member Variance dof =
        ChiSquared.CheckParam dof
        dof * 2.
    /// Computes the standard deviation.
    static member StandardDeviation dof =
        ChiSquared.CheckParam dof
        sqrt (dof * 2.)
    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample dof =
        ChiSquared.CheckParam dof
        //rndgen.NextFloat() * (max - min) + min
        raise (NotImplementedException())

    /// Computes the probability density function.
    static member PDF dof x =
        ChiSquared.CheckParam dof
        if x < 0.0 || dof < 1. then
            0.0
        else
            let gammaF = Gamma._gamma (dof/2.)
            let k = 2.**(dof/2.)
            let fraction = (1./((k)*gammaF))
            let ex1 = (x**((dof/2.)-1.))
            let ex2 = exp(-x/2.)
            let pdffunction = fraction*(ex1*ex2)
            pdffunction 

    // TO DO: unlear function. Commented out until known. (see https://github.com/fslaborg/FSharp.Stats/issues/209)

    ///// Computes the logarithm of probability density function.
    //static member PDFLn dof x = 
    //    if System.Double.IsPositiveInfinity(dof) || System.Double.IsPositiveInfinity(x) || x=0. then
    //        System.Double.NegativeInfinity
    //    else
    //        ((1.0 - (dof/2.0))*System.Math.Log(2.0)) + ((dof - 1.0)*System.Math.Log(x)) - (x*x/2.0) - Gamma.gammaLn(dof/2.0)

    /// Computes the cumulative distribution function.
    static member CDF dof x =
        ChiSquared.CheckParam dof
        if dof = 0. then 
            if x > 0. then 1.
            else 0.
        elif isNan x then nan
        else Gamma.lowerIncomplete (dof/2.) (x/2.)

    /// Returns the support of the exponential distribution: [0, Positive Infinity).
    static member Support dof =
        ChiSquared.CheckParam dof
        (0., System.Double.PositiveInfinity)

    /// Initializes a ChiSquared distribution 
    static member Init dof =
        { new Distribution<float,float> with
            member d.Mean              = ChiSquared.Mean dof
            member d.StandardDeviation = ChiSquared.StandardDeviation dof 
            member d.Variance          = ChiSquared.Variance dof
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = ChiSquared.Sample dof
            member d.PDF x             = ChiSquared.PDF dof x           
            member d.CDF x             = ChiSquared.CDF dof  x         
        }  

    