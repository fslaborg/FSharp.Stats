namespace FSharp.Stats.Distributions.Continuous

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

    /// <summary>Computes the mode.</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode dof =
        ChiSquared.CheckParam dof
        max (dof - 2.)  0.

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean dof =
        ChiSquared.CheckParam dof
        dof

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance dof =
        ChiSquared.CheckParam dof
        dof * 2.
    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation dof =
        ChiSquared.CheckParam dof
        sqrt (dof * 2.)

    /// <param name="dof">Degrees of freedom (must be positive).</param>
    /// <returns>A sample from χ²(dof), interpreted as Gamma(dof/2, 2).</returns>
    static member SampleUnchecked (dof: float) : float =
        let alpha = dof / 2.0
        let beta  = 1. / 2.0
        Gamma.SampleUnchecked alpha beta


    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample dof =
        ChiSquared.CheckParam dof
        ChiSquared.SampleUnchecked dof

    /// <summary>Computes the probability density function.</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
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
    
    /// <summary>Computes the cumulative distribution function.</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF dof x =
        ChiSquared.CheckParam dof
        if dof = 0. then 
            if x > 0. then 1.
            else 0.
        elif isNan x then nan
        else Gamma.lowerIncompleteRegularized (dof/2.) (x/2.)

    /// <summary>Computes the inverse CDF (quantile function).</summary>
    /// <param name="dof">Degrees of freedom.</param>
    /// <param name="p">The probability value in [0.0, 1.0].</param>
    /// <returns>The quantile corresponding to the cumulative probability p.</returns>
    static member InvCDF (dof: float) (p: float) : float =
        let alpha = dof / 2.0
        let beta  = 1. / 2.0
        Gamma.InvCDF alpha beta p

    /// <summary>Returns the support of the exponential distribution: [0, Positive Infinity).</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support dof =
        ChiSquared.CheckParam dof
        Interval.CreateClosed<float> (0.,System.Double.PositiveInfinity)


    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString dof =
        sprintf "ChiSquared(dof = %f)" dof


    /// <summary>Fits a Chi-squared distribution by method-of-moments from raw observations.</summary>
    /// <param name="observations">An array of observed values.</param>
    /// <param name="weights">Optional weights.</param>
    /// <returns>Estimated degrees of freedom k.</returns>
    static member inline Fit(observations: 'T[], ?weights: 'T[]) : 'T =
        let mean =
            match weights with
            | None   -> Array.average observations
            | Some w -> Array.weightedMean w observations
        // Mean = k ⇒ k = mean
        mean

    /// <summary>Estimates the Chi-squared distribution from data.</summary>
    /// <param name="observations">An array of observed values.</param>
    /// <param name="weights">Optional weights.</param>
    /// <returns>An initialized Chi-squared distribution with estimated k.</returns>
    static member inline Estimate(observations: 'T[], ?weights: 'T[]) =
        let k = ChiSquared.Fit(observations, ?weights = weights)
        ChiSquared.Init k

    /// <summary>Initializes a ChiSquared distribution </summary>
    /// <remarks></remarks>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init dof =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = ChiSquared.Mean dof
            member d.StandardDeviation = ChiSquared.StandardDeviation dof 
            member d.Variance          = ChiSquared.Variance dof
            member d.CDF x             = ChiSquared.CDF dof  x 
            member d.InvCDF x          = ChiSquared.InvCDF dof  x 
            
            member d.Mode              = ChiSquared.Mode dof
            member d.Parameters        = DistributionParameters.ChiSquared {DOF=dof}
            member d.Sample ()         = ChiSquared.Sample dof
            member d.PDF x             = ChiSquared.PDF dof x           
            override d.ToString()      = ChiSquared.ToString dof        
        }  

    