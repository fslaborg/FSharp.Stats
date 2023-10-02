namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// Student's T-distribution
// ------------------------
// wiki: "http://en.wikipedia.org/wiki/Student%27s_t-distribution"
// ######


    
/// Student's T-distribution
type StudentT =
    
    // Student's T-distribution helper functions.
    static member CheckParam mu tau dof = 
        if System.Double.IsNaN(mu) || tau < 0.0 || System.Double.IsNaN(dof)  || dof < 0. then 
            failwith "Student's T-distribution should be parametrized by mu, tau and dof > 0.0."

    /// <summary>Computes the mode.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode mu tau dof =
        StudentT.CheckParam mu tau dof
        mu
    
    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean mu tau dof =
        StudentT.CheckParam mu tau dof
        mu

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance mu tau dof =
        StudentT.CheckParam mu tau dof
        match dof with
        | df when System.Double.IsPositiveInfinity(df) -> tau*tau
        | df when df > 2.0 -> dof*tau*tau/(dof-2.0)
        | _ -> System.Double.PositiveInfinity

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation mu tau dof =
        StudentT.CheckParam mu tau dof
        sqrt (StudentT.Variance mu tau dof)
            

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample mu tau dof =
        StudentT.CheckParam mu tau dof
        let gamma = Gamma.Sample (0.5*dof) 0.5
        Normal.Sample mu (tau*sqrt(dof/gamma))
        // let gamma = 1. / Gamma.Sample (0.5*dof) (0.5*dof)
        // Normal.Sample mu (tau*sqrt(gamma))

    /// <summary>Computes the probability density function.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member PDF mu tau dof x =
        StudentT.CheckParam mu tau dof
        let d = (x - mu) / tau
        exp (SpecialFunctions.Gamma._gammaLn((dof + 1.)/2.) - SpecialFunctions.Gamma._gammaLn(dof/2.)) * System.Math.Pow(1.0 + (d*d / dof), (-0.5 * (dof + 1.))) / sqrt (dof*pi) / tau

    /// <summary>Computes the cumulative distribution function.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF mu tau dof x =
        StudentT.CheckParam mu tau dof            
        let k = (x - mu) / tau
        let h = dof / (dof + (k * k))
        let ib = 0.5 * SpecialFunctions.Beta.lowerIncompleteRegularized (dof/2.0) 0.5 h
        if x <= mu then ib else 1.0 - ib    
        
    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member InvCDF mu tau dof x =
        StudentT.CheckParam mu tau dof            
        failwithf "InvCDF not implemented yet" 

    /// <summary>Returns the support of the exponential distribution: (Negative Infinity, Positive Infinity).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support mu tau dof =
        StudentT.CheckParam mu tau dof
        Interval.CreateOpen<float>(Double.NegativeInfinity, Double.PositiveInfinity)

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString mu tau dof =
        sprintf "StudentT(μ = %f, σ = %f, dof = %f" mu tau dof



    /// <summary>Initializes a Student's T-distribution</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="tau"></param>
    /// <param name="dof"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init mu tau dof =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = StudentT.Mean mu tau dof
            member d.StandardDeviation = StudentT.StandardDeviation mu tau dof
            member d.Variance          = StudentT.Variance mu tau dof
            member d.CDF x             = StudentT.CDF mu tau dof x  
            member d.InvCDF x          = StudentT.InvCDF mu tau dof x  
            
            member d.Mode              = StudentT.Mode mu tau dof
            member d.Parameters        = DistributionParameters.StudentT {Mean=mu;StandardDeviation=tau;DOF=dof}
            member d.Sample ()         = StudentT.Sample mu tau dof
            member d.PDF x             = StudentT.PDF mu tau dof x      
            override d.ToString()      = StudentT.ToString mu tau dof
        } 

