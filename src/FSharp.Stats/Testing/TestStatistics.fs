namespace FSharp.Stats.Testing
open System
open FSharp.Stats.GenericMath


module TestStatistics =
    
    open FSharp.Stats

    /// <summary>
    ///   Creates a new T-Test for a given statistic
    ///   with given degrees of freedom.
    /// </summary>
    /// 
    /// <param name="Statistic">The test statistic.</param>
    /// <param name="DegreesOfFreedom">The degrees of freedom for the numerator.</param>    
    /// <param name="PValueLeft">One Tailed/Sided.</param>
    /// <param name="PValueRight"> One Tailed/Sided.</param>   
    /// <param name="PValue">Two Tailed/Sided.</param>   
    type TTestStatistics<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>> = 
        {
            Statistic            : 'T
            DegreesOfFreedom     : 'T
            PValueLeft           : 'T
            PValueRight          : 'T
            PValue               : 'T            
        }

    let createTTest (statistic: 'T) (dof: 'T) =
        let cdf: 'T  = Distributions.Continuous.StudentT.CDF 0. 1. ('T.One dof) statistic
        let pvalue: 'T = if statistic > T 0. then T 1. - cdf else cdf
        
        {Statistic=statistic; DegreesOfFreedom=dof; PValueLeft=1. - pvalue; PValueRight=pvalue; PValue=pvalue*2.;}

    /// <summary>
    ///   Creates a new F-Test for a given statistic
    ///   with given degrees of freedom.
    /// </summary>
    type FTestStatistics = {
        /// <summary name="statistic">The test statistic.</summary>
        Statistic            : float
        /// <summary name="d1">The degrees of freedom for the numerator.</summary>
        DegreesOfFreedom1    : float
        /// <summary name="d2">The degrees of freedom for the denominator.</summary>
        DegreesOfFreedom2    : float
        PValue               : float 
        PValueTwoTailed      : float            
    }

    let createFTest statistic dof1 dof2 =
        let cdf  =  Distributions.Continuous.F.CDF dof1 dof2 statistic            
        let pvalue = 1. - cdf
        let pvalueTwoTailed = pvalue * 2.
        {Statistic=statistic; DegreesOfFreedom1=dof1; DegreesOfFreedom2=dof2; PValue=pvalue; PValueTwoTailed = pvalueTwoTailed}


    /// <summary>
    ///   Computes the Chi-Square test statistics for a given statistic
    ///   with given degrees of freedom.
    /// </summary>
    type ChiSquareStatistics = {
        /// <summary name="Statistic">The test statistic.</summary>
        Statistic            : float
        /// <summary name="DegreesOfFreedom">The degrees of freedom for the numerator.</summary>    
        DegreesOfFreedom     : float
        /// <summary name="PValueLeft">One Tailed/Sided.</summary>
        PValueLeft           : float
        /// <summary name="PValueRight"> One Tailed/Sided.</summary>   
        PValueRight          : float
        /// <summary name="PValue">Two Tailed/Sided.</summary>   
        PValue               : float            
    }


    let createChiSquare statistic dof =
        let cdf  = Distributions.Continuous.ChiSquared.CDF dof statistic
        let pvalue = if statistic > 0. then 1. - cdf else cdf
        {Statistic = statistic; DegreesOfFreedom = dof; PValueLeft = 1. - pvalue; PValueRight = pvalue; PValue = pvalue * 2.}

    
    /// <summary>
    ///   Computes the Wilcoxon test statistics for a given statistic.
    /// </summary>
    type WilcoxonTestStatistics = {
        /// <summary name="Statistic">The test statistic.</summary>
        Statistic            : float
        PValueLeft           : float
        PValueRight          : float 
        /// <summary name="PValueTwoTailed">Two Tailed/Sided.</summary>   
        PValueTwoTailed      : float 
    }    
    let createWilcoxon statistic =
        let cdf  =  Distributions.Continuous.Normal.CDF 0. 1.  statistic         
        let pvalue = 1.-  cdf
        let pvalueTwoTailed = pvalue * 2.
        {Statistic=statistic; PValueLeft=pvalue;PValueRight = cdf; PValueTwoTailed = pvalueTwoTailed}