namespace FSharp.Stats.Testing


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
    type TTestStatistics = {
        Statistic            : float
        DegreesOfFreedom     : float
        PValueLeft           : float
        PValueRight          : float
        PValue               : float            
    }

    let createTTest statistic dof =
        let cdf  = Distributions.Continuous.StudentT.CDF 0. 1. dof statistic
        let pvalue = if statistic > 0. then 1. - cdf else cdf
        {Statistic=statistic; DegreesOfFreedom=dof; PValueLeft=1. - pvalue; PValueRight=pvalue; PValue=pvalue*2.;}


    /// <summary>
    ///   Creates a new F-Test for a given statistic
    ///   with given degrees of freedom.
    /// </summary>
    /// 
    /// <param name="statistic">The test statistic.</param>
    /// <param name="d1">The degrees of freedom for the numerator.</param>
    /// <param name="d2">The degrees of freedom for the denominator.</param>
    type FTestStatistics = {
        Statistic            : float
        DegreesOfFreedom1    : float
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
    /// 
    /// <param name="Statistic">The test statistic.</param>
    /// <param name="DegreesOfFreedom">The degrees of freedom for the numerator.</param>    
    /// <param name="PValueLeft">One Tailed/Sided.</param>
    /// <param name="PValueRight"> One Tailed/Sided.</param>   
    /// <param name="PValue">Two Tailed/Sided.</param>   
    type ChiSquareStatistics = {
        Statistic            : float
        DegreesOfFreedom     : float
        /// one tailed/sided chiSquare pValue
        PValueLeft           : float
        /// one tailed/sided chiSquare pValue (default)
        PValueRight          : float
        /// two tailed/sided chiSquare pValue
        PValue               : float            
    }


    let createChiSquare statistic dof =
        let cdf  = Distributions.Continuous.ChiSquared.CDF dof statistic
        let pvalue = if statistic > 0. then 1. - cdf else cdf
        {Statistic = statistic; DegreesOfFreedom = dof; PValueLeft = 1. - pvalue; PValueRight = pvalue; PValue = pvalue * 2.}

    
    /// <summary>
    ///   Computes the Wilcoxon test statistics for a given statistic.
    /// </summary>
    /// <param name="Statistic">The test statistic.</param>
    /// <param name="PValue">One Tailed/Sided.</param>
    /// <param name="PValueTwoTailed">Two Tailed/Sided.</param>   
    type WilcoxonTestStatistics = {
        Statistic            : float
        PValueLeft           : float
        PValueRight          : float 
        PValueTwoTailed      : float 
    }    
    let createWilcoxon statistic =
        let cdf  =  Distributions.Continuous.Normal.CDF 0. 1.  statistic         
        let pvalue = 1.-  cdf
        let pvalueTwoTailed = pvalue * 2.
        {Statistic=statistic; PValueLeft=pvalue;PValueRight = cdf; PValueTwoTailed = pvalueTwoTailed}


    /// <summary>
    ///   Computes the Mann-Whitney U-test statistics for a given statistic.
    /// </summary>
    /// <param name="Statistic">The test statistic.</param>
    /// <param name="PValue">One Tailed/Sided.</param>
    /// <param name="PValueTwoTailed">Two Tailed/Sided.</param>
    type UTestTestStatistics = {
        Statistic       : float
        PValueLeft      : float
        PValueRight     : float
        PValueTwoTailed : float
    }
    let createUTest statistic : UTestTestStatistics =
        let cdf = Distributions.Continuous.Normal.CDF 0. 1. statistic
        {
            Statistic       = statistic
            PValueLeft      = 1. - cdf
            PValueRight     = cdf
            PValueTwoTailed = cdf * 2.
        }