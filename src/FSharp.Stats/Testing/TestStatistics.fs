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


