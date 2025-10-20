namespace FSharp.Stats.Testing
open System
open FsMath
open FsMath.GenericMath

// TODO: Update specific distributions to support generic type 'T to avoid explicit float casting

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
        and Numerics.IPowerFunctions<'T>
        and 'T: (static member op_Explicit: ^T -> float)
        and 'T : comparison >  = 
        {
            Statistic            : 'T
            DegreesOfFreedom     : 'T
            PValueLeft           : 'T
            PValueRight          : 'T
            PValue               : 'T            
        }

    let inline createTTest<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.ILogarithmicFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T> 
        and 'T: (static member op_Explicit: ^T -> float)
        and 'T : comparison > 
        (statistic: 'T) (dof: 'T) =
        let fstatistic = toFloat statistic
        let fdof = toFloat dof
        
        let cdf  = Distributions.Continuous.StudentT.CDF 0. 1. fdof fstatistic |> T
        let pvalue = if fstatistic > 0. then 1. - cdf else cdf
        
        {
            Statistic=statistic; 
            DegreesOfFreedom=dof; 
            PValueLeft= T(1. - pvalue); 
            PValueRight=T(pvalue); 
            PValue=T(pvalue*2.);
        }

    /// <summary>
    ///   Creates a new F-Test for a given statistic
    ///   with given degrees of freedom.
    /// </summary>
    /// 
    /// <param name="statistic">The test statistic.</param>
    /// <param name="d1">The degrees of freedom for the numerator.</param>
    /// <param name="d2">The degrees of freedom for the denominator.</param>
    type FTestStatistics<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>
        and 'T: (static member op_Explicit: ^T -> float)
        and 'T : comparison > = 
        {
            Statistic            : 'T
            DegreesOfFreedom1    : 'T
            DegreesOfFreedom2    : 'T
            PValue               : 'T 
            PValueTwoTailed      : 'T            
        }

    let inline createFTest<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>
        and 'T: (static member op_Explicit: ^T -> float)
        and 'T : comparison >
        (statistic: 'T) (dof1: 'T) (dof2: 'T) =
        let fstatistic = toFloat statistic
        let fdof1 = toFloat dof1
        let fdof2 = toFloat dof2
        let cdf  =  Distributions.Continuous.F.CDF fdof1 fdof2 fstatistic            
        let pvalue = 1. - cdf
        let pvalueTwoTailed = pvalue * 2.
        {
            Statistic=statistic; 
            DegreesOfFreedom1=dof1; 
            DegreesOfFreedom2=dof2; 
            PValue=T pvalue; 
            PValueTwoTailed = T pvalueTwoTailed
        }


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
    type ChiSquareStatistics<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>
        and 'T: (static member op_Explicit: ^T -> float)
        and 'T : comparison > = 
        {
            Statistic            : 'T
            DegreesOfFreedom     : 'T
            /// one tailed/sided chiSquare pValue
            PValueLeft           : 'T
            /// one tailed/sided chiSquare pValue (default)
            PValueRight          : 'T
            /// two tailed/sided chiSquare pValue
            PValue               : 'T            
        }


    let inline createChiSquare<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>
        and 'T: (static member op_Explicit: ^T -> float)
        and 'T : comparison > 
        (statistic:'T) (dof:'T) =
        let fstatistic = toFloat statistic
        let fdof = toFloat dof
        
        
        let cdf  = Distributions.Continuous.ChiSquared.CDF fdof fstatistic
        let pvalue = if fstatistic > 0. then 1. - cdf else cdf
        {
            Statistic = statistic; 
            DegreesOfFreedom = dof; 
            PValueLeft =T(1. - pvalue); 
            PValueRight = T pvalue; 
            PValue = T(pvalue * 2.)
        }

    
    /// <summary>
    ///   Computes the Wilcoxon test statistics for a given statistic.
    /// </summary>
    /// <param name="Statistic">The test statistic.</param>
    /// <param name="PValue">One Tailed/Sided.</param>
    /// <param name="PValueTwoTailed">Two Tailed/Sided.</param>   
    type WilcoxonTestStatistics<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>
        and 'T: (static member op_Explicit: ^T -> float)
        and 'T : comparison > = 
        {
            Statistic            : 'T
            PValueLeft           : 'T
            PValueRight          : 'T 
            PValueTwoTailed      : 'T 
        }    
    let inline createWilcoxon<'T when 'T :> Numerics.INumber<'T>
        and Numerics.IFloatingPoint<'T>
        and Numerics.IExponentialFunctions<'T>
        and Numerics.IRootFunctions<'T>
        and Numerics.IPowerFunctions<'T>
        and 'T: (static member op_Explicit: ^T -> float)
        and 'T : comparison >
        (statistic:'T) =
        let fstatistic = toFloat statistic

        let cdf  =  Distributions.Continuous.Normal.CDF 0. 1.  fstatistic         
        let pvalue = 1.-  cdf
        let pvalueTwoTailed = pvalue * 2.
        {
            Statistic=statistic; 
            PValueLeft=T pvalue;
            PValueRight = T cdf; 
            PValueTwoTailed = T pvalueTwoTailed
        }