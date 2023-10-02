namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// Studentized range (q) distribution
// ------------------------------
// ######

open FSharp.Stats.Integration


/// Studentized range (q) distribution. Used in Tukey's HSD post hoc test.
/// method from: QUANTILES FROM THE MAXIMUM STUDENTIZED RANGE DISTRIBUTION, Ferreira, Rev. Mat. Estat., v.25, n.1, p.117-135, 2007
/// table from: Tables of range and studentized range, Harter, 1960 and Lawal B, Applied Statistical Methods in Agriculture, Health and Life Sciences, DOI 10.1007/978-3-319-05555-8, 2014
type StudentizedRange =
    /// <summary>tudentized range distribution helper functions.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="r"></param>
    /// <param name="v"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CheckParam q r v = 
        if  System.Double.IsNaN(q) || 
            System.Double.IsNaN(r) || 
            System.Double.IsNaN(v) ||
            r < 1.0 || 
            v < 1.0 
            then failwith "Studentized range distribution should be parametrized by r and v > 1.0."
    
    /// Computes the mode.
    static member Mode =
        failwithf "Not implemented yet"

    /// Computes the mean.
    static member Mean =
        failwithf "Not implemented yet"

    /// Computes the variance.
    static member Variance =
        failwithf "Not implemented yet"

    /// Computes the standard deviation.
    static member StandardDeviation =
        failwithf "Not implemented yet"
            

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample() =
        failwithf "Not implemented yet"

    /// Computes the probability density function.
    static member PDF =
        failwithf "Not implemented yet"

    /// <summary>Computes the cumulative density function.<br />Accuracy defines the number of steps within the integration (Recommended: 1k-10k, default: 2k). pValue accuracy is minimum 3 digits for v>3 at pValue=0.05.<br />q:qValue r:number of treatments v:df (n-r) c:1.<br />Integration can be performed in parallel using PSeq</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="r"></param>
    /// <param name="v"></param>
    /// <param name="c"></param>
    /// <param name="accuracy"></param>
    /// <param name="computeParallel"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF q r v c accuracy computeParallel =
        // An alternative (not implemented) algorithm makes use of t statistic to approximate q quite accurate: 
        // An accurate, non-iterativeapproximation for studentizedrange quantiles John R. Gleason ,Computational Statistics & Data Analysis 31 (1999) 147           
        let accuracy   = defaultArg accuracy 2000
    
        StudentizedRange.CheckParam q r v
        let normal = Normal.Init 0. 1.
    
        let h q r =
            let integrateInner y = 
                let normalPDF = normal.PDF y
                let normalCDF = (normal.CDF y - normal.CDF (y - q))**(r - 1.)
                normalPDF * normalCDF
            if  not (Precision.almostEqualNorm (integrateInner -20.) (10.**(-20.))) || 
                not (Precision.almostEqualNorm (integrateInner  20.) (10.**(-20.))) 
                then printfn "Warning: Integral in q distribution H(q) does not start/end at y=0. Extend borders [-20,20]!"
            r * (NumericalIntegration.definiteIntegral(Midpoint, -20., 20., accuracy, Parallel=computeParallel) integrateInner)

    
        let f q r v c =
            let partH u = (h (q * sqrt u) r) ** c
            let gammapart = 2.**(v/2.)*SpecialFunctions.Gamma._gamma (v/2.)
            let sndQuotient u = 
                let a = v**(v/2.)*Math.Exp((-u * v)/2.)*u**(v/2. - 1.)
                a / gammapart
            let com u = partH u * sndQuotient u
            let check =
                let bordercase = com 50.
                if not (Precision.almostEqualNorm bordercase (10.**(-20.))) then 
                    printfn "Warning: Integral in q distribution F(q) does not end at y=0 but at y=%.12f. Extend border [0,50]!" bordercase
            NumericalIntegration.definiteIntegral(Midpoint, 0., 50., accuracy, Parallel=computeParallel) com

        f q r v c

    
    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="r"></param>
    /// <param name="v"></param>
    /// <param name="c"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member InvCDF q r v c =
        failwithf "InvCDF not implemented yet"
    
    /// <summary>Initializes a studentized range distribution.     <br />Accuracy defines the number of steps within the CDF integration (Recommended: 1k-10k, default: 2k). pValue accuracy is minimum 3 digits for v>3.<br />q:qValue r:number of treatments v:df (n-r) c:1.   <br />Integration can be performed in parallel using PSeq</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="v"></param>
    /// <param name="c"></param>
    /// <param name="accuracy"></param>
    /// <param name="computeParallel"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init r v c accuracy computeParallel =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = StudentizedRange.Mean
            member d.StandardDeviation = StudentizedRange.StandardDeviation
            member d.Variance          = StudentizedRange.Variance
            member d.CDF q             = StudentizedRange.CDF q r v c accuracy computeParallel
            member d.InvCDF q          = StudentizedRange.InvCDF q r v c 

            member d.Mode              = StudentizedRange.Mode
            member d.Parameters        = DistributionParameters.StudentizedRange {R=r;V=v;C=c;Accuracy=accuracy;ComputeParallel=computeParallel}
            member d.Sample ()         = StudentizedRange.Sample()
            member d.PDF x             = StudentizedRange.PDF      
            override d.ToString()      = d.ToString()
        }   

