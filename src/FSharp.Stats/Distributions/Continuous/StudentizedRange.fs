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
    /// Studentized range distribution helper functions.
    static member CheckParam q r v = 
        if  System.Double.IsNaN(q) || 
            System.Double.IsNaN(r) || 
            System.Double.IsNaN(v) ||
            r < 1.0 || 
            v < 1.0 
            then failwith "Studentized range distribution should be parametrized by r and v > 1.0."
    

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

    /// Computes the cumulative density function.
    /// Accuracy defines the number of steps within the integration (Recommended: 1k-10k, default: 2k). pValue accuracy is minimum 3 digits for v>3 at pValue=0.05.
    /// q:qValue r:number of treatments v:df (n-r) c:1.
    /// Integration can be performed in parallel using PSeq
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

        //Lawal B, Applied Statistical Methods in Agriculture, Health and Life Sciences, DOI 10.1007/978-3-319-05555-8, 2014
        //StudentizedRange.CDF 18.   2.  1. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9473 (2k accuracy) 0.9459 (1k accuracy)
        //StudentizedRange.CDF 59.6 20.  1. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9618 (2k accuracy) 0.9459 (1k accuracy)
        //StudentizedRange.CDF 6.08  2.  2. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9507 (2k accuracy) 0.9521 (1k accuracy)
        //StudentizedRange.CDF 16.8 20.  2. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9503 (2k accuracy) 0.9481 (1k accuracy)
        //StudentizedRange.CDF 4.5   2.  3. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9501 (2k accuracy) 0.9505 (1k accuracy)
        //StudentizedRange.CDF 11.2 20.  3. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9495 (2k accuracy) 0.9495 (1k accuracy)
        //StudentizedRange.CDF 3.93  2.  4. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9501 (2k accuracy) 0.9901 (1k accuracy)
        //StudentizedRange.CDF 9.23 20.  4. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9499 (2k accuracy) 0.9901 (1k accuracy)
        //StudentizedRange.CDF 3.64  2.  5. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9502 (2k accuracy)        (1k accuracy)
        //StudentizedRange.CDF 8.21 20.  5. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9500 (2k accuracy)        (1k accuracy)
        //StudentizedRange.CDF 3.46  2.  6. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9500 (2k accuracy)        (1k accuracy)
        //StudentizedRange.CDF 7.59 20.  6. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9501 (2k accuracy)        (1k accuracy)
    
    /// Initializes a studentized range distribution.     
    /// Accuracy defines the number of steps within the CDF integration (Recommended: 1k-10k, default: 2k). pValue accuracy is minimum 3 digits for v>3.
    /// q:qValue r:number of treatments v:df (n-r) c:1.   
    /// Integration can be performed in parallel using PSeq
    static member Init r v c accuracy computeParallel =
        { new Distribution<float,float> with
            member d.Mean              = StudentizedRange.Mean
            member d.StandardDeviation = StudentizedRange.StandardDeviation
            member d.Variance          = StudentizedRange.Variance
            member d.Sample ()         = StudentizedRange.Sample()
            member d.PDF x             = StudentizedRange.PDF      
            member d.CDF q             = StudentizedRange.CDF q r v c accuracy computeParallel
        }   

