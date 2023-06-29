namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// F-distribution or Fisher–Snedecor distribution
// ----------------------------------------------
// wiki: "https://en.wikipedia.org/wiki/F-distribution"
// ######


/// F-distribution
type F =
    
    // F-distribution helper functions.
    static member CheckParam dof1 dof2 = 
        if dof1 <= 0.0 || dof2 <= 0.0 || isNan(dof1) || isNan(dof2) then 
            failwith "F-distribution should be parametrized by dof1 and dof2 > 0.0. and <> nan"

    static member private CheckX x = 
        if x<0. || isNan(x) then 
            failwith "X cannot be a negative value or nan"

    /// Computes the Mode.
    static member Mode dof1 dof2 =
        F.CheckParam dof1 dof2
        if (dof1 <= 2) then raise (NotSupportedException())        
        
        (dof2*(dof1 - 2.0))/(dof1*(dof2 + 2.0))
        
    /// Computes the mean.
    static member Mean dof1 dof2 =
        F.CheckParam dof1 dof2
        if dof2 <= 2. then
            nan
        else
            dof2 / (dof2 - 2.0)

    /// Computes the variance.
    static member Variance dof1 dof2 =
        F.CheckParam dof1 dof2
        if dof2 <= 4. then
            nan
        else
            (2.0 * dof2 * dof2 * (dof1 + dof2 - 2.)) /
                        (dof1 * (dof2 - 2.) * (dof2 - 2.) * (dof2 - 4.))

    /// Computes the standard deviation.
    static member StandardDeviation dof1 dof2 =
        F.CheckParam dof1 dof2
        sqrt (F.Variance dof1 dof2)
            

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample dof1 dof2 =
        F.CheckParam dof1 dof2
        let gamma1 = Gamma.Sample (dof1 / 2.0) 2.0
        let gamma2 = Gamma.Sample (dof2 / 2.0) 2.0
        gamma1 / gamma2

    /// Computes the probability density function.
    static member PDF dof1 dof2 x =
        F.CheckParam dof1 dof2
        F.CheckX x
        if isInf(dof1) && isInf(dof2) then
            if x=1. then
                infinity
            else
                0.
        elif dof1 > 1e14 || isInf(dof1) then
            Gamma.PDF (dof2/2.) (2./dof2) (1./x)
        elif isInf(dof2) || isInf(dof2**dof2) then
            Gamma.PDF (dof1/2.) (2./dof1) x
        else
            let b = SpecialFunctions.Beta.beta (dof1 * 0.5) (dof2 * 0.5)                
            (1./b) * (Math.Pow(dof1/dof2, (dof1/2.))) * (Math.Pow(x, ((dof1/2.)-1.))) *(Math.Pow((1.+x*(dof1/dof2),(-1.*((dof1+dof2)/2.)))))

    /// Computes the cumulative distribution function.
    static member CDF dof1 dof2 x =
        F.CheckParam dof1 dof2
        F.CheckX x
        //equals 1 - cdf(x)
        //let u = dof2 / (dof2 + dof1 * x)
        //Beta.lowerIncomplete (dof2 * 0.5) (dof1 * 0.5) u
        //equals cdf(x)
        let u = (dof1 * x) / (dof2 + dof1 * x) 
        SpecialFunctions.Beta.lowerIncompleteRegularized (dof1 * 0.5) (dof2 * 0.5) u

    /// Computes the inverse of the cumulative distribution function.
    static member InvCDF dof1 dof2 x =
        F.CheckParam dof1 dof2
        if (x <= 0.0 || x > 1.0) then
            invalidArg "P" "Input must be between zero and one"
        else
            //let u = dof2 / (dof2 + dof1 * x)
            //Beta.lowerIncomplete (dof2 * 0.5) (dof1 * 0.5) u
            failwithf "InvCDF not implemented yet"

    /// Returns the support of the exponential distribution: (0., Positive Infinity).
    static member Support dof1 dof2 =
        F.CheckParam dof1 dof2
        (0., System.Double.PositiveInfinity)
     
    /// A string representation of the distribution.
    static member ToString dof1 dof2 =
        sprintf "FisherSnedecor(d1 = %f, d2 = %f" dof1 dof2
    
    /// Initializes a F-distribution         
    static member Init dof1 dof2 =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = F.Mean dof1 dof2
            member d.StandardDeviation = F.StandardDeviation dof1 dof2
            member d.Variance          = F.Variance dof1 dof2
            member d.CDF x             = F.CDF dof1 dof2 x
            member d.InvCDF x          = F.InvCDF dof1 dof2 x
            
            member d.Mode              = F.Mode dof1 dof2
            member d.Sample ()         = F.Sample dof1 dof2
            member d.PDF x             = F.PDF dof1 dof2 x      
            member d.Parameters        = DistributionParameters.F {DOF1=dof1;DOF2=dof2}
            override d.ToString()      = F.ToString dof1 dof2
        }   
