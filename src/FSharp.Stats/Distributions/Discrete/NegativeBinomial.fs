namespace FSharp.Stats.Distributions.Discrete

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops


// ######
// NegativeBinomial distribution
// ######
    
//r = number of failures
//p = success probability

type NegativeBinomial =

    // NegativeBinomial distribution helper functions.
    static member CheckParam r p = 
        if r <= 0 || (p < 0. || p > 1.) then 
            failwith "NegativeBinomial distribution should be parametrized by number of failures r > 0.0, sucess probability p between 0.0 ans 1.0."

    /// Computes the mode.
    static member Mode r p =
        NegativeBinomial.CheckParam r p
        if r > 1 then
            Math.Floor(float (r - 1) * (1.0 - p)/p) |> int
        else
            0


    /// Computes the mean.
    static member Mean r p =
        NegativeBinomial.CheckParam r p
        (p * float r) / (1. - p)
        
    /// Computes the variance.
    static member Variance r p =
        NegativeBinomial.CheckParam r p
        (p * float r) / ((1. - p) * (1. - p))
        
    /// Computes the standard deviation.
    static member StandardDeviation r p =
        sqrt (NegativeBinomial.Variance r p)

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member SampleUnchecked r p =
        let lambda = Distributions.Continuous.Gamma.SampleUnchecked (float r) p
        let c = Math.Exp(-lambda)
 
        let rec loop pp kk =
            let kk' = kk + 1
            let pp' = pp * Random.rndgen.NextFloat()
            if pp' >= c then
                loop pp' kk'
            else 
                kk' - 1
        
        loop 1.0 0

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample r p =
        NegativeBinomial.CheckParam r p
        NegativeBinomial.SampleUnchecked r p
   
        
    /// Computes the probability mass function.
    static member PMF r p x = 
        NegativeBinomial.CheckParam r p
        (SpecialFunctions.Binomial.coeffcient (x + r - 1) (r - 1)) * Math.Pow(1. - p, float x) * Math.Pow(p, r)
       
    /// Computes the log probability mass function.
    static member PDFLn r p x = 
        NegativeBinomial.CheckParam r p
        (SpecialFunctions.Binomial.coeffcientLn (x + r - 1) (r - 1)) + float x * Math.Log(1. - p) + float r * Math.Log(p) 

    /// Computes the cumulative distribution function.
    static member CDF r p x =
        NegativeBinomial.CheckParam r p
        1.0 - SpecialFunctions.Beta.lowerIncomplete(float x + 1.,float  r, 1. - p)
        

    ///// Fits the underlying distribution to a given set of observations.
    //static member Fit(observations:float[],?maxIter,?tolerance) =
    //    let maxIter = defaultArg maxIter 10000
    //    let tol     = defaultArg tolerance 1e-8
            
    //    let lnSum = observations |> Seq.sumBy (log)
    //    let mean = observations |> Seq.average

    //    let s = log(mean) - lnSum / float observations.Length

    //    if (Double.IsNaN(s)) then
    //        raise (ArgumentException("Observation vector contains negative values.", "observations"))
    
    //    // initial approximation
    //    let alpha' = (3. - s + Math.Sqrt((s - 3.) * (s - 3.) + 24. * s)) / (12. * s)  

    //    let rec newtonRaphson iter state =
    //        if iter < maxIter && (true) then
    //            let num = Math.Log(state) - SpecialFunctions.Gamma.digamma(state) - s
    //            let den = (1. / state) - SpecialFunctions.Gamma.trigamma(state)
    //            let state' = state - num / den
    //            if (abs (state' - state) > tol ) then
    //                newtonRaphson (iter+1) (state')
    //            else
    //                state
    //        else
    //            state    

    //    let alpha = newtonRaphson 0 alpha' 

    //    (alpha, (mean / alpha) ) 
            
    ///// <summary>
    /////   Estimates a new Gamma distribution from a given set of observations.
    ///// </summary>
    //static member Estimate(observations:float[],?maxIter,?tolerance) =
    //    let maxIter = defaultArg maxIter 10000
    //    let tol     = defaultArg tolerance 1e-8    
    //    let alpha,beta = NegativeBinomial.Fit(observations,maxIter,tol)
    //    NegativeBinomial.Init alpha beta 

    /// Returns the support of the NegativeBinomial distribution: [0, max Int32).
    static member Support r p =
        NegativeBinomial.CheckParam r p
        (0.0, System.Int32.MaxValue)

    /// A string representation of the distribution.
    static member ToString r p = 
        sprintf "NegativeBinomial(r = %i, p = %f)" r p

    /// Initializes a NegativeBinomial distribution
    static member Init r p =
        { new DiscreteDistribution<_,int> with            
            member d.Mean              = NegativeBinomial.Mean r p
            member d.StandardDeviation = NegativeBinomial.StandardDeviation r p   
            member d.Variance          = NegativeBinomial.Variance r p
            member d.CDF x             = NegativeBinomial.CDF r p x

            member d.Mode              = NegativeBinomial.Mode r p
            member d.Sample ()         = NegativeBinomial.Sample r p
            member d.PMF x             = NegativeBinomial.PMF r p x           
            
            override d.ToString()  = NegativeBinomial.ToString r p
        }

