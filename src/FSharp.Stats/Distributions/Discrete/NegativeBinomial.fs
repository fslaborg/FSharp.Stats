namespace FSharp.Stats.Distributions.Discrete

open System
open FSharp.Stats
open FSharp.Stats.Distributions

///The distribution of the number of trials needed (x) to get the rth success in repeated independent bernoulli trials with individual probability p. 
//Until the (x-1)th trial (r-1) successes must be achieved (binomial distribution). Therefore to get the rth success in the xth trial, you have to multiply Binom(p,x-1,r-1) by p.
type NegativeBinomial_trials =
    
    //r = number of successes
    //p = success probability
    //x = number of trials

    // NegativeBinomial distribution helper functions.
    static member CheckParam r p = 
        if r <= 0 || (p < 0. || p > 1.) then 
            failwith "NegativeBinomial distribution should be parametrized by number of successes r > 0, sucess probability p between 0.0 and 1.0."

    /// <summary>Computes the mode. Number of trials with the highest probability to obtain r successes with the last trial.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode r p =
        NegativeBinomial_trials.CheckParam r p
        if r > 1 then
            if p = 0. then 
                failwith "Mode cannot be determined if p = 0."
            else
                r + int (Math.Floor(float (r - 1) * (1.0 - p)/p))
        else
            1

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean r p =
        NegativeBinomial_trials.CheckParam r p
        if p = 0. then 
            nan 
        else float r / p
        
    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance r p =
        NegativeBinomial_trials.CheckParam r p
        if p = 0. then 
            nan 
        else (float r * (1. - p)) / (p**2.)
        
    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation r p =
        if p = 0. then 
            nan 
        else sqrt (NegativeBinomial_trials.Variance r p)

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member SampleUnchecked r p =
        failwithf "not implemented yet"

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample r p =
        NegativeBinomial_trials.CheckParam r p
        NegativeBinomial_trials.SampleUnchecked r p

    /// <summary>Computes the probability mass function</summary>
    /// <returns>Probability of requiring x trials when r successes are required at a individual probability of p</returns>
    static member PMF r p (x: int) = 
        NegativeBinomial_trials.CheckParam r p
        if x < r then 0.
        else SpecialFunctions.Binomial.coeffcient (x - 1) (r-1) * pown p r * pown (1. - p) (x - r)
            

    /// Computes the log probability mass function.
    static member PMFLn r p (x: int) = 
        NegativeBinomial_trials.CheckParam r p
        if x < r then 
            log 0.
        else 
            SpecialFunctions.Binomial.coeffcientLn (x - 1) (r-1) + float r * log(p) + float (x - r) * log (1. - p) 
            
    /// <summary>Computes the cumulative distribution function. P(X lower or equal then x)</summary>
    /// <remarks>X=the number of trials at the rth succeess</remarks>
    /// <returns>Probability of requiring a maximum of x trials when r successes are required at a individual probability of p</returns>  
    static member CDF (r: int) (p: float) x =
        NegativeBinomial_trials.CheckParam r p
        if float r > x then 0. 
        else
            1.0 - SpecialFunctions.Beta.lowerIncompleteRegularized ((x - float r) + 1.) (float  r) (1. - p)

    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks>X=the number of trials at the rth succeess</remarks>
    static member InvCDF (r: int) (p: float) x =
        NegativeBinomial_trials.CheckParam r p
        failwithf "InvCDF not implemented yet"

    /// <summary>Returns the support of the NegativeBinomial distribution: [0, max Int32).</summary>
    static member Support r p =
        NegativeBinomial_trials.CheckParam r p
        Interval.CreateRightOpen<int>(r, Int32.MaxValue)

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString r p = 
        sprintf "NegativeBinomial_trials(r = %i, p = %f)" r p

    /// <summary> Initializes a negative binomial distribution.
    /// <summary>The negative binomial distribution is a discrete probability distribution<br />that models the number of trials needed x to get the rth success in repeated <br />independent Bernoulli trials with probability p.<br /></summary><br /><param name="r">The number of success states</param><br /><param name="p">The probability of each independent bernoulli trial</param><br /><param name="x">The number of trials until the rth success</param></summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init r p =
        { new DiscreteDistribution<_,int> with            
            member d.Mean              = NegativeBinomial_trials.Mean r p
            member d.StandardDeviation = NegativeBinomial_trials.StandardDeviation r p   
            member d.Variance          = NegativeBinomial_trials.Variance r p

            member d.Mode              = NegativeBinomial_trials.Mode r p
            member d.Sample ()         = NegativeBinomial_trials.Sample r p
            member d.PMF x             = NegativeBinomial_trials.PMF r p x
            member d.CDF x             = NegativeBinomial_trials.CDF r p x
            member d.Parameters        = DistributionParameters.NegativeBinomial {R=r;P=p}
            member d.InvCDF x          = NegativeBinomial_trials.InvCDF r p x
            
            override d.ToString()  = NegativeBinomial_trials.ToString r p
        }


///The distribution of the number of failures (k) before the rth success in repeated independent bernoulli trials with individual probability p. 
type NegativeBinomial_failures =
    
    //r = number of successes
    //p = success probability
    //k = number of failures

    // NegativeBinomial distribution helper functions.
    static member CheckParam r p = 
        if r <= 0 || (p < 0. || p > 1.) then 
            failwith "NegativeBinomial distribution should be parametrized by number of successes r > 0, sucess probability p between 0.0 and 1.0."
    
    /// <summary>Computes the mode. Number of failures with the highest probability to obtain r successes with the last trial.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode r p =
        NegativeBinomial_failures.CheckParam r p
        if r > 1 then
            if p = 0. then 
                failwith "Mode cannot be determined if p = 0."
            else
                int (Math.Floor(float (r - 1) * (1.0 - p)/p))
        else
            0

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean r p =
        NegativeBinomial_failures.CheckParam r p
        if p = 0. then 
            nan 
        else (float r * (1. - p)) / p
        
    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance r p =
        NegativeBinomial_failures.CheckParam r p
        if p = 0. then 
            nan 
        else (float r * (1. - p)) / (p**2.)
        
    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation r p =
        if p = 0. then 
            nan 
        else sqrt (NegativeBinomial_failures.Variance r p)

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member SampleUnchecked r p =
        failwithf "not implemented yet"
        
    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample r p =
        NegativeBinomial_failures.CheckParam r p
        NegativeBinomial_failures.SampleUnchecked r p
   
    /// <summary>Computes the probability mass function</summary>
    /// <returns>Probability of requiring k failures when r successes are required at a individual probability of p</returns>
    static member PMF r p (k: int) = 
        NegativeBinomial_failures.CheckParam r p
        (SpecialFunctions.Binomial.coeffcient (k + r - 1) k) * Math.Pow(1. - p, float k) * Math.Pow(p, r)

    /// <summary>Computes the log probability mass function</summary>
    /// <returns>log probability of requiring k failures when r successes are required at a individual probability of p</returns>
    static member PMFLn r p (k: int) = 
        NegativeBinomial_failures.CheckParam r p
        (SpecialFunctions.Binomial.coeffcientLn (k + r - 1) (r - 1)) + float k * Math.Log(1. - p) + float r * Math.Log(p) 

    /// <summary>Computes the cumulative distribution function. P(X lower or equal then k)</summary>
    /// <remarks>X=the number of failures before the rth succeess</remarks>
    /// <returns>Probability of requiring a maximum of k failures when r successes are required at a individual probability of p</returns>
    static member CDF (r: int) (p: float) k =
        NegativeBinomial_failures.CheckParam r p
        1.0 - SpecialFunctions.Beta.lowerIncompleteRegularized (float k + 1.) (float  r) (1. - p)

    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks>X=the number of failures before the rth succeess</remarks>
    static member InvCDF (r: int) (p: float) k =
        NegativeBinomial_failures.CheckParam r p
        failwithf "InvCDF not implemented yet"

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

    /// <summary>Returns the support of the NegativeBinomial distribution: [r, max Int32).</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support r p =
        NegativeBinomial_failures.CheckParam r p
        (0, System.Int32.MaxValue)

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString r p = 
        sprintf "NegativeBinomial_failures(r = %i, p = %f)" r p

    /// <summary> Initializes a negative binomial distribution.
    /// <summary>The negative binomial distribution is a discrete probability distribution<br />that models the number of failures needed k to get the rth success in repeated <br />independent Bernoulli trials with probability p.<br /></summary><br /><param name="r">The number of success states</param><br /><param name="p">The probability of each independent bernoulli trial</param><br /><param name="k">The number of failures before the rth success</param></summary>
    /// <remarks></remarks>
    /// <param name="r"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init r p =
        { new DiscreteDistribution<_,int> with            
            member d.Mean              = NegativeBinomial_failures.Mean r p
            member d.StandardDeviation = NegativeBinomial_failures.StandardDeviation r p   
            member d.Variance          = NegativeBinomial_failures.Variance r p

            member d.Mode              = NegativeBinomial_failures.Mode r p
            member d.Sample ()         = NegativeBinomial_failures.Sample r p
            member d.PMF x             = NegativeBinomial_failures.PMF r p x
            member d.CDF x             = NegativeBinomial_failures.CDF r p x
            member d.Parameters        = DistributionParameters.NegativeBinomial {R=r;P=p}
            member d.InvCDF x          = NegativeBinomial_failures.InvCDF r p x
            
            override d.ToString()  = NegativeBinomial_failures.ToString r p
        }

