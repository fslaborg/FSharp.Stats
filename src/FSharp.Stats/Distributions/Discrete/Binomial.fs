namespace FSharp.Stats.Distributions.Discrete

open FSharp.Stats
open FSharp.Stats.Distributions

// ######
// Discrete Univariate Binomial distribution
// ----------------------------------------------
// wiki: "href="http://en.wikipedia.org/wiki/Binomial_distribution"
// ######


    
// (p) is the success probability in each trial.    
// (n) is the number of trails,
//k is the number of observed successe


///Binomial distribution
type Binomial =
    
    // Binomial distribution helper functions.
    static member CheckParam p n = 
        if n < 0 || p < 0. || p > 1. || isNan(p) then 
            failwith "Binomial distribution should be parametrized by n > 0.0 and 0 ≤ p ≤ 1."

    /// <summary>Computes the mode.</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode p n =
        Binomial.CheckParam p n
        match p with
        | 1.0 -> n
        | 0.  -> 0
        | _   -> floor (float(n + 1) * p) |> int
    
    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean p n =
        Binomial.CheckParam p n
        (float n) * p

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance p n =
        Binomial.CheckParam p n
        p * (1.0 - p) * float n

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation p n =
        Binomial.CheckParam p n
        sqrt (Binomial.Variance p n)
            

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).<br />No parameter checking!</summary>
    /// <remarks></remarks>
    /// <param name="SampleUnchecked"></param>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member internal SampleUnchecked p n =          
        let rec loop n k =
            if n = 0 then k
            else 
                let k' = if Random.rndgen.NextFloat() < p then k + 1 else k
                loop (n-1) k'
        loop n 0
        //let rec loop p n k =
        //    if k < n then
        //        let k' = if Random.rndgen.NextFloat() < p then k + 1 else k
        //        loop p n k'
        //    else    
        //        k                                        
            
        //loop p n 0
            

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample p n =
        Binomial.CheckParam p n
        Binomial.SampleUnchecked p n

        
    /// <summary>Computes the probability mass function at k, i.e. P(K = k)</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <param name="k"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member PMF p n k =
        Binomial.CheckParam  p n
        if k < 0 || k > n then
            0.0
        elif p = 0. then
            if k = 0 then 1. else 0.
        else
            exp ( (SpecialFunctions.Binomial._coeffcientLn n k) + (float k * log p + ( float (n - k)*log(1.-p) )) )

            
    /// Computes the cumulative distribution function at x, i.e. P(X &lt;= x).
    static member CDF p n (x:float) =
        Binomial.CheckParam p n            
        if (x < 0.) then 
            0.0
        elif (x > float n) then
            1.0
        else
            let k = floor x |> int 
            SpecialFunctions.Beta.lowerIncompleteRegularized (float (n-k)) (float (k + 1)) (1. - p)
        
    /// Computes the inverse cumulative distribution function (quantile function).
    static member InvCDF p n (x:float) =
        Binomial.CheckParam p n            
        failwithf "InvCDF not implemented yet"

    /// <summary>
    ///   Fits the underlying distribution to a given set of observations.
    /// </summary>
    static member Fit (n:int) (observations:float[]) =
        let n' = float n
        observations
        |> Array.averageBy (fun o -> o / n') 

    /// <summary>
    ///   Estimates a new Binomial distribution from a given set of observations.
    /// </summary>
    static member Estimate (n:int) (observations:float[]) =
        Binomial.Fit n observations
        |> fun p -> Binomial.Init p n  


    /// <summary>Returns the support of the Binomial distribution: (0., n).</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support p n =
        Binomial.CheckParam p n
        Interval.CreateClosed<int> (0,n)


    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString p n =
        sprintf "Binomial(p = %f, n = %i)" p n


    /// <summary>Initializes a Binomial distribution</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init p n =
        { new DiscreteDistribution<float,int> with
            member d.Mean              = Binomial.Mean p n
            member d.StandardDeviation = Binomial.StandardDeviation p n
            member d.Variance          = Binomial.Variance p n
            member d.CDF x             = Binomial.CDF p n x
            member d.InvCDF x          = Binomial.InvCDF p n x
            //member d.CoVariance        = Binomial.CoVariance p n
            member d.Mode              = Binomial.Mode p n
            member d.Sample ()         = Binomial.Sample p n
            member d.PMF k             = Binomial.PMF p n k    
            member d.Parameters        = DistributionParameters.Binomial {P=p;N=n}
            override d.ToString()      = Binomial.ToString p n
        }   

