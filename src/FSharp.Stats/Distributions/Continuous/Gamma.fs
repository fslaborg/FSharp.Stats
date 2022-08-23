namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops


// ######
// Gamma distribution
// ######


    
/// Gamma distribution
/// Sampling implementation based on:
///     "A Simple Method for Generating Gamma Variables" - Marsaglia & Tsang
///     ACM Transactions on Mathematical Software, Vol. 26, No. 3, September 2000, Pages 363-372.
type Gamma =

    // Gamma distribution helper functions.
    static member CheckParam alpha beta = 
        if alpha <= 0.0 || beta <= 0.0 then 
            failwith "Gamma distribution should be parametrized by alpha > 0.0, beta > 0.0."

    /// Computes the mean.
    static member Mean alpha beta =
        Gamma.CheckParam alpha beta
        alpha / beta
        
    /// Computes the variance.
    static member Variance alpha beta =
        Gamma.CheckParam alpha beta
        alpha / (beta * beta)
        
    /// Computes the standard deviation.
    static member StandardDeviation alpha beta =
        Gamma.CheckParam alpha beta
        sqrt (alpha / (beta * beta))
        
    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample alpha beta = 
        // Source: fsmathtools (same in MN)
        Gamma.CheckParam alpha beta
        let mutable a = alpha
        // Fix when alpha is less than one.
        let alphafix =
            if alpha < 1.0 then
                a <- alpha + 1.0
                (Random.rndgen.NextFloat() ** (1.0 / alpha))
            else
                1.0
        let d = a - 1.0 / 3.0
        let c = 1.0 / sqrt(9.0 * d)
        let rec gammaSample () =
            // TODO Implement unchecked Normal.Sample 
            let mutable x = Normal.Sample 0.0 1.0
            let mutable v = 1.0 + c * x
            while v <= 0.0 do
                x <- Normal.Sample 0.0 1.0
                v <- 1.0 + c * x
            v <- v * v * v
            let u = Random.rndgen.NextFloat()
            x <- x * x
            if u < 1.0 - 0.0331 * x * x then
                d * v
            elif (log u) < 0.5 * x + d * (1.0 - v + (log v)) then
                d * v
            else gammaSample()
        alphafix * gammaSample() / beta
        //failwith "Not implemented yet."
        
    /// Computes the probability density function.
    static member PDF alpha beta x = 
        Gamma.CheckParam alpha beta
        if x >= 0.0 then
            //(beta**alpha) * (x ** (alpha - 1.0)) * (exp (-beta*x)) / SpecialFunctions.Gamma.gamma alpha
            Math.Pow(beta, alpha) * Math.Pow(x, alpha - 1.0) * (exp (-beta * x)) / SpecialFunctions.Gamma._gamma alpha
        else 0.0
        
    /// Computes the cumulative distribution function.
    static member CDF alpha beta x =
        Gamma.CheckParam alpha beta
        if alpha = 0.0 && beta = 0.0 then 
            0.0
        else 
            SpecialFunctions.Gamma.lowerIncomplete alpha (x * beta)
        
    /// Fits the underlying distribution to a given set of observations.
    static member Fit(observations:float[],?maxIter,?tolerance) =
        let maxIter = defaultArg maxIter 10000
        let tol     = defaultArg tolerance 1e-8
            
        let lnSum = observations |> Seq.averageBy (log)
        let mean = observations |> Seq.average

        let s = log(mean) - lnSum / float observations.Length

        if (Double.IsNaN(s)) then
            raise (ArgumentException("Observation vector contains negative values.", "observations"))
    
        // initial approximation
        let alpha' = (3. - s + Math.Sqrt((s - 3.) * (s - 3.) + 24. * s)) / (12. * s)  

        let rec newtonRaphson iter state =
            if iter < maxIter && (true) then
                let num = Math.Log(state) - SpecialFunctions.Gamma.digamma(state) - s
                let den = (1. / state) - SpecialFunctions.Gamma.trigamma(state)
                let state' = state - num / den
                if (abs (state' - state) > tol ) then
                    newtonRaphson (iter+1) (state')
                else
                    state
            else
                state    

        let alpha = newtonRaphson 0 alpha' 
            
        let theta = mean / alpha 
        (alpha, 1. / theta) // beta = 1 / theta
            
    /// <summary>
    ///   Estimates a new Gamma distribution from a given set of observations.
    /// </summary>
    static member Estimate(observations:float[],?maxIter,?tolerance) =
        let maxIter = defaultArg maxIter 10000
        let tol     = defaultArg tolerance 1e-8    
        let alpha,beta = Gamma.Fit(observations,maxIter,tol)
        Gamma.Init alpha beta 

    /// Returns the support of the exponential distribution: [0, Positive Infinity).
    static member Support alpha beta =
        Gamma.CheckParam alpha beta
        (0.0, System.Double.PositiveInfinity)

    /// Initializes a Gamma distribution
    static member Init alpha beta =
        { new Distribution<float,float> with
            member d.Mean              = Gamma.Mean alpha beta
            member d.StandardDeviation = Gamma.StandardDeviation alpha beta   
            member d.Variance          = Gamma.Variance alpha beta
            //member d.CoVariance        = Gamma.CoVariance alpha beta 
            member d.Sample ()         = Gamma.Sample alpha beta
            member d.PDF x             = Gamma.PDF alpha beta x           
            member d.CDF x             = Gamma.CDF alpha beta x         
        }

