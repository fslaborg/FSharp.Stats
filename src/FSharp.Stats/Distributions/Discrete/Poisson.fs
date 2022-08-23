namespace FSharp.Stats.Distributions.Discrete

open FSharp.Stats
open FSharp.Stats.Distributions

// ######
// Poisson-Distribution
// ----------------------------------------------
// wiki: "href="https://en.wikipedia.org/wiki/Poisson_distribution"
// ######

    
    // (lambda) is the expected number of occurrences.    


    
///Binomial distribution
type Poisson =

    // Binomial distribution helper functions.
    static member CheckParam lambda = if lambda <= 0. then failwith "Binomial distribution should be parametrized by lambda > 0."


    /// Computes the mean.
    static member Mean lambda =
        Poisson.CheckParam lambda
        failwith "Not implemented yet."

    /// Computes the variance.
    static member Variance lambda =
        Poisson.CheckParam lambda
        failwith "Not implemented yet."

    /// Computes the standard deviation.
    static member StandardDeviation lambda =
        Poisson.CheckParam lambda
        failwith "Not implemented yet."
            
    ///// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    ///// No parameter checking!
    //static member internal SampleUnchecked p n =            
    //    let rec loop p n k =
    //        if k < n then
    //            let k' = if Random.rndgen.NextFloat() < p then k + 1 else k
    //            loop p n k'
    //        else    
    //            k                                        
            
    //    loop p n 0

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    /// No parameter checking!
    static member internal SampleUnchecked lambda =            
        Poisson.CheckParam lambda
        failwith "Not implemented yet."
            

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample lambda =
        Poisson.CheckParam lambda
        failwith "Not implemented yet."

    /// Computes the probability density function at k, i.e. P(K = k)
    static member PDF lambda k =
        if k > 170 then 
            System.Math.E ** (System.Math.Log lambda * float k - SpecialFunctions.Factorial._factorialLn k) * System.Math.E**(-lambda)
        else
            (lambda**float k * System.Math.E**(-lambda)) / SpecialFunctions.Factorial.factorial k


    /// Computes the cumulative distribution function at x, i.e. P(X <= x).
    static member CDF lambda =
        Poisson.CheckParam lambda
        failwith "Not implemented yet."




    /// Returns the support of the Binomial distribution: (0., n).
    static member Support lambda =
        Poisson.CheckParam lambda
        failwith "Not implemented yet."

    /// Initializes a Binomial distribution 
    static member Init lambda =
        { new Distribution<float,int> with
            member d.Mean              = Poisson.Mean lambda
            member d.StandardDeviation = Poisson.StandardDeviation lambda
            member d.Variance          = Poisson.Variance lambda
            //member d.CoVariance        = Binomial.CoVariance p n
            member d.Sample ()         = Poisson.Sample lambda
            member d.PDF k             = Poisson.PDF lambda k
            member d.CDF x             = Poisson.CDF lambda
        }

