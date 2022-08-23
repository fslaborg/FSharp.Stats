namespace FSharp.Stats.Distributions.Discrete

open FSharp.Stats
open FSharp.Stats.Distributions

// ######
// Bernoulli distribution
// ######


   
/// Bernoulli distribution.
type Bernoulli =

    // https://planetcalc.com/486/
    // > Mean, or expected value of a binomial distribution is equal to "np"(n=1 in bernoulli distribution),
    // > and the variance is equal to "np(1-p)"

    // Bernoulli distribution helper functions.
    static member CheckParam p = 
        if p < 0.0 || p > 1.0 then failwith "Bernoulli distribution should be parametrized by p in [0.0, 1.0]."


    /// Computes the mean.
    static member Mean p =
        Bernoulli.CheckParam p
        p

    /// Computes the variance.
    static member Variance p =
        Bernoulli.CheckParam p
        p * (1.0 - p)

    /// Computes the standard deviation.
    static member StandardDeviation p =
        Bernoulli.CheckParam p
        sqrt (p * (1.0 - p))

    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample p = 
        Bernoulli.CheckParam p
//            if rndgen.NextFloat() < p then 0.0 else 1.0
        failwith "Not implemented yet."

    // Rename PMF? https://en.wikipedia.org/wiki/Probability_mass_function
    // > A probability mass function differs from a probability density function (PDF) in that the latter is associated with continuous 
    // > rather than discrete random variables. A PDF must be integrated over an interval to yield a probability.

    /// Computes the probability density function.
    static member PDF p x =
        Bernoulli.CheckParam p
        match x with
        | 0.0 -> 1.0 - p
        | 1.0 -> p
        | _ -> 0.0

    /// Computes the cumulative distribution function. P(X>=k)
    static member CDF p x =
        Bernoulli.CheckParam p
        // Summary: This cdf calculates the probability, that value x is greater or equal to a random value (R) taken from the bernoulli distribution.
        // Reminder: A bernoulli distribution can only return 0 or 1 as result.
        //// If the value x is greater than 1.0, then the probability that x is greater than the random outcome (R) is 1.0, since R∈{0,1}.
        if x >= 1.0 then 1.0
        //// Example: p = 0.8. 80% of the time R=1 and 20% of the time R=0. The probability that x in the range of 0.0 ... 0.99 is greater than R is 20%. Therefore 1-p=q.
        elif x >= 0.0 then 1.0 - p
        // If the value x is less than 0, the probability that x is greater than the random outcome (R) of p is 0 since, R∈{0,1}.
        else 0.0

    /// Returns the support of the bernoulli distribution: {0, 1}.
    static member Support p =
        Bernoulli.CheckParam p
        [0.0; 1.0]

    /// Initializes a uniform distribution 
    static member Init p =
        { new Distribution<float,float> with
            member d.Mean              = Bernoulli.Mean p
            member d.StandardDeviation = Bernoulli.StandardDeviation p 
            member d.Variance          = Bernoulli.Variance p
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = Bernoulli.Sample p
            member d.PDF x             = Bernoulli.PDF p x           
            member d.CDF x             = Bernoulli.CDF p x         
        }   



