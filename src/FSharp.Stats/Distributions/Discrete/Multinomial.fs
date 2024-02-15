namespace FSharp.Stats.Distributions.Discrete

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.SpecialFunctions
    
// n is the number of trails,
// k is the vector of observed successes
// p is the vector of event probabilities in each trial

type Multinomial =
    
    // Multinomial distribution helper functions.
    static member CheckParam (p: vector) n  = 
        if n < 0 then
            failwith "Multinomial distribution should be parametrized by n >= 0."
        let checkBetween p =
            p < 0. || p > 1. || isNan(p)
        if (p |> Seq.map checkBetween |> Seq.exists id) then 
            failwith "Multinomial distribution should be parametrized by 0 ≤ p_i ≤ 1."
        
        let pSum = Seq.sum p
        if Math.Round(pSum,15) <> 1. then 
            failwithf "Multinomial distribution: The sum of probabilities should sum up to 1 but sums up to %.16f" pSum
    
    /// <summary>Computes the mean vector</summary>
    /// <remarks></remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="n">number of trails</param>
    static member Mean (p: vector) (n: int) =
        Multinomial.CheckParam p n
        p * float n

    /// <summary>Computes the variance vector</summary>
    /// <remarks></remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="n">number of trails</param>
    static member Variance p n =
        Multinomial.CheckParam p n
        Vector.init p.Length (fun i -> p.[i] * (1. - p.[i]) * float n)

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="n">number of trails</param>
    static member StandardDeviation p n =
        Multinomial.CheckParam p n
        Vector.map sqrt (Multinomial.Variance p n)

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()). No parameter checking!</summary>
    /// <remarks></remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="n">number of trails</param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member internal SampleUnchecked p n =          
        failwithf "Not implemented yet"

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="n">number of trails</param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample p n =
        Multinomial.CheckParam p n
        Multinomial.SampleUnchecked p n

    /// <summary>The probability mass function (PMF) for the multinomial distribution describes the probability of observing a specific set of outcomes in a series of independent trials with different categories. P(K_i = k_i)</summary>
    /// <remarks>n (sum of x's) must be smaller than 170 to not result in infinity due to factorial calculations</remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="x">vector of observed successes</param>
    /// <returns>Probability of observing the exact outcome of k's with given p's.</returns>
    /// <example>
    /// <code>
    ///   open FSharp.Stats
    ///   open FSharp.Stats.Distributions.Discrete
    ///   let p = vector [0.3; 0.5; 0.2]
    ///   let x = Vector.Generic.ofArray [|2; 4; 1|]
    ///   let pdf = Multinomial.PMF p x
    ///   // result: 0.118125
    /// </code>
    /// </example>
    static member PMF_Unchecked (p: vector) (x: Vector<int>) =
        //corresponds to function with gamma notation
        let n = x |> Seq.sum |> int
        let a = Factorial.factorialLn n
        let b = x |> Seq.fold (fun acc xi -> Factorial.factorialLn xi + acc) 0.
        let c = p |> Seq.indexed |> Seq.fold (fun acc (i,pi) -> pi**x.[i] * acc) 1.
        floor (0.5 + Math.Exp(a - b)) * c // must be an integer, to compensate for floating point errors: floor +0.5

    static member PMF (p: vector) (x: Vector<int>) =
        let n = x |> Seq.sum |> int
        //checks
        Multinomial.CheckParam p n
        if p.Length <> x.Length then failwithf "Probability vector must be of same size as success vector!"
        Seq.iter2 (fun pi xi -> if pi = 0. && xi <> 0 then failwithf "At least one probability of 0. is associated with a success event! This is impossible") p x
        Multinomial.PMF_Unchecked p x

    /// Computes the cumulative distribution.
    static member CDF p n (x:float) =
        Multinomial.CheckParam p n
        failwithf "not implemented yet, difficult for multinomial results!"

    /// <summary>Returns the support of the Multinomial distribution: for each x: [0, n].</summary>
    /// <remarks></remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="n">number of trails</param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support (p: vector) n =
        Multinomial.CheckParam p n
        Vector.Generic.init p.Length (fun xi -> Interval.createClosedOfSize 0 n)

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="n">number of trails</param>
    static member ToString (p: vector) (n: int) =
        sprintf "Multinomial(p = %A, n = %i,)" p n

    /// <summary>Initializes a Multinomial distribution</summary>
    /// <remarks></remarks>
    /// <param name="p">vector of event probabilities in each trial</param>
    /// <param name="n">number of trails</param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    //static member Init p n =
    //    { new DiscreteDistribution<float,int> with
    //        member d.Mean              = Multinomial.Mean p n
    //        member d.StandardDeviation = Multinomial.StandardDeviation p n
    //        member d.Variance          = Multinomial.Variance p n
    //        member d.CDF x             = Multinomial.CDF p n x
    //        member d.InvCDF x          = Multinomial.InvCDF p n x
    //        //member d.CoVariance        = Multinomial.CoVariance p n
    //        member d.Mode              = Multinomial.Mode p n
    //        member d.Sample ()         = Multinomial.Sample p n
    //        member d.PMF k             = Multinomial.PMF p n k    
    //        member d.Parameters        = DistributionParameters.Multinomial {P=p;N=n}
    //        override d.ToString()      = Multinomial.ToString p n
    //    }   

