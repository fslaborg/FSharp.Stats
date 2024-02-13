namespace FSharp.Stats.Distributions.Discrete

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.SpecialFunctions
    
// n is the number of trails,
// k is the number of observed successes
// p is the vector of event probabilities in each trial

type Multinomial =
    
    // Multinomial distribution helper functions.
    static member CheckParam (p: vector) n  = 
        if n < 0 then
            failwith "Multinomial distribution should be parametrized by n > 0."
        let checkBetween p =
            p < 0. || p > 1. || isNan(p)
        if (p |> Seq.map checkBetween |> Seq.exists id) then 
            failwith "Multinomial distribution should be parametrized by 0 ≤ p_i ≤ 1."
    
    /// <summary>Computes the mean vector</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean (p: vector) (n: int) =
        Multinomial.CheckParam p n
        p * float n

    /// <summary>Computes the variance vector</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance p n =
        Multinomial.CheckParam p n
        Vector.init p.Length (fun i -> p.[i] * (1. - p.[i]) * float n)

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
        Multinomial.CheckParam p n
        Vector.map sqrt (Multinomial.Variance p n)

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
        //let rec loop n k =
        //    if n = 0 then k
        //    else 
        //        let k' = if Random.rndgen.NextFloat() < p then k + 1 else k
        //        loop (n-1) k'
        //loop n 0
        1.

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
        Multinomial.CheckParam p n
        Multinomial.SampleUnchecked p n

    /// <summary>Computes the probability mass function at k, i.e. P(K_i = k_i)</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <param name="k"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member PMF (p: vector) (k: Vector<int>) =
        let n = k |> Seq.sum |> int
        Multinomial.CheckParam p n
        let a = Factorial.factorialLn n
        let b = k |> Seq.fold (fun acc xi -> Factorial.factorialLn xi + acc) 0.
        let c = p |> Seq.indexed |> Seq.fold (fun acc (i,pi) -> pi**k.[i] * acc) 1.
        floor (0.5 + Math.Exp(a - b)) * c
            
    /// Computes the cumulative distribution function at x, i.e. P(X &lt;= x).
    static member CDF p n (x:float) =
        Multinomial.CheckParam p n
        failwithf "not implemented yet"
        
    /// Computes the inverse cumulative distribution function (quantile function).
    static member InvCDF p n (x:float) =
        Multinomial.CheckParam p n            
        failwithf "InvCDF not implemented yet"

    /// <summary>Returns the support of the Multinomial distribution: (0., n).</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support p n =
        Multinomial.CheckParam p n
        failwithf "InvCDF not implemented yet"

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString (p: vector) (n: int) (k: Vector<int>) =
        sprintf "Multinomial(p = %A, n = %i, k = %A)" p n k

    /// <summary>Initializes a Multinomial distribution</summary>
    /// <remarks></remarks>
    /// <param name="p"></param>
    /// <param name="n"></param>
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

