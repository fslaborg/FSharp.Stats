namespace FSharp.Stats.Distributions.Discrete

open FSharp.Stats
open FSharp.Stats.Distributions

/// <summary>
///   Categorical distribution (finite discrete distribution over k categories with probabilities).
/// </summary>
type Categorical =

    /// <summary>Checks if the given probability vector is valid (all values ∈ [0,1] and sum to 1).</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    static member CheckParam (probabilities: float[]) =
        if probabilities.Length = 0 then failwith "Categorical distribution must have at least one category."
        if probabilities |> Array.exists (fun p -> p < 0. || p > 1. || Ops.isNan(p)) then
            failwith "All probabilities must be in [0, 1]."
        let total = probabilities |> Array.sum
        if abs (total - 1.0) > 1e-8 then
            failwithf "Probabilities must sum to 1.0 (got %f)." total

    /// <summary>Computes the mean (expected index) of the categorical distribution.</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <returns>The weighted average of indices based on probabilities.</returns>
    static member Mean (probabilities: float[]) =
        Categorical.CheckParam probabilities
        probabilities
        |> Array.mapi (fun i p -> float i * p)
        |> Array.sum

    /// <summary>Computes the variance of the categorical distribution.</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <returns>The variance: E[X²] - (E[X])².</returns>
    static member Variance (probabilities: float[]) =
        Categorical.CheckParam probabilities
        let mean = Categorical.Mean probabilities
        probabilities
        |> Array.mapi (fun i p -> p * float i ** 2.0)
        |> Array.sum
        |> fun ex2 -> ex2 - mean ** 2.0

    /// <summary>Generates a random sample from the categorical distribution. No parameter checking.</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <returns>An integer representing the sampled category index.</returns>
    static member SampleUnchecked (probabilities: float[]) =
        let rnd = Random.rndgen.NextFloat()
        let rec search i acc =
            if i >= probabilities.Length then probabilities.Length - 1
            elif acc + probabilities[i] >= rnd then i
            else search (i + 1) (acc + probabilities[i])
        search 0 0.0

    /// <summary>Generates a random sample from the categorical distribution.</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <returns>An integer representing the sampled category index.</returns>
    static member Sample (probabilities: float[]) =
        Categorical.CheckParam probabilities
        Categorical.SampleUnchecked probabilities

    /// <summary>Computes the probability mass function at index k: P(X = k).</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <param name="k">The index to evaluate.</param>
    /// <returns>The probability of observing category k.</returns>
    static member PMF (probabilities: float[]) (k:int) =
        Categorical.CheckParam probabilities
        if k < 0 || k >= probabilities.Length then 0.0
        else probabilities[k]

    /// <summary>Computes the cumulative distribution function at x: P(X ≤ x).</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <param name="x">A float value where CDF is evaluated (interpreted as an index).</param>
    /// <returns>The cumulative probability up to and including ⌊x⌋.</returns>
    static member CDF (probabilities: float[]) (x: float) =
        Categorical.CheckParam probabilities
        if x < 0. then 0.0
        elif x >= float (probabilities.Length) then 1.0
        else
            let k = floor x |> int
            probabilities
            |> Array.take (k + 1)
            |> Array.sum

    /// <summary>Returns the support (valid outcome range) of the distribution: [0, k-1].</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <returns>A closed interval [0, k-1].</returns>
    static member Support (probabilities: float[]) =
        Categorical.CheckParam probabilities
        Interval.CreateClosed<int>(0, probabilities.Length - 1)

    /// <summary>Returns a string representation of the categorical distribution.</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <returns>A string describing the distribution.</returns>
    static member ToString (probabilities: float[]) =
        sprintf "Categorical(p = [%s])" (String.concat "; " (probabilities |> Array.map string))

    /// <summary>Fits the categorical distribution to a set of integer observations (category indices).</summary>
    /// <param name="numCategories">Total number of categories.</param>
    /// <param name="observations">Array of category indices (0-based).</param>
    /// <returns>Estimated probability vector.</returns>
    static member Fit (numCategories:int) (observations:int[]) =
        let counts = Array.zeroCreate numCategories
        for i in observations do
            if i < 0 || i >= numCategories then
                failwithf "Observation index %d out of bounds [0, %d]" i (numCategories - 1)
            counts[i] <- counts[i] + 1
        let total = float (Array.sum counts)
        counts |> Array.map (fun c -> float c / total)

    /// <summary>Estimates a categorical distribution from observed category indices.</summary>
    /// <param name="numCategories">Total number of categories.</param>
    /// <param name="observations">Array of category indices (0-based).</param>
    /// <returns>A categorical distribution fitted to the observations.</returns>
    static member Estimate numCategories observations =
        Categorical.Fit numCategories observations
        |> fun probs -> Categorical.Init probs

    /// <summary>Initializes a categorical distribution instance from a given probability vector.</summary>
    /// <param name="probabilities">An array of probabilities over k categories.</param>
    /// <returns>A distribution implementing DiscreteDistribution&lt;float, int&gt;.</returns>
    static member Init (probabilities: float[]) =
        Categorical.CheckParam probabilities
        { new DiscreteDistribution<float, int> with
            member d.Mean              = Categorical.Mean probabilities
            member d.Variance          = Categorical.Variance probabilities
            member d.StandardDeviation = sqrt (Categorical.Variance probabilities)
            member d.Sample ()         = Categorical.Sample probabilities
            member d.CDF x             = Categorical.CDF probabilities x
            member d.InvCDF x          = failwith "InvCDF not implemented for Categorical"
            member d.Mode              = probabilities |> Array.mapi (fun i p -> i, p) |> Array.maxBy snd |> fst
            member d.PMF k             = Categorical.PMF probabilities k
            member d.Parameters        = DistributionParameters.Categorical probabilities
            override d.ToString()      = Categorical.ToString probabilities
        }


    /// <summary>
    /// Converts a cumulative unnormalized weight array into a normalized PMF.
    /// </summary>
    /// <param name="cdf">An array of cumulative weights.</param>
    /// <returns>A normalized probability mass function.</returns>
    static member CdfToPmf (cdf: float[]) =
        if cdf.Length = 0 then failwith "CDF must not be empty."
        if cdf |> Array.exists (fun x -> x < 0. || Ops.isNan x) then
            failwith "CDF values must be non-negative."
        let pmf = 
            Array.init cdf.Length (fun i ->
                if i = 0 then cdf.[0]
                else cdf.[i] - cdf.[i - 1])
        let total = Array.sum pmf
        if total = 0.0 then failwith "CDF must contain non-zero mass."
        pmf |> Array.map (fun x -> x / total)

    /// <summary>
    /// Initializes a Categorical distribution from an unnormalized CDF.
    /// </summary>
    /// <param name="cdf">An array of cumulative unnormalized weights.</param>
    /// <returns>A normalized Categorical distribution.</returns>
    static member FromCdfUnnormalized (cdf: float[]) =
        let probs = Categorical.CdfToPmf cdf
        Categorical.Init probs

