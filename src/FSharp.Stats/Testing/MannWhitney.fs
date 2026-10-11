namespace FSharp.Stats.Testing

/// <summary>
/// Mann-Whitney U test (also known as the Wilcoxon rank-sum test) for two independent samples.
/// Uses a normal approximation, which is reliable for combined sample sizes ≥ 20.
/// </summary>
module MannWhitneyTest =

    open FSharp.Stats

    /// Result of a Mann-Whitney U test for two independent samples.
    type MannWhitneyTestStatistics =
        {
            /// U statistic for sample 1: number of pairs (x₁, x₂) where x₁ > x₂ (counting ties as 0.5).
            U1: float
            /// U statistic for sample 2: U1 + U2 = n₁ × n₂.
            U2: float
            /// Z-score from the normal approximation.
            Statistic: float
            /// One-sided p-value: P(sample 1 is stochastically less than sample 2).
            PValueLeft: float
            /// One-sided p-value: P(sample 1 is stochastically greater than sample 2).
            PValueRight: float
            /// Two-sided p-value.
            PValue: float
        }

    /// <summary>
    /// Creates a Mann-Whitney U test for two independent samples using a normal approximation.
    /// </summary>
    /// <param name="sample1">First independent sample.</param>
    /// <param name="sample2">Second independent sample.</param>
    /// <param name="continuityCorrection">
    /// When true, applies a continuity correction (±0.5) to the z-score. Recommended for small samples.
    /// Default: true.
    /// </param>
    /// <remarks>
    /// The test is reliable for combined sample sizes ≥ 20. For smaller samples, exact p-values
    /// (not yet implemented) should be used instead of the normal approximation.
    ///
    /// Ties are handled via the standard variance correction.
    ///
    /// A large U1 (close to n₁×n₂) indicates that sample 1 tends to have larger values than sample 2,
    /// yielding a small PValueRight. Conversely, a small U1 yields a small PValueLeft.
    /// </remarks>
    let create (sample1: seq<float>) (sample2: seq<float>) (continuityCorrection: bool) : MannWhitneyTestStatistics =
        let arr1 = Seq.toArray sample1
        let arr2 = Seq.toArray sample2
        let n1   = float arr1.Length
        let n2   = float arr2.Length
        let bigN = n1 + n2

        // Rank combined array; first n1 entries correspond to sample1
        let combined = Array.append arr1 arr2
        let ranks    = Rank.RankAverage() combined

        // Sum of ranks for sample 1
        let r1 = Array.sumBy (fun i -> ranks.[i]) [| 0 .. arr1.Length - 1 |]

        // U statistics
        let u1 = r1 - n1 * (n1 + 1.) / 2.
        let u2 = n1 * n2 - u1

        // Variance with tie correction
        let tieCorrection =
            ranks
            |> Array.countBy id
            |> Array.sumBy (fun (_, cnt) ->
                let t = float cnt
                t * t * t - t)

        let mu  = n1 * n2 / 2.
        let var =
            if bigN <= 1. then 0.
            else (n1 * n2 / (bigN * (bigN - 1.))) * ((bigN * bigN * bigN - bigN - tieCorrection) / 12.)

        let sigma = sqrt var

        // Z-score using U1 (positive z → sample1 tends to be larger)
        let z =
            if sigma = 0. then 0.
            else
                let cc =
                    if not continuityCorrection then 0.
                    elif u1 > mu then -0.5
                    elif u1 < mu then  0.5
                    else 0.
                (u1 - mu + cc) / sigma

        let pLeft  = Distributions.Continuous.Normal.CDF 0. 1. z
        let pRight = 1. - pLeft
        let pTwo   = 2. * (min pLeft pRight)

        {
            U1          = u1
            U2          = u2
            Statistic   = z
            PValueLeft  = pLeft
            PValueRight = pRight
            PValue      = pTwo
        }
