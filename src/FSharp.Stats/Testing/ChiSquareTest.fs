namespace FSharp.Stats.Testing

open System
open FSharp.Stats


/// <summary>
///   Two-Sample (Goodness-of-fit) Chi-Square Test (Upper Tail)
/// </summary>
/// 
/// <remarks>
/// <para>
///   A chi-square test (also chi-squared or χ2  test) is any statistical
///   hypothesis test in which the sampling distribution of the test statistic
///   is a ChiSquareDistribution when
///   the null hypothesis is true, or any in which this is asymptotically true,
///   meaning that the sampling distribution (if the null hypothesis is true) 
///   can be made to approximate a chi-square distribution as closely as desired
///   by making the sample size large enough.</para>
/// <para>
///   The chi-square test is used whenever one would like to test whether the
///   actual data differs from a random distribution. </para>
///   
/// <para>
///   References:
///   <list type="bullet">
///     <item><description><a href="http://en.wikipedia.org/wiki/Chi-square_test">
///        Wikipedia, The Free Encyclopedia. Chi-Square Test. Available on:
///        http://en.wikipedia.org/wiki/Chi-square_test </a></description></item>
///   
///     <item><description><a href="http://www2.lv.psu.edu/jxm57/irp/chisquar.html">
///        J. S. McLaughlin. Chi-Square Test. Available on:
///        http://www2.lv.psu.edu/jxm57/irp/chisquar.html </a></description></item>
///   </list></para>
/// </remarks>
/// 
type ChiSquareTest =

    
    /// <summary>
    ///   Computes the Chi-Square goodness-of-fit test.
    ///   n data points -> degrees of freedom = n - 1
    /// </summary>
    static member compute (degreesOfFreedom:int) (expected:seq<float>) (observed:seq<float>) =
        let chi2 =
            Seq.zip observed expected
            |> Seq.fold (fun acc (obs,exp) -> 
                let d = obs - exp
                acc + (d * d) / exp) 0.0
        TestStatistics.createChiSquare chi2 (float degreesOfFreedom)

    /// <summary>
    ///   Computes the Chi-Square goodness-of-fit test with Yates's continuity correction.
    /// </summary>
    /// <remarks>
    ///   Yates's correction subtracts 0.5 from each |observed - expected| term before squaring.
    ///   It is recommended when the degrees of freedom equal 1 (two categories) and expected
    ///   cell counts are small.  For df > 1 or large samples the uncorrected <c>compute</c> is
    ///   preferable.
    ///
    ///   Reference: Yates, F. (1934). Contingency tables involving small numbers and the chi-squared
    ///   test. Supplement to the Journal of the Royal Statistical Society, 1(2), 217-235.
    /// </remarks>
    static member computeWithYates (degreesOfFreedom:int) (expected:seq<float>) (observed:seq<float>) =
        let chi2 =
            Seq.zip observed expected
            |> Seq.fold (fun acc (obs,exp) ->
                let diff = abs (obs - exp) - 0.5
                acc + (diff * diff) / exp) 0.0
        TestStatistics.createChiSquare chi2 (float degreesOfFreedom)

    /// <summary>
    ///   Computes the Chi-Square goodness-of-fit test with Williams's correction.
    /// </summary>
    /// <remarks>
    ///   Williams's correction divides the chi-square statistic by
    ///   q = 1 + (k^2 - 1) / (6 * n * k), where k is the number of categories and
    ///   n is the total observed count.  This provides a better approximation to the
    ///   chi-squared distribution when sample sizes are small.
    ///
    ///   Reference: Williams, D. A. (1976). Improved likelihood ratio tests for complete
    ///   contingency tables. Biometrika, 63(1), 33-37.
    /// </remarks>
    static member computeWithWilliams (degreesOfFreedom:int) (expected:seq<float>) (observed:seq<float>) =
        let observedArr = Seq.toArray observed
        let expectedArr = Seq.toArray expected
        let k = float observedArr.Length
        let n = Array.sum observedArr
        let q = 1.0 + (k * k - 1.0) / (6.0 * n * k)
        let chi2Raw =
            Array.zip observedArr expectedArr
            |> Array.fold (fun acc (obs,exp) ->
                let d = obs - exp
                acc + (d * d) / exp) 0.0
        let chi2 = chi2Raw / q
        TestStatistics.createChiSquare chi2 (float degreesOfFreedom)

    static member pearsonChiSquared (table:ContingencyTable<_,_>) =
        42.

    static member pearsonChiSquared (table:Contingency2x2<_,_>) =
        let apply o e =
          let diff = abs (o - e)
          diff * diff / e
        
        let a = table.A
        let b = table.B
        let c = table.C 
        let d = table.D 

        let N = float (a + b + c + d)
        let rowSums = [| float (a + b); float (c + d) |]
        let colSums = [| float (a + c); float (b + d) |]

        // compute Σ (O – E)²/E (with Yates)
        let chi2 =
          [| for i in 0..1 do
               for j in 0..1 do
                 let O = float table.[i,j]
                 let E = rowSums.[i] * colSums.[j] / N
                 yield apply O E |]
          |> Array.sum

        let df = 1  // (2–1)*(2–1)
        TestStatistics.createChiSquare chi2 (float df)

        
    /// Pearson χ² test with **Yates’s continuity correction** (only for 2×2).
    static member pearsonChiSquaredWithYates (table:Contingency2x2<_,_>) =
        let applyYates o e =
          let diff = abs (o - e) - 0.5
          diff * diff / e
        
        let a = table.A
        let b = table.B
        let c = table.C 
        let d = table.D 

        let N = float (a + b + c + d)
        let rowSums = [| float (a + b); float (c + d) |]
        let colSums = [| float (a + c); float (b + d) |]

        // compute Σ (O – E)²/E (with Yates)
        let chi2 =
          [| for i in 0..1 do
               for j in 0..1 do
                 let O = float table.[i,j]
                 let E = rowSums.[i] * colSums.[j] / N
                 yield applyYates O E |]
          |> Array.sum

        let df = 1  // (2–1)*(2–1)
        TestStatistics.createChiSquare chi2 (float df)
