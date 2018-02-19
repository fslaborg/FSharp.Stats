namespace FSharp.Stats.Testing


/// <summary>
///   Two-Sample (Goodness-of-fit) Chi-Square Test (Upper Tail)
/// </summary>
/// 
/// <remarks>
/// <para>
///   A chi-square test (also chi-squared or χ2  test) is any statistical
///   hypothesis test in which the sampling distribution of the test statistic
///   is a <see cref="ChiSquareDistribution">chi-square distribution</see> when
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
module ChiSquareTest =

    open FSharp.Stats

    /// Computes the Chi-Square test
    // n data points -> degrees of freedom = n - 1
    let compute (degreesOfFreedom:int) (expected:seq<float>) (observed:seq<float>) =
        let chi2 =
            Seq.zip observed expected
            |> Seq.fold (fun acc (obs,exp) -> 
                let d = obs - exp
                acc + (d * d) / exp) 0.0
        
        TestStatistics.createChiSquare chi2 (float degreesOfFreedom)

        

