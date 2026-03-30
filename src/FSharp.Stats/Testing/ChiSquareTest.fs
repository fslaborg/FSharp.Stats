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

    
    /// Computes the Chi-Square test
    /// n data points -&gt; degrees of freedom = n - 1 
    static member compute (degreesOfFreedom:int) (expected:seq<float>) (observed:seq<float>) =
        //let chechParams =
        //    if expected |> Seq.exists (fun x -> abs x < 5.) then printfn "Warning: A value less than 5 is present in expected values. Results may not be correct!"
        //    let sumEx = Seq.sum expected
        //    let sumOb = Seq.sum observed
        //    if Math.Round(sumEx,1) <> Math.Round(sumOb,1) then printfn "Warning: The sum of observed values does not match the sum of expected values. SumEx: %.3f SumOb: %.3f" sumEx sumOb
        let chi2 =
            Seq.zip observed expected
            |> Seq.fold (fun acc (obs,exp) -> 
                let d = obs - exp
                acc + (d * d) / exp) 0.0
        
        TestStatistics.createChiSquare chi2 (float degreesOfFreedom)

    /// <summary>
    ///   Pearson χ² test of independence for an r×c contingency table.
    /// </summary>
    /// <remarks>
    ///   For each cell (i,j) the expected count is E = rowTotal(i) × colTotal(j) / N.
    ///   The test statistic is χ² = Σ (O − E)² / E and has (r−1)(c−1) degrees of freedom.
    ///   Cells with expected count zero are skipped (they contribute 0 to χ²).
    /// </remarks>
    static member pearsonChiSquared (table:ContingencyTable<_,_>) =
        let numRows = table.NumRows
        let numCols = table.NumCols
        let N = float (Contingency.total table)
        if N = 0.0 then invalidArg "table" "ContingencyTable is empty (grand total is zero)"
        let rowTotals = table.RowKeys |> Array.map (fun r -> float (Contingency.rowTotal r table))
        let colTotals = table.ColKeys |> Array.map (fun c -> float (Contingency.columnTotal c table))
        let chi2 =
            [| for i in 0..numRows-1 do
                   for j in 0..numCols-1 do
                       let O = float (Contingency.getCount table.RowKeys.[i] table.ColKeys.[j] table)
                       let E = rowTotals.[i] * colTotals.[j] / N
                       if E > 0.0 then
                           yield (O - E) * (O - E) / E |]
            |> Array.sum
        let df = (numRows - 1) * (numCols - 1)
        TestStatistics.createChiSquare chi2 (float df)

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
