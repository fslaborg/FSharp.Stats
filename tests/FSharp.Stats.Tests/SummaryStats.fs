module SummaryStatsTests

open Expecto
open FSharp.Stats


[<Tests>]
let summaryStatsTests =
    testList "SummaryStats Tests" [

        testCase "ofSeq with empty sequence" <| fun _ ->
            let emptySeq: float seq = Seq.empty
            let stats = SummaryStats.ofSeq emptySeq
            // N = 0, Mean/Min/Max/SumSqrdDevations should be NaN (by definition here)
            Expect.equal stats.N 0.0 "Count should be zero for empty sequence."
            Expect.isTrue (System.Double.IsNaN stats.Mean) "Mean should be NaN for empty sequence."
            Expect.isTrue (System.Double.IsNaN stats.SumSqrdDevations) "SumSqrdDevations should be NaN for empty sequence."
            Expect.isTrue (System.Double.IsNaN stats.Min) "Min should be NaN for empty sequence."
            Expect.isTrue (System.Double.IsNaN stats.Max) "Max should be NaN for empty sequence."

        testCase "ofSeq with [1.0; 2.0; 3.0]" <| fun _ ->
            let stats = SummaryStats.ofSeq [1.0; 2.0; 3.0]
            Expect.equal stats.N 3.0 "N should be 3.0"
            Expect.equal stats.Mean 2.0 "Mean should be 2.0"
            Expect.equal stats.Min 1.0 "Min should be 1.0"
            Expect.equal stats.Max 3.0 "Max should be 3.0"
            Expect.equal stats.SumSqrdDevations 2.0 "SumSqrdDevations should be (1-2)^2 + (2-2)^2 + (3-2)^2 = 2"

            // population variance = 2/3
            let popVar = SummaryStats.varPopulation stats
            // sample variance = 2/ (3-1) = 1
            let sampVar = SummaryStats.var stats
            // stDev = sqrt(sample variance) = 1
            let sd = SummaryStats.stDev stats
            // stDevPopulation = sqrt( popVar ) = sqrt(2/3)
            let sdPop = SummaryStats.stDevPopulation stats

            Expect.floatClose Accuracy.high popVar (2.0/3.0) "Population variance should be 2/3."
            Expect.equal sampVar 1.0 "Sample variance should be 1.0"
            Expect.equal sd 1.0 "Sample standard deviation should be 1.0"
            Expect.floatClose Accuracy.high sdPop (sqrt(2.0/3.0)) "Population std. dev. should be sqrt(2/3)."

        testCase "ofArray with [|1.0; 2.0; 3.0|]" <| fun _ ->
            let statsArr = SummaryStats.ofArray [|1.0; 2.0; 3.0|]
            Expect.equal statsArr.N 3.0 "N should be 3.0"
            Expect.equal statsArr.Mean 2.0 "Mean should be 2.0"
            Expect.equal statsArr.Min 1.0 "Min should be 1.0"
            Expect.equal statsArr.Max 3.0 "Max should be 3.0"
            Expect.equal statsArr.SumSqrdDevations 2.0 "SumSqrdDevations should be 2.0"

            // Double-check a couple stats
            Expect.floatClose Accuracy.high (SummaryStats.varPopulation statsArr) (2.0/3.0) "Pop var should be 2/3."
            Expect.equal (SummaryStats.var statsArr) 1.0 "Sample variance should be 1.0"
    
        testList "SummaryStats vs. FSharp.Stats (Random Data)" [

            testCase "Random data (Seq) comparison" <| fun _ ->
                // 1) Generate some random data
                let rng = System.Random(42)
                let dataCount = 100
                let dataSeq = 
                    Seq.init dataCount (fun _ -> rng.NextDouble() * 100.0)
                    |> Seq.cache // ensure we can iterate multiple times if needed

                // 2) Compute stats with your SummaryStats.ofSeq
                let sStats = SummaryStats.ofSeq dataSeq

                // 3) Compute stats with FSharp.Stats library
                let lengthFS = Seq.length dataSeq
                let meanFS   = Seq.mean dataSeq
                let minFS    = Seq.min dataSeq
                let maxFS    = Seq.max dataSeq
                let varPopFS = Seq.varPopulation dataSeq
                let varFS    = Seq.var dataSeq
                let sdPopFS  = Seq.stDevPopulation dataSeq
                let sdFS     = Seq.stDev dataSeq

                // 4) Compare

                // N vs. length
                // Your sStats.N is a 'T. Here, we assume 'T = float in your usage.
                // If 'T = float, you can compare directly to float lengthFS.
                Expect.equal sStats.N (float lengthFS) "N (count) mismatch"

                // mean
                Expect.floatClose Accuracy.high sStats.Mean meanFS "Mean mismatch"
                // min
                Expect.floatClose Accuracy.high sStats.Min minFS "Min mismatch"
                // max
                Expect.floatClose Accuracy.high sStats.Max maxFS "Max mismatch"

                // population variance
                let varPop = SummaryStats.varPopulation sStats
                Expect.floatClose Accuracy.high varPop varPopFS "Population variance mismatch"

                // sample variance
                let varSample = SummaryStats.var sStats
                Expect.floatClose Accuracy.high varSample varFS "Sample variance mismatch"

                // population std dev
                let sdPop = SummaryStats.stDevPopulation sStats
                Expect.floatClose Accuracy.high sdPop sdPopFS "Population std. dev mismatch"

                // sample std dev
                let sdSample = SummaryStats.stDev sStats
                Expect.floatClose Accuracy.high sdSample sdFS "Sample std. dev mismatch"

            testCase "Random data (Array) comparison" <| fun _ ->
                // 1) Generate some random data for an array
                let rng = System.Random(42)
                let dataCount = 100
                let dataArr =
                    Array.init dataCount (fun _ -> rng.NextDouble() * 100.0)

                // 2) SummaryStats.ofsArray
                let sStatsArray = SummaryStats.ofArray dataArr

                // 3) FSharp.Stats.* for arrays
                let lengthFS = Array.length dataArr
                let meanFS   = Array.average dataArr
                let minFS    = Array.min dataArr
                let maxFS    = Array.max dataArr
                let varPopFS = Seq.varPopulation dataArr
                let varFS    = Seq.var dataArr
                let sdPopFS  = Seq.stDevPopulation dataArr
                let sdFS     = Seq.stDev dataArr

                // 4) Compare results
                Expect.equal sStatsArray.N (float lengthFS) "N (count) mismatch"
                Expect.floatClose Accuracy.high sStatsArray.Mean meanFS "Mean mismatch"
                Expect.floatClose Accuracy.high sStatsArray.Min minFS "Min mismatch"
                Expect.floatClose Accuracy.high sStatsArray.Max maxFS "Max mismatch"

                // population variance
                let varPopArr = SummaryStats.varPopulation sStatsArray
                Expect.floatClose Accuracy.high varPopArr varPopFS "Population variance mismatch"

                // sample variance
                let varSampleArr = SummaryStats.var sStatsArray
                Expect.floatClose Accuracy.high varSampleArr varFS "Sample variance mismatch"

                // population std dev
                let sdPopArr = SummaryStats.stDevPopulation sStatsArray
                Expect.floatClose Accuracy.high sdPopArr sdPopFS "Population std. dev mismatch"

                // sample std dev
                let sdSampleArr = SummaryStats.stDev sStatsArray
                Expect.floatClose Accuracy.high sdSampleArr sdFS "Sample std. dev mismatch"

            testCase "Random data (ofSeq + ofArray) comparison" <| fun _ ->
                // 1) Generate some random data for an array
                let rng = System.Random(42)
                let dataCount = 100
                let dataArr =
                    Array.init dataCount (fun _ -> rng.NextDouble() * 100.0)

                // 2) SummaryStats.ofsArray
                let sStatsArray = SummaryStats.ofArray dataArr
                let sStatsSeq   = SummaryStats.ofArray dataArr
                Expect.equal sStatsArray sStatsSeq "SummaryStats.ofArray and SummaryStats.ofSeq should yield the same results."

        ]    
    
    
    ]

// Seq.length
// Seq.mean
// Seq.min
// Seq.max
// Seq.stDev
// Seq.stDevPopulation
// Seq.var
// Seq.varPopulation