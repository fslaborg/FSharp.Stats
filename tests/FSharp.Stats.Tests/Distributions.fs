module DistributionsTests 
open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open Distance.OneDimensional

// Defining an accuracy appropriate for testing random sampling and inference
let fittingAccuracy : Accuracy = {absolute= 0.1 ;relative= 0.1}

[<Tests>]
let distanceFunctionsTests =
    // Tests taken directly from the source implementation in scipy
    //
    // WassersteinDistance: https://github.com/scipy/scipy/blob/master/scipy/stats/stats.py#L6986
    // EnergyDistance: https://github.com/scipy/scipy/blob/master/scipy/stats/stats.py#L7068
    testList "Distributions.Distance" [
        testCase "test_WassersteinDistance" <| fun () ->
            let xs = [|3.4; 3.9; 7.5; 7.8|]
            let ys = [|4.5; 1.4|]
            let xWeights = [|1.4; 0.9; 3.1; 7.2|]
            let yWeights = [|3.2; 3.5|]
            let distance = wassersteinDistanceWeighted xs ys xWeights yWeights
            Expect.floatClose Accuracy.high distance 4.0781331438047861 "Should be equal (double precision)"
        testCase "test_EnergyDistance" <| fun () ->
            let xs =        [|0.7; 7.4; 2.4; 6.8|]
            let ys =        [|1.4; 8. |]
            let xWeights =  [|2.1; 4.2; 7.4; 8. |]
            let yWeights =  [|7.6; 8.8|]
            let distance = energyDistanceWeighted xs ys xWeights yWeights
            Expect.floatClose Accuracy.high distance 0.88003340976158217 "Should be equal (double precision)"
    ]

[<Tests>]
let chiSquaredTests =
    testList "ChiSquaredTests" [
        testList "CheckParam" [
            // edge cases:
            testCase "CheckParam10" <| fun () ->
                let testCase = Continuous.ChiSquared.CheckParam 10.
                Expect.isTrue (testCase = ()) "Should be unit"
            testCase "CheckParam0" <| fun () ->
                let testCase = Continuous.ChiSquared.CheckParam 0.
                Expect.isTrue (testCase = ()) "Should be unit"
            testCase "CheckParamInfinity" <| fun () ->
                let testCase = Continuous.ChiSquared.CheckParam infinity
                Expect.isTrue (testCase = ()) "Should be unit"
            testCase "CheckParam-1" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.CheckParam -1.) "Should throw"
            testCase "CheckParam-infinity" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.CheckParam -infinity) "Should throw"
            testCase "CheckParamNan" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.CheckParam nan) "Should throw"
        ]

        testList "Distributions.ChiSquared" [
            // cases for mean, variance, and standard deviation based on https://web.archive.org/web/20211109131558/https://www.statlect.com/probability-distributions/chi-square-distribution
            testCase "Mean10" <| fun () ->
                let testCase = Continuous.ChiSquared.Mean 10.
                Expect.floatClose Accuracy.veryHigh testCase 10. "Should be equal"
            // edge cases:
            testCase "Mean0" <| fun () ->
                let testCase = Continuous.ChiSquared.Mean 0.
                Expect.floatClose Accuracy.veryHigh testCase 0. "Should be equal"
            testCase "MeanInfinity" <| fun () ->
                let testCase = Continuous.ChiSquared.Mean infinity
                Expect.isTrue (testCase = infinity) "Should be equal"
            testCase "Mean-1" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Mean -1. |> ignore) "Should throw"
            testCase "Mean-Infinity" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Mean -infinity |> ignore) "Should throw"
            testCase "MeanNan" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Mean nan |> ignore) "Should throw"

            testCase "Variance10" <| fun () ->
                let testCase = Continuous.ChiSquared.Variance 10.
                Expect.floatClose Accuracy.veryHigh testCase 20. "Should be equal"
            // edge cases:
            testCase "Variance0" <| fun () ->
                let testCase = Continuous.ChiSquared.Variance 0.
                Expect.floatClose Accuracy.veryHigh testCase 0. "Should be equal"
            testCase "VarianceInfinity" <| fun () ->
                let testCase = Continuous.ChiSquared.Variance infinity
                Expect.isTrue (testCase = infinity) "Should be equal"
            testCase "Variance-1" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Variance -1. |> ignore) "Should throw"
            testCase "Variance-Infinity" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Variance -infinity |> ignore) "Should throw"
            testCase "VarianceNan" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Variance nan |> ignore) "Should throw"

            testCase "StandardDeviation10" <| fun () ->
                let testCase = Continuous.ChiSquared.StandardDeviation 8.
                Expect.floatClose Accuracy.veryHigh testCase 4. "Should be equal"
            // edge cases:
            testCase "StandardDeviation0" <| fun () ->
                let testCase = Continuous.ChiSquared.StandardDeviation 0.
                Expect.floatClose Accuracy.veryHigh testCase 0. "Should be equal"
            testCase "StandardDeviationInfinity" <| fun () ->
                let testCase = Continuous.ChiSquared.StandardDeviation infinity
                Expect.isTrue (testCase = infinity) "Should be equal"
            testCase "StandardDeviation-1" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.StandardDeviation -1. |> ignore) "Should throw"
            testCase "StandardDeviation-Infinity" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.StandardDeviation -infinity |> ignore) "Should throw"
            testCase "StandardDeviationNan" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.StandardDeviation nan |> ignore) "Should throw"

            // edge cases:
            testCase "PDF.testCaseDof0X4,7" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 0. 4.7
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseDof0X1" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 0. 1.
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseDof0X0" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 0. 0.
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseDof0XInfinity" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 0. infinity
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseDof0X-1" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 0. -1.
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseDof0X-infinity" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 0. -infinity
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseDof0XNan" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 0. nan
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseX-1" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 2. -1.
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseX0" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 2. 0.
                Expect.floatClose Accuracy.low testCase 0.5 "Should be equal"
            testCase "PDF.testCaseX-infinity" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 2. -infinity
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseXInfinity" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 2. infinity
                Expect.floatClose Accuracy.low testCase 0. "Should be equal"
            testCase "PDF.testCaseXNan" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 2. nan
                Expect.isTrue (Double.IsNaN testCase) "Should be equal"
            //TestCases from https://www.analyticscalculators.com/calculator.aspx?id=63
            testCase "PDF.testCase1" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 2. 4.7
                Expect.floatClose Accuracy.low testCase 0.04768458 "Should be equal"
            testCase "PDF.testCase2" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 20.0 4.7
                Expect.floatClose Accuracy.low testCase 0.00028723 "Should be equal"
            testCase "PDF.testCase3" <| fun () ->
                let testCase = Continuous.ChiSquared.PDF 100. 80.
                Expect.floatClose Accuracy.low testCase 0.01106689 "Should be equal"

            //// edge cases:
            //testCase "PDFLnInfinityDof" <| fun () ->
            //    let testCase = Continuous.ChiSquared.PDFLn infinity 1.
            //    Expect.isTrue (testCase = -infinity) "Should be equal"
            //testCase "PDFLnInfinityX" <| fun () ->
            //    let testCase = Continuous.ChiSquared.PDFLn 1. infinity
            //    Expect.isTrue (testCase = -infinity) "Should be equal"

            // TO DO: TestCases for other edge cases. Not done as long as the function of PDFLn remains unclear (s. https://github.com/fslaborg/FSharp.Stats/issues/209)

            // edge cases:
            testCase "CDF.testCaseDof0X1" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 0. 1.)
                Expect.isTrue (testCase = 0.) "Should be equal"
            testCase "CDF.testCaseDof0X10" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 0. 10.)
                Expect.isTrue (testCase = 0.) "Should be equal"
            testCase "CDF.testCaseDof0XInfinity" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 0. infinity)
                Expect.isTrue (testCase = 0.) "Should be equal"
            testCase "CDF.testCaseDof0XNan" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 0. nan)
                Expect.isTrue (testCase = 1.) "Should be equal"
            testCase "CDF.testCaseDof0X-infinity" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 0. -infinity)
                Expect.isTrue (testCase = 1.) "Should be equal"
            testCase "CDF.testCaseDof0X0" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 0. 0.)
                Expect.isTrue (testCase = 1.) "Should be equal"
            testCase "CDF.testCaseDof1X0" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 1. 0.)
                Expect.floatClose Accuracy.veryHigh testCase 1. "Should be equal"
            testCase "CDF.testCaseDof1XInfinity" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 1. infinity)
                Expect.floatClose Accuracy.veryHigh testCase 0. "Should be equal"
            testCase "CDF.testCaseDof1X-infinity" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 1. -infinity)
                Expect.isTrue (isNan testCase) "Should be NaN"
            testCase "CDF.testCaseDof1XNan" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 1. nan)
                Expect.isTrue (isNan testCase) "Should be NaN"
            //TestCases from Williams RBG, Introduction to Statistics for Geographers and Earth Scientist, 1984, DOI 10.1007/978-1-349-06815-9 p 333
            testCase "CDF.testCase1" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 20. 12.443)
                Expect.isTrue (Math.Round(testCase,3) = 0.900) "Should be equal"
            testCase "CDF.testCase12" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 3. 1.424)
                Expect.isTrue (Math.Round(testCase,3) = 0.700) "Should be equal"
            testCase "CDF.testCase13" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 100. 67.327)
                Expect.isTrue (Math.Round(testCase,3) = 0.995) "Should be equal"
            testCase "CDF.testCase14" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 100. 129.561)
                Expect.isTrue (Math.Round(testCase,3) = 0.025) "Should be equal"

            testCase "Support-1" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Support -1. |> ignore) "Should throw"
            testCase "Support-infinity" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Support -infinity |> ignore) "Should throw"
            testCase "SupportNan" <| fun () ->
                Expect.throws (fun () -> Continuous.ChiSquared.Support nan |> ignore) "Should throw"
            //testCase "Support0" <| fun () ->
            //    let testCase = Continuous.ChiSquared.Support 0.
            //    Expect.isTrue (testCase = (0., infinity)) "Should be equal"
            //testCase "Support1" <| fun () ->
            //    let testCase = Continuous.ChiSquared.Support 1.
            //    Expect.isTrue (testCase = (0., infinity)) "Should be equal"
            //testCase "SupportInfinity" <| fun () ->
            //    let testCase = Continuous.ChiSquared.Support infinity
            //    Expect.isTrue (testCase = (0., infinity)) "Should be equal"

            // is based on the functions from before but nevertheless should be tested, too
            testCase "chiSquared-1" <| fun () ->
                let testCase = ContinuousDistribution.chiSquared -1.
                Expect.throws (fun () -> testCase.Mean |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.Variance |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.StandardDeviation |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 0. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 10. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF -infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF -1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF nan |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 0. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 10. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF -infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF -1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF nan |> ignore) "Should throw"
            testCase "chiSquared-infinity" <| fun () ->
                let testCase = ContinuousDistribution.chiSquared -infinity
                Expect.throws (fun () -> testCase.Mean |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.Variance |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.StandardDeviation |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 0. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 10. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF -infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF -1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF nan |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 0. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 10. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF -infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF -1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF nan |> ignore) "Should throw"
            testCase "chiSquaredNan" <| fun () ->
                let testCase = ContinuousDistribution.chiSquared nan
                Expect.throws (fun () -> testCase.Mean |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.Variance |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.StandardDeviation |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 0. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF 10. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF -infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF -1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.CDF nan |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 0. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF 10. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF -infinity |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF -1. |> ignore) "Should throw"
                Expect.throws (fun () -> testCase.PDF nan |> ignore) "Should throw"
            testCase "chiSquared0" <| fun () ->
                let testCase = ContinuousDistribution.chiSquared 0.
                Expect.floatClose Accuracy.veryHigh testCase.Mean 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh testCase.Variance 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh testCase.StandardDeviation 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF 0.) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF 1.) 1. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF 10.) 1. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF infinity) 1. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF -1.) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF -infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF nan) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF 0.) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF 1.) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF 10.) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -1.) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF nan) 0. "Should be equal"
            testCase "chiSquared1" <| fun () ->
                let testCase = ContinuousDistribution.chiSquared 1.
                Expect.floatClose Accuracy.veryHigh testCase.Mean 1. "Should be equal"
                Expect.floatClose Accuracy.veryHigh testCase.Variance 2. "Should be equal"
                Expect.floatClose Accuracy.veryHigh testCase.StandardDeviation (sqrt 2.) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF 0.) 0. "Should be equal"
                Expect.floatClose Accuracy.medium (testCase.CDF 1.) 0.682689 "Should be equal"
                Expect.floatClose Accuracy.low (testCase.CDF 10.) 0.998 "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF infinity) 1. "Should be equal"
                Expect.isTrue (testCase.CDF -1. |> isNan) "Should be equal"
                Expect.isTrue (testCase.CDF -infinity |> isNan) "Should be equal"
                Expect.isTrue (testCase.CDF nan |> isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 0. = infinity) "Should be equal"
                Expect.floatClose Accuracy.medium (testCase.PDF 1.) 0.24197 "Should be equal"
                Expect.floatClose Accuracy.low (testCase.PDF 10.) 0.00085 "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -1.) 0. "Should be equal"
                Expect.isTrue (isNan <| testCase.PDF nan) "Should be equal"
            testCase "chiSquaredInfinity" <| fun () ->
                let testCase = ContinuousDistribution.chiSquared infinity
                Expect.isTrue (testCase.Mean = infinity) "Should be equal"
                Expect.isTrue (testCase.Variance = infinity) "Should be equal"
                Expect.isTrue (testCase.StandardDeviation = infinity) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF 0.) 0. "Should be equal"
                Expect.isTrue (testCase.CDF 1. |> isNan) "Should be equal"
                Expect.isTrue (testCase.CDF 10. |> isNan) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF infinity) 1. "Should be equal"
                Expect.isTrue (testCase.CDF -1. |> isNan) "Should be equal"
                Expect.isTrue (testCase.CDF -infinity |> isNan) "Should be equal"
                Expect.isTrue (testCase.CDF nan |> isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 0. |> isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 1. |> isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 10. |> isNan) "Should be equal"
                Expect.isTrue (testCase.PDF infinity |> isNan) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -1.) 0. "Should be equal"
                Expect.isTrue (isNan <| testCase.PDF nan) "Should be equal"
        ]
    ]

//Test ommitted due to long runtime of CodeCov
//[<Tests>]
//let studentizedRangeTests =
//    //TestCases from critical q value tables from: Lawal B, Applied Statistical Methods in Agriculture, Health and Life Sciences, DOI 10.1007/978-3-319-05555-8, 2014
//    testList "Distributions.studentizedRange" [
//        testCase "CDF.testCase_0.95_1" <| fun () ->
//            let testCase = 1. - (Continuous.StudentizedRange.CDF 3.46 2. 6. 1. None false)
//            Expect.isTrue (Math.Round(testCase,4) = 0.05) "Should be equal"
//        testCase "CDF.testCase_0.95_2" <| fun () ->
//            let testCase = 1. - (Continuous.StudentizedRange.CDF 2.83 2. 60. 1. None false)
//            Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"
//        testCase "CDF.testCase_0.95_3" <| fun () ->
//            let testCase = 1. - (Continuous.StudentizedRange.CDF 7.59 20. 6. 1. None false)
//            Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"
//        testCase "CDF.testCase_0.95_4" <| fun () ->
//            let testCase = 1. - (Continuous.StudentizedRange.CDF 5.24 20. 60. 1. None false)
//            Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"            
//    //TestCases from R ptukey(q, 4, 36, nranges = 1, lower.tail = TRUE, log.p = FALSE)
//    //https://keisan.casio.com/exec/system/15184848911695
//        testCase "CDF.testCase_r1" <| fun () ->
//            let testCase = Continuous.StudentizedRange.CDF 3. 4. 36. 1. None false
//            Expect.floatClose Accuracy.medium testCase 0.8342594 "Should be equal"
//        testCase "CDF.testCase_r2" <| fun () ->
//            let testCase = Continuous.StudentizedRange.CDF 6. 4. 36. 1. None false
//            Expect.floatClose Accuracy.medium testCase 0.9991826 "Should be equal"
//        testCase "CDF.testCase_r3" <| fun () ->
//            let testCase = Continuous.StudentizedRange.CDF 9. 4. 36. 1. None false
//            Expect.floatClose Accuracy.medium testCase 0.9999987 "Should be equal"
//        testCase "CDF.testCase_r4" <| fun () ->
//            let testCase = Continuous.StudentizedRange.CDF 11. 4. 36. 1. None false
//            Expect.floatClose Accuracy.medium testCase 1. "Should be equal"         
//    ]

[<Tests>]
let chiTests =
    // TestCases from R: library(chi) function: dchi(x, dof)
    testList "Distributions.Continuous.Chi" [
        testCase "PDF.testCase_1" <| fun () ->
            let testCase = Continuous.Chi.PDF 1. 1.
            Expect.floatClose Accuracy.medium 0.4839414 testCase "Should be equal" 
        testCase "PDF.testCase_2" <| fun () ->
            let testCase = Continuous.Chi.PDF 1. 8.
            Expect.floatClose Accuracy.veryHigh  1.010454e-14 testCase "Should be equal" 
        testCase "PDF.testCase_3" <| fun () ->
            let testCase = Continuous.Chi.PDF 8. 1.
            Expect.floatClose Accuracy.medium 0.01263606 testCase "Should be equal" 
        testCase "PDF.testCase_4" <| fun () ->
            let testCase = Continuous.Chi.PDF 8. 8.
            Expect.floatClose Accuracy.veryHigh 5.533058e-10 testCase "Should be equal" 
        // TestCases from R: library(chi) function: pchi(x, dof)
        testCase "CDF.testCase_1" <| fun () ->
            let testCase = Continuous.Chi.CDF 1. 1.
            Expect.floatClose Accuracy.medium testCase 0.6826895 "Should be equal"
        testCase "CDF.testCase_2" <| fun () ->
            let testCase = Continuous.Chi.CDF 12. 5.
            Expect.floatClose Accuracy.medium testCase 0.9851771 "Should be equal"
        testCase "CDF.testCase_3" <| fun () ->
            let testCase = Continuous.Chi.CDF 8. 1.
            Expect.floatClose Accuracy.medium testCase 0.001751623 "Should be equal"
        testCase "CDF.testCase_4" <| fun () ->
            let testCase = Continuous.Chi.CDF 80. 8.
            Expect.floatClose Accuracy.medium testCase 0.09560282 "Should be equal"         
    ]

let multivariateNormalTests =
    let mvn = ContinuousDistribution.multivariateNormal (vector [0.;0.;0.;0.;0.]) (Matrix.identity 5)
    let pdfs=
        [|
            [0.537667139546100;3.578396939725760;-0.124144348216312;0.488893770311789;-1.068870458168032]
            [0.318765239858981;0.725404224946106;0.671497133608080;0.293871467096658;0.325190539456195]
            [-0.433592022305684;0.714742903826096;0.717238651328838;0.888395631757642;1.370298540095228]
        |]
        |> Array.map (fun v -> 
            mvn.PDF (vector v)
            )
    // TestCases from Matlab: 
    (*
    mu = zeros(1,5);
    Sigma = eye(5);
    rng('default')  % For reproducibility
    X = mvnrnd(mu,Sigma,8)
    y = mvnpdf(X)
    *)
    testList "Distributions.multivariateNormal" [
        testCase "PDF.testCase_1" <| fun () ->
            Expect.floatClose Accuracy.veryHigh 0.000007209186311 pdfs.[0] "Should be equal" 
        testCase "PDF.testCase_2" <| fun () ->
            let testCase = Continuous.Chi.PDF 1. 8.
            Expect.floatClose Accuracy.veryHigh  0.005352921388597 pdfs.[1] "Should be equal" 
        testCase "PDF.testCase_3" <| fun () ->
            let testCase = Continuous.Chi.PDF 8. 1.
            Expect.floatClose Accuracy.veryHigh 0.001451989663439 pdfs.[2] "Should be equal"        
    ]

[<Tests>]
let hypergeometricTests =   

    let hypergeoDistribution_basicCase = Distributions.DiscreteDistribution.hypergeometric 50 40 5
    let hypergeoDistribution_K_equal_n = Distributions.DiscreteDistribution.hypergeometric 50 20 20
    let hypergeoDistribution_max_K = Distributions.DiscreteDistribution.hypergeometric 50 50 20
    let hypergeoDistribution_max_n = Distributions.DiscreteDistribution.hypergeometric 50 20 50
    let hypergeoDistribution_max_K_n = Distributions.DiscreteDistribution.hypergeometric 50 50 50
    // 2022-06-23
    // https://hypergeon.wikipedia.org/wiki/Hypergeometric_distribution
    // N is population size,
    // K is the number of success states in the population,
    // n is the number of draws,
    // k is the number of observed successes
    // N ∈ {0,1,2,...}
    // K ∈ {0,1,2,...,N}
    // n ∈ {0,1,2,...,N}
    testList "Distributions.Discrete.Hypergeometric" [
        test "hypergeoCheckParam" {
            // Low N edge cases are difficult to test separately, as K and n MUST be smaller than N, but MUST also be bigger than 0  
            let N_isZero = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 0 1 1
            let N_isNegative = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam -2 1 1
            let N_isPositive = Distributions.Discrete.Hypergeometric.CheckParam 2 1 1
            //
            let K_isZero = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 0 1
            let K_isNegative = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 -2 1
            let K_positiveBiggerN = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 3 1
            let K_positiveEqualN = Distributions.Discrete.Hypergeometric.CheckParam 2 2 1
            let K_positiveSmallerN = Distributions.Discrete.Hypergeometric.CheckParam 2 1 1
            //
            let n_isZero = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 1 0
            let n_isNegative = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 1 -2
            let n_positiveBiggerN = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 1 3
            let n_positiveEqualN = Distributions.Discrete.Hypergeometric.CheckParam 2 1 2
            let n_positiveSmallerN = Distributions.Discrete.Hypergeometric.CheckParam 2 1 1
            Expect.throws N_isZero "N_isZero"
            Expect.throws N_isNegative "N_isNegative"
            Expect.equal N_isPositive () "N_isPositive"
            //
            Expect.throws K_isZero "K_isZero"
            Expect.throws K_isNegative "K_isNegative"
            Expect.throws K_positiveBiggerN "K_positiveBiggerN"
            Expect.equal K_positiveEqualN () "K_positiveEqualN"
            Expect.equal K_positiveSmallerN () "K_positiveSmallerN"
            //
            Expect.throws n_isZero "n_isZero"
            Expect.throws n_isNegative "n_isNegative"
            Expect.throws n_positiveBiggerN "n_positiveBiggerN"
            Expect.equal n_positiveEqualN () "n_positiveEqualN"
            Expect.equal n_positiveSmallerN () "n_positiveSmallerN"
        }
        test "hypergeoCheckParam_k" {
            let k_isNegative = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 2 -2
            let k_isPositive = Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 2 1
            let k_isPositive_allEqual = Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 2 2
            let k_isPositiveBiggerN = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 2 5
            let k_isPositiveBiggerK = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 3 3
            let k_isPositiveBigger_n = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam_k 4 3 2 3
            Expect.throws k_isNegative "k_isNegative"
            Expect.equal k_isPositive () "k_isPositive; should not throw."
            Expect.equal k_isPositive_allEqual () "k_isPositive_allEqual; should not throw."
            Expect.throws k_isPositiveBiggerN "k_isPositiveBiggerN"
            Expect.throws k_isPositiveBiggerK "k_isPositiveBiggerK"
            Expect.throws k_isPositiveBigger_n "k_isPositiveBigger_n"
        }
        // 2022-06-23
        // https://www.emathhelp.net/calculators/probability-statistics/hypergeometric-distribution-calculator/?pn=50&pk=40&sn=5&sk=5
        test "Mean" {
            Expect.floatClose Accuracy.high hypergeoDistribution_basicCase.Mean 4.0 "hyperDistribution_basicCase"
            Expect.floatClose Accuracy.high hypergeoDistribution_K_equal_n.Mean 8.0 "hyperDistribution_K_equal_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K.Mean 20.0 "hyperDistribution_max_K"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_n.Mean 20.0 "hyperDistribution_max_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K_n.Mean 50.0 "hyperDistribution_max_K_n"
        }
        // 2022-06-23
        // https://www.emathhelp.net/calculators/probability-statistics/hypergeometric-distribution-calculator/?pn=50&pk=40&sn=5&sk=5
        test "Variance" {
            Expect.floatClose Accuracy.high hypergeoDistribution_basicCase.Variance 0.73469387755102 "hyperDistribution_basicCase"
            Expect.floatClose Accuracy.high hypergeoDistribution_K_equal_n.Variance 2.938775510204082 "hyperDistribution_K_equal_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K.Variance 0.0 "hyperDistribution_max_K"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_n.Variance 0.0 "hyperDistribution_max_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K_n.Variance 0.0 "hyperDistribution_max_K_n"
        }
        // 2022-06-23
        // https://www.emathhelp.net/calculators/probability-statistics/hypergeometric-distribution-calculator/?pn=50&pk=40&sn=5&sk=5
        test "StandardDeviation" {
            Expect.floatClose Accuracy.high hypergeoDistribution_basicCase.StandardDeviation 0.857142857142857 "hyperDistribution_basicCase"
            Expect.floatClose Accuracy.high hypergeoDistribution_K_equal_n.StandardDeviation 1.714285714285714 "hyperDistribution_K_equal_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K.StandardDeviation 0.0 "hyperDistribution_max_K"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_n.StandardDeviation 0.0 "hyperDistribution_max_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K_n.StandardDeviation 0.0 "hyperDistribution_max_K_n"
        }

        // 2022-06-23
        // https://www.omnicalculator.com/statistics/hypergeometric-distribution
        test "PMF" {
            // test k = 0; Accuracy.medium, because online calculator has not enough decimal places.
            Expect.floatClose Accuracy.medium (hypergeoDistribution_basicCase.PMF 0) 0.00011894 "hyperDistribution_basicCase k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_K_equal_n.PMF 0) 0.0000006375 "hyperDistribution_K_equal_n k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K.PMF 0) 0. "hyperDistribution_max_K k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_n.PMF 0) 0. "hyperDistribution_max_n k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K_n.PMF 0) 0. "hyperDistribution_max_K_n k=0"
            // test any k 
            Expect.floatClose Accuracy.medium (hypergeoDistribution_basicCase.PMF 3) 0.20984 "hyperDistribution_basicCase k=3"
            // Accuracy.low, because online calculator has not enough decimal places.
            Expect.floatClose Accuracy.low (hypergeoDistribution_K_equal_n.PMF 6) 0.1196 "hyperDistribution_K_equal_n k=6"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K.PMF 10) 0. "hyperDistribution_max_K k=10"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_n.PMF 13) 0. "hyperDistribution_max_n k=44"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K_n.PMF 50) 1.0 "hyperDistribution_max_K_n k=50"
        }
        // 2022-06-23
        // https://www.omnicalculator.com/statistics/hypergeometric-distribution
        test "CDF" {
            Expect.floatClose Accuracy.medium (hypergeoDistribution_basicCase.CDF 3)0.2581 "hyperDistribution_basicCase k=3"
            // Accuracy.low, because online calculator has not enough decimal places.
            Expect.floatClose Accuracy.low (hypergeoDistribution_K_equal_n.CDF 7) 0.3858 "hyperDistribution_K_equal_n k=7"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K.CDF 14) 0.0 "hyperDistribution_max_K k=14"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_n.CDF 3) 0.0 "hyperDistribution_max_n k=3"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K_n.CDF 3) 0.0 "hyperDistribution_max_K_n k=3"
        }
        //// No idea what this is meant for, but its Syntax differs from Bernoulli.Support
        //test "Support" {
        //    /// 40 20 5 do not matter as long as they don't fail "hypergeoCheckParam"
        //    let s = Distributions.Discrete.Hypergeometric.Support 40 20 5
        //    Expect.equal s (0., infinity) ""
        //}
        test "SampleUnchecked" {
            let generateALL = Distributions.Discrete.Hypergeometric.Sample 40 20 40
            let generate50 = Array.init 50 (fun x -> Distributions.Discrete.Hypergeometric.Sample 40 20 10)
            let numbersAreBetween_1_K = generate50 |> Array.forall (fun x -> x > 0 && x < 20)
            // If N = n then k = K
            Expect.equal generateALL 20 "generateALL"
            Expect.isTrue numbersAreBetween_1_K "numbersAreBetween_1_K"
            }
        ]

let exponentialTests =
    // references is R V. 2022.02.3 Build 492
    // PDF is used with expPDF <- dexp(3,0.59)
    // CDF is created with expCDF <- pexp(3, 0.59)
    testList "Distributions.Continuous.Exponential" [

        let createExpDistCDF  lambda x = FSharp.Stats.Distributions.Continuous.Exponential.CDF lambda x    
        let createExpDistPDF  lambda x = Distributions.Continuous.Exponential.PDF lambda x
        
        testCase "exp check param" <| fun () -> 
            Expect.throws (fun () -> (Distributions.Continuous.Exponential.CheckParam 0. )) "Should fail when lamda  =  0.0"
            Expect.throws (fun () -> (Distributions.Continuous.Exponential.CheckParam -3. )) "Should fail when lamda  < 0."
            Expect.throws (fun () -> (Distributions.Continuous.Exponential.CheckParam -infinity )) "Should fail when lamda < 0.0  "  
        
        testCase "Exponential Lambda regular " <| fun () -> 
            Expect.floatClose Accuracy.low (createExpDistCDF 0.3 5.) 0.776869 "CDF should be equal"
            Expect.floatClose Accuracy.low (createExpDistPDF 0.3 5.) 0.066939 "PDF should be equal"
        //testCase "Exponential Lambda= NaN CDF " <| fun () -> 
        //    let lamdaNaN = createExpDistCDF nan 5.
        //    Expect.isTrue (nan.Equals (lamdaNaN)) "Distribution can't be initialized with lambda = nan "

        testCase "Exponential Lambda= infinity CDF " <| fun () -> 
        
            Expect.floatClose Accuracy.low (createExpDistCDF infinity 5.) 1. "CDF should be 1 with lamda = infinity"

            Expect.isTrue (nan.Equals (createExpDistPDF infinity 5.)) "PDF can't be initialized with lambda = infinity "
 
        testCase "Exponential Lambda= NaN " <| fun () -> 

            Expect.isTrue (nan.Equals (createExpDistCDF nan 5.)) "CDF can't be initialized with lambda = nan "

            Expect.isTrue (nan.Equals (createExpDistPDF nan 5.)) "PDF can't be initialized with lambda = nan "


        testCase "Exponential x regular" <| fun () -> 
            let regularx = createExpDistCDF 0.59 3.
            Expect.floatClose Accuracy.low regularx 0.829667011174591 "CDF should be equal"
            let regularx' = createExpDistPDF 0.59 3. 
            Expect.floatClose Accuracy.low regularx' 0.100496463406992 "PDF should be equal"

        testCase "Exponential x NaN  " <| fun () -> 
            Expect.isTrue (nan.Equals (createExpDistCDF 0.5 nan)) "CDF can't be initialized with x = nan "
            Expect.floatClose Accuracy.low (createExpDistPDF 0.5 nan) 0. "PDF should be 0 when x = nan"

        testCase "Exponential x infinity" <| fun () -> 
            Expect.floatClose Accuracy.low (createExpDistCDF 0.5 infinity) 1. "CDF should be 1 when x = infinity"
            Expect.floatClose Accuracy.low (createExpDistPDF 0.5 infinity) 0. "PDF should be 0 when x = infinity"

        testCase "Exponential Mean" <| fun () -> 
            Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.Mean 0.5) 2.0 "Mean should be equal"
            Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.Mean 10.) 0.1 "Mean should be equal"
            Expect.isTrue (nan.Equals (Distributions.Continuous.Exponential.Mean nan)) "Mean can't be calculated with lambda = nan "
            // Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.Mean infinity) 0. "Mean should be 0 when lambda = infinity"
        testCase "Exponential Standard Deviation" <| fun () -> 
            Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.Mean 0.5) 2.0 "StDev should be equal"
            Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.Mean 10.) 0.1 "StDev should be equal"
            Expect.isTrue (nan.Equals (Distributions.Continuous.Exponential.Mean nan)) "StDev can't be calculated with lambda = nan "
            //Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.StandardDeviation infinity) 0. "StDev should be 0 when lambda = infinity"

        testCase "Exponential Variance" <| fun () ->
             Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.Variance 0.5) 4.0 "Variance should be equal"
             Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.Variance 10.) 0.01 "Variance should be equal"
             Expect.isTrue (nan.Equals (Distributions.Continuous.Exponential.Variance nan)) "Variance can't be calculated with lambda = nan "
             //Expect.floatClose Accuracy.low (Distributions.Continuous.Exponential.StandardDeviation infinity) 0. "StDev should be 0 when lambda = infinity"

            ]

[<Tests>]
let bernoulliTests =

    let test_basicNumber = 0.42

    let bernoulliDistribution_basicCase = Distributions.DiscreteDistribution.bernoulli test_basicNumber
    let bernoulliDistribution_nan = Distributions.DiscreteDistribution.bernoulli nan
    let bernoulliDistribution_zero = Distributions.DiscreteDistribution.bernoulli 0.0
    let bernoulliDistribution_one = Distributions.DiscreteDistribution.bernoulli 1.0

    // 2022-06-22
    // Wikipedia: https://de.wikipedia.org/wiki/Bernoulli-Verteilung#Definition 
    // "p is element of closed intervall between 0. and 1."
    testList "Distributions.Discrete.Bernoulli" [
        test "bernCheckParam" {
            let test_lowerThan0 = fun (x: unit) -> Distributions.Discrete.Bernoulli.CheckParam -0.1
            let test_highterThan1 = fun (x: unit) -> Distributions.Discrete.Bernoulli.CheckParam 1.1
            let test_basic = Distributions.Discrete.Bernoulli.CheckParam test_basicNumber
            let test_zero = Distributions.Discrete.Bernoulli.CheckParam 0.
            let test_one = Distributions.Discrete.Bernoulli.CheckParam 1.
            let test_nan = Distributions.Discrete.Bernoulli.CheckParam nan // 
            let test_infinity = fun (x: unit) -> Distributions.Discrete.Bernoulli.CheckParam infinity
            let test_negativeInfinity = fun (x: unit) -> Distributions.Discrete.Bernoulli.CheckParam -infinity
            Expect.throws test_lowerThan0 ""
            Expect.throws test_highterThan1 ""
            Expect.equal test_basic () ""
            Expect.equal test_zero () ""
            Expect.equal test_one () ""
            Expect.equal test_nan () ""
            Expect.throws test_infinity ""
            Expect.throws test_negativeInfinity ""
        }

        test "Mean" {
            Expect.equal bernoulliDistribution_basicCase.Mean test_basicNumber ""
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.Mean)) ""
            Expect.equal bernoulliDistribution_zero.Mean 0.0 ""
            Expect.equal bernoulliDistribution_one.Mean 1.0 ""
        }
        // 2022-06-22
        // Compared to: https://www.trignosource.com/statistics/bernoulli%20distribution.html
        test "Variance" {
            Expect.equal bernoulliDistribution_basicCase.Variance 0.2436 ""
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.Variance)) ""
            Expect.equal bernoulliDistribution_zero.Variance 0.0 ""
            Expect.equal bernoulliDistribution_one.Variance 0.0 ""
        }
        // 2022-06-22
        // Compared to: https://www.trignosource.com/statistics/bernoulli%20distribution.html
        // https://www.kristakingmath.com/blog/bernoulli-random-variables
        test "StandardDeviation" {
            Expect.equal bernoulliDistribution_basicCase.StandardDeviation (sqrt 0.2436) ""
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.StandardDeviation)) ""
            Expect.equal bernoulliDistribution_zero.StandardDeviation (sqrt 0.0) ""
            Expect.equal bernoulliDistribution_one.StandardDeviation (sqrt 0.0) ""
        }
        // not implemented
        test "Sample" {
            Expect.throws (bernoulliDistribution_basicCase.Sample >> ignore) ""
            Expect.throws (bernoulliDistribution_nan.Sample >> ignore) ""
            Expect.throws (bernoulliDistribution_zero.Sample >> ignore) ""
            Expect.throws (bernoulliDistribution_one.Sample >> ignore)  ""
        }
        test "PDF" {
            /// propabiliy of an outcome to be be of a certain value. Bernoulli distribution can only result in 0 (failure) or 1 (success) so anything except 
            /// those should have a propability of 0.
            let test_ZeroAndOne (bd: Distributions.DiscreteDistribution<float,int>) = 
                let propabilitySuccess = bd.PMF 1
                let propabilityFailure = bd.PMF 0
                Expect.equal propabilitySuccess (bd.Mean) $"test_ZeroAndOne.propabilitySuccess for {bd.Mean}"
                Expect.floatClose Accuracy.high propabilityFailure (1.0 - bd.Mean) $"test_ZeroAndOne.propabilityFailure for {bd.Mean}"
            //let test_ZeroPDFCases (bd: Distributions.DiscreteDistribution<float,int>) =
            //    Expect.equal (bd.PMF 0.1) 0.0 $"test_ZeroPDFCases 0.1 for {bd.Mean}"
            //    Expect.equal (bd.PMF -0.1) 0.0 $"test_ZeroPDFCases -0.1 for {bd.Mean}"
            //    Expect.equal (bd.PMF 1.1) 0.0 $"test_ZeroPDFCases 1.1 for {bd.Mean}"
            //    Expect.equal (bd.PDF nan) 0.0 $"test_ZeroPDFCases nan for {bd.Mean}"
            //    Expect.equal (bd.PDF infinity) 0.0 $"test_ZeroPDFCases infinity for {bd.Mean}"
            //    Expect.equal (bd.PDF -infinity) 0.0 $"test_ZeroPDFCases -infinity for {bd.Mean}"
            //test_ZeroPDFCases bernoulliDistribution_basicCase
            //test_ZeroPDFCases bernoulliDistribution_nan
            //test_ZeroPDFCases bernoulliDistribution_zero
            //test_ZeroPDFCases bernoulliDistribution_one
            //
            test_ZeroAndOne bernoulliDistribution_basicCase
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.PMF 0)) $"test_ZeroAndOne.propabilitySuccess for nan"
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.PMF 1)) $"test_ZeroAndOne.propabilityFailure for nan"
            test_ZeroAndOne bernoulliDistribution_zero
            test_ZeroAndOne bernoulliDistribution_one
        }
        test "CDF" {
            // For P(x>=R) and R∈{0,1}, where R is the random outcome of the bernoulli distribution, any value below 0 has a probability of 0 to be greater or equal to R
            let test_ZeroCDFCases (bd: Distributions.DiscreteDistribution<float,int>) = 
                Expect.equal (bd.CDF -0.1) 0.0 $"test_ZeroCDFCases -0.1 for {bd.Mean}"
                Expect.equal (bd.CDF -infinity) 0.0 $"test_ZeroCDFCases -infinity for {bd.Mean}"
                Expect.equal (bd.CDF nan) 0.0 $"test_ZeroCDFCases -infinity for {bd.Mean}"
            let test_OneCDFCases (bd: Distributions.DiscreteDistribution<float,int>) = 
                Expect.equal (bd.CDF 1.0) 1.0 $"test_OneCDFCases 1.0 for {bd.Mean}"
                Expect.equal (bd.CDF 1.1) 1.0 $"test_OneCDFCases 1.1 for {bd.Mean}"
                Expect.equal (bd.CDF infinity) 1.0 $"test_OneCDFCases infinity for {bd.Mean}"
            test_ZeroCDFCases bernoulliDistribution_basicCase
            test_ZeroCDFCases bernoulliDistribution_nan
            test_ZeroCDFCases bernoulliDistribution_zero
            test_ZeroCDFCases bernoulliDistribution_one
            //
            test_OneCDFCases bernoulliDistribution_basicCase
            test_OneCDFCases bernoulliDistribution_nan
            test_OneCDFCases bernoulliDistribution_zero
            test_OneCDFCases bernoulliDistribution_one
            //
            Expect.floatClose Accuracy.high (bernoulliDistribution_basicCase.CDF 0.8) (1.0 - bernoulliDistribution_basicCase.Mean) ""
            Expect.isTrue (isNan <| bernoulliDistribution_nan.CDF 0.8) ""
            Expect.floatClose Accuracy.high (bernoulliDistribution_zero.CDF 0.8) (1.0 - bernoulliDistribution_zero.Mean) ""
            Expect.floatClose Accuracy.high (bernoulliDistribution_one.CDF 0.8) (1.0 - bernoulliDistribution_one.Mean) ""
        }
        //// Tbh. i have no idea what this is for
        //test "Support" {
        //    // insert any number which does not throw an error in "bernCheckParam".
        //    Expect.sequenceEqual (Distributions.Discrete.Bernoulli.Support 0.2) [0.0; 1.0] ""
        //}
    ]

[<Tests>]
let bandWithTests =
    testList "Distribution.Bandwidth.BinNumber" [
        //Reference:https://www.statisticshowto.com/choose-bin-sizes-statistics/#rice
        // tested with r Function ceiling(1+log2(x))
        
        testCase "Distribution.Bandwidth.BinNumber.sturges" <| fun () ->
            let sturges1 =Distributions.Bandwidth.BinNumber.sturges 1.
            Expect.floatClose Accuracy.veryHigh 1 sturges1 "desirable number of classes Should be equal"

            let sturgesForNull = Distributions.Bandwidth.BinNumber.sturges 0.
            Expect.isTrue (-infinity = sturgesForNull) "desirable number of classes should be equal"

            let sturgesForNegative = Distributions.Bandwidth.BinNumber.sturges -1.
            Expect.isTrue (nan.Equals(sturgesForNegative)) "desirable number of classes should be nan."

            let sturgesForNan = Distributions.Bandwidth.BinNumber.sturges nan
            Expect.isTrue (nan.Equals(sturgesForNan)) "desirable number of classes should be nan."

            let sturgesForPositivInifinity = Distributions.Bandwidth.BinNumber.sturges infinity
            Expect.isTrue (infinity = sturgesForPositivInifinity) "desirable number of classes should be equal"

            let sturgesForNegativeInfinity = Distributions.Bandwidth.BinNumber.sturges -infinity
            Expect.isTrue (nan.Equals(sturgesForNegativeInfinity)) "desirable number of classes should be nan."

            let sturgesWithRealWorldProblem = Distributions.Bandwidth.BinNumber.sturges 1000.
            Expect.floatClose Accuracy.veryHigh sturgesWithRealWorldProblem 11 "desirable number of bins should be equal"

        // reference:https://www.rdocumentation.org/packages/npsp/versions/0.7-5/topics/rule
        // tested with R function ceiling(2*(n ** (1./3.)))

        testCase "Distribution.Bandwidth.BinNumber.riceRule" <| fun () ->
            let riceRule1 = Distributions.Bandwidth.BinNumber.riceRule 1.
            Expect.floatClose Accuracy.veryHigh riceRule1 2. "desirable number of classes should be equal"

            let riceRuleForNull = Distributions.Bandwidth.BinNumber.riceRule 0.
            Expect.floatClose Accuracy.veryHigh riceRuleForNull 0. "desirbale number of classes should be equal to expected Value"

            let riceRuleForNan = Distributions.Bandwidth.BinNumber.riceRule nan
            Expect.isTrue (nan.Equals(riceRuleForNan)) "desirable number of classes should be nan."

            let riceRuleForNegative = Distributions.Bandwidth.BinNumber.riceRule (-1.)
            Expect.isTrue (nan.Equals(riceRuleForNegative)) "desirable number of classes should be nan."

            let riceRuleForPositiveInfinity = Distributions.Bandwidth.BinNumber.riceRule infinity
            Expect.isTrue (infinity = riceRuleForPositiveInfinity) "desirable number of classes should be equal"

            let riceRuleForNegativeInfinity = Distributions.Bandwidth.BinNumber.riceRule -infinity
            Expect.isTrue (infinity= riceRuleForNegativeInfinity) "desirable number of classes should be -infinity."

            let riceRuleWithRealWorldExample = Distributions.Bandwidth.BinNumber.riceRule 1000.
            Expect.floatClose Accuracy.veryHigh riceRuleWithRealWorldExample 20. "desirable number of bins should be equal"

   
    ]

[<Tests>]
let FDistributionTests =
    // Values taken from R 4.0.3 and Wolfram alpha 23.06.2022
    // Weisstein, Eric W. "F-Distribution." From MathWorld--A Wolfram Web Resource. https://mathworld.wolfram.com/F-Distribution.html

    testList "Distributions.Continuous.F" [
        testCase "fCheckParam_dof1<0" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.F.CheckParam -0.5 420.))
                "fCheckParam does not fail with dof1<0"
        
        testCase "fCheckParam_dof2<0" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.F.CheckParam 420. -0.5))
                "fCheckParam does not fail with dof2<0"
        
        testCase "fCheckParam_dof1=0" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.F.CheckParam 0. 420.))
                "fCheckParam does not fail with dof1<0"
        
        testCase "fCheckParam_dof2=0" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.F.CheckParam 420. 0.))
                "fCheckParam does not fail with dof2<0"

        testCase "fCheckParam_dof1=nan" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.F.CheckParam nan 420.))
                "fCheckParam does not fail with dof1=nan"
        
        testCase "fCheckParam_dof2=nan" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.F.CheckParam 420. nan))
                "fCheckParam does not fail with dof2=nan"
     
        testCase "fCheckParam_dof1=-infinity" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.F.CheckParam -infinity 420.))
                "fCheckParam does not fail with dof1=-infinity"
        
        testCase "fCheckParam_dof2=-infinity" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.F.CheckParam 420. -infinity))
                "fCheckParam does not fail with dof2=-infinity"
 
        testCase "fCheckParam_dof1=infinity" <| fun () ->
            Expect.isTrue
                ((Continuous.F.CheckParam infinity 420.|> fun x -> true))
                "fCheckParam does fail with dof1=infinity"
        
        testCase "fCheckParam_dof2=infinity" <| fun () ->
            Expect.isTrue
                ((Continuous.F.CheckParam 420. infinity|> fun x -> true))
                "fCheckParam does fail with dof2=infinity"

        testCase "Continuous.F.Mean" <| fun () ->
            let dof1 = 10.
            let dof2 = 10.
            let testcase = Continuous.F.Mean dof1 dof2
            let r_value = 1.25
            Expect.floatClose
                Accuracy.medium
                testcase
                r_value
                (sprintf "Continuous.F.Mean with dof1=%f and dof2=%f does not yield the expected %f" dof1 dof2 r_value)
        
        testCase "Continuous.F.Mean_dof2<=2" <| fun () ->
            let dof1 = 10.
            let dof2 = 2.
            let dof2_1 = 1.
            let dof2_2 = 1.5
            let testcase    = Continuous.F.Mean dof1 dof2
            let testcase2   = Continuous.F.Mean dof1 dof2_1
            let testcase3   = Continuous.F.Mean dof1 dof2_2
            let r_value     = nan
            Expect.isTrue
                ((isNan testcase)&& isNan(r_value)&&isNan(testcase2)&&isNan(testcase3))
                (sprintf "Continuous.F.Mean with dof<=2 does not return nan %A %A %A" testcase testcase2 testcase3 )

        testCase "Continuous.F.Mean_dof1=Infininty" <| fun () ->
            let dof1 = infinity
            let dof2 = 69.
            let testcase    = Continuous.F.Mean dof1 dof2
            let r_value     = 1.02985
            Expect.floatClose
                Accuracy.medium
                (testcase)
                (r_value)
                (sprintf "Continuous.F.Mean with dof2=69. does not return nan %A" testcase  )

        testCase "Continuous.F.Mean_dof2=Infininty" <| fun () ->
            let dof1 = 10.
            let dof2 = infinity
            let testcase    = Continuous.F.Mean dof1 dof2
            let r_value     = nan
            Expect.isTrue
                ((isNan testcase)&& isNan(r_value))
                (sprintf "Continuous.F.Mean with dof<=2 does not return nan %A" testcase  )
        
        testCase "Continuous.F.Mean_dof1&2=Infininty" <| fun () ->
            let dof1 = infinity
            let dof2 = infinity
            let testcase    = Continuous.F.Mean dof1 dof2
            let r_value     = nan
            Expect.isTrue
                ((isNan testcase)&& isNan(r_value))
                (sprintf "Continuous.F.Mean with dof<=2 does not return nan %A" testcase  )

        testCase "Continuous.F.Variance" <| fun () ->
            let dof1 = 10.
            let dof2 = 10.
            let testcase = Continuous.F.Variance dof1 dof2
            let r_value = 0.9375
            Expect.floatClose
                Accuracy.medium
                testcase
                r_value
                (sprintf "Continuous.F.Variance with dof1=%f and dof2=%f does not yield the expected %f" dof1 dof2 r_value)
        
        testCase "Continuous.F.Variance_dof2<=4" <| fun () ->
            let dof1        = 10.
            let dof2s       = [4. .. 0.5 .. 0.]
            let testcase    = 
                dof2s|>
                List.map(fun dof2 -> Continuous.F.Variance dof1 dof2 |> isNan)
            let r_value     = nan

            Expect.isTrue
                (isNan(r_value)&& (List.contains false testcase|> not))
                (sprintf "Continuous.F.Variance with dof<=2 does not return nan")
        
        testCase "Continuous.F.StandardDeviation" <| fun () ->
            let dof1 = 10.
            let dof2 = 10.
            let testcase = Continuous.F.StandardDeviation dof1 dof2
            let r_value = 0.968246
            Expect.floatClose
                Accuracy.medium
                testcase
                r_value
                (sprintf "Continuous.F.StandardDeviation with dof1=%f and dof2=%f does not yield the expected %f" dof1 dof2 r_value)
  
        testCase "Continuous.F.StandardDeviation_dof2<=4" <| fun () ->
            let dof1        = 10.
            let dof2s       = [4. .. 0.5 .. 0.]
            let testcase    = 
                dof2s|>
                List.map(fun dof2 -> Continuous.F.StandardDeviation dof1 dof2 |> isNan)
            let r_value     = nan

            Expect.isTrue
                (isNan(r_value)&& (List.contains false testcase|> not))
                (sprintf "Continuous.F.Variance with dof<=2 does not return nan")
        
        testCase "Continuous.F.Sample" <| fun () ->
            let dof1        = 10000.
            let dof2        = 10000.
            let testcase    = 
                [for i=0 to 10000 do Continuous.F.Sample dof1 dof2]
                |> List.mean
                |> round 5
                
            let r_value     = 
                round 5 (1.000359)

            Expect.floatClose
                Accuracy.low
                testcase
                r_value
                "The mean of 100 sampled values is not close to the respective R value"       
        
        //testCase "fCheckX" <| fun () ->
        //    Expect.throws
        //        (fun () -> (Continuous.F.CheckX -10.))
        //        "fCheckX does not fail with negative values"
        //    Expect.throws
        //        (fun () -> (Continuous.fCheckX nan))
        //        "fCheckX does not fail with nan"
        //    Expect.throws
        //        (fun () -> (Continuous.fCheckX -infinity))
        //        "fCheckX does not fail with -infinity"
        //    Expect.isTrue
        //        ((Continuous.fCheckX 10.)|> fun x -> true)
        //        "fCheckX fails with positive values"
        //    Expect.isTrue
        //        ((Continuous.fCheckX 0)|> fun x -> true)
        //        "fCheckX fails with 0"
        //    Expect.isTrue
        //        ((Continuous.fCheckX infinity)|> fun x -> true)
        //        "fCheckX fails with infinity"
                
        testCase "Continuous.F.PDF" <| fun () ->
            let dof1        = 69.
            let dof2        = 420.
            let testcase    = Continuous.F.PDF dof1 dof2 50.
            let r_value     = 3.90748e-163

            Expect.floatClose
                Accuracy.low
                testcase
                r_value
                "Continuous.F.PDF does not yield the expected value"
        
        testCase "Continuous.F.PDF_infinity" <| fun () ->
            let dof1        = 69.
            let dof2        = 420.
            let testcase_1    = 
                Continuous.F.PDF dof1 infinity 50.
            let testcase_2    = 
                Continuous.F.PDF infinity dof2 50.
            let testcase_3    = 
                Continuous.F.PDF infinity infinity 50.
            
            let r_value_1     = 0.
            let r_value_2     = 4.539216e-269
            let r_value_3     = 0.

            Expect.floatClose
                Accuracy.low
                testcase_1
                r_value_1
                "Continuous.F.PDF_infinity with dof2=infinity does not yield the expected value"
            Expect.floatClose
                Accuracy.low
                testcase_2
                r_value_2
                (sprintf"Continuous.F.PDF_infinity with dof1=infinity does not yield the expected value. Actual: %A, expected: %A"testcase_2 r_value_2)
            Expect.floatClose
                Accuracy.low
                testcase_3
                r_value_3
                "Continuous.F.PDF_infinity with dof1&dof2=infinity does not yield the expected value"

        testCase "Continuous.F.CDF" <| fun () ->
            let dof1        = 69.
            let dof2        = 420.
            let testcase    = Continuous.F.CDF dof1 dof2 50.
            let r_value     = 1.

            Expect.floatClose
                Accuracy.low
                testcase
                r_value
                (sprintf"Continuous.F.CDF dof1 dof2 50. does not yield 1. but %A"testcase)


        testCase "Continuous.F.CDF_infinity" <| fun () ->
            let dof1        = 69.
            let dof2        = 420.
            let testcase_1    = 
                Continuous.F.CDF dof1 infinity 50.
            let testcase_2    = 
                Continuous.F.CDF infinity dof2 50.
            let testcase_3    = 
                Continuous.F.CDF infinity infinity 50.
            
            let r_value_1     = nan
            let r_value_2     = nan
            let r_value_3     = nan

            Expect.isTrue
                (isNan(testcase_1)&&isNan(r_value_1))
                "Continuous.F.CDF with dof2=infinity does not yield the expected value"
            Expect.isTrue
                (isNan(testcase_2)&&isNan(r_value_2))
                "Continuous.F.CDF with dof1=infinity does not yield the expected value"
            Expect.isTrue
                (isNan(testcase_3)&&isNan(r_value_3))
                "Continuous.F.CDF with dof1&dof2=infinity does not yield the expected value"

        testCase "Continuous.F.Support" <| fun () ->
            let dof1            = 10.
            let dof2            = 25.
            let testcase    = 
                Continuous.F.Support dof1 dof2
            let r_value     = (0., System.Double.PositiveInfinity)

            Expect.isTrue
                ((fst testcase=fst r_value) && (snd testcase=snd r_value))
                "Continuous.F.Support does not return the expected Tupel"
        
        testCase "Continuous.F.Support_infinity" <| fun () ->
            let dof1            = infinity
            let dof2            = infinity
            let testcase    = 
                Continuous.F.Support dof1 dof2
            let r_value     = (0., System.Double.PositiveInfinity)

            Expect.isTrue
                ((fst testcase=fst r_value) && (snd testcase=snd r_value))
                "Continuous.F.Support does not return the expected Tupel"


    ]

let binomialTests =
    // TestCases from R: library(chi) function: dchi(x, dof)

    testList "Distributions.Discrete.Binominal" [
        // Values taken from R 4.0.3 

        testCase "binomialCheckParamN<0" <| fun () ->
            Expect.throws 
                (fun () -> Discrete.Binomial.CheckParam 0.5 (-5)) 
                "binomialCheckParam does work with n<0" 
        
        testCase "binomialCheckParamP<0." <| fun () ->
            Expect.throws
                (fun () -> Discrete.Binomial.CheckParam (-0.5) 10)
                "binomialCheckParam does work with p<0" 
        
        testCase "binomialCheckParamP>1." <| fun () ->
            Expect.throws 
                (fun () -> Discrete.Binomial.CheckParam 1.5 10) 
                "binomialCheckParam does work with p>1" 
        
        testCase "binomialCheckParamPInfinite." <| fun () ->
            Expect.throws 
                (fun () -> Discrete.Binomial.CheckParam infinity 10) 
                "binomialCheckParam does work with p=infinity" 
        
        testCase "binomialCheckParamPNegInfinite." <| fun () ->
            Expect.throws 
                (fun () -> Discrete.Binomial.CheckParam (-infinity) 10) 
                "binomialCheckParam does work with p=-infinity" 
        
        testCase "binomialCheckParamPnan" <| fun () ->
            Expect.throws 
                (fun () -> Discrete.Binomial.CheckParam (nan) 10) 
                (sprintf"binomialCheckParam does work with p=nan ,yields")

        testCase "Binomial.Mean_n=0" <| fun () ->
            let testCase    = Discrete.Binomial.Mean 0.5 0
            let r_value  = 0
            Expect.equal
                testCase
                r_value
                "Binominal mean with n=0 does not yield the expected value of 0" 
        
        testCase "Binomial.Mean" <| fun () ->
            let testCase    = Discrete.Binomial.Mean 0.5 500
            let r_value  = 250
            Expect.equal
                testCase
                r_value
                "Binominal mean with n=500 and p=0.5 does not yield the expected value of 250" 

        testCase "Binomial.Variance_n=0" <| fun () ->
            let testCase    = Discrete.Binomial.Variance 0.5 0
            let r_value     = 0
            Expect.equal
                testCase
                r_value
                "Binominal Variance with n=0 a does not yield the expected value of 0" 

        testCase "Binomial.StandardDeviation_n=0" <| fun () ->
            let testCase = Discrete.Binomial.StandardDeviation 0.5 0
            let r_value     = 0
            Expect.equal
                testCase
                r_value
                "Binominal StandardDeviation with n=0 does not yield the expected value of 0" 

        testCase "Binomial.Variance" <| fun () ->
            let testCase    = Discrete.Binomial.Variance 0.69 420
            let r_value     = 89.838
            Expect.floatClose 
                Accuracy.high
                testCase
                r_value
                "Binominal Variance with n=420 and p=0.69 does not yield the expected value of 89.838" 

        testCase "Binomial.StandardDeviation" <| fun () ->
            let testCase    = Discrete.Binomial.StandardDeviation 0.69 420
            let r_value     = 9.478291
            Expect.floatClose
                Accuracy.high
                testCase
                r_value
                "Binominal StandardDeviation with n=420 and p=0.69 does not yield the expected value of 9.478291" 
            
        testCase "Binomial.PMF" <| fun () ->
            let testCase    = Discrete.Binomial.PMF 0.69 420 237
            let r_value     = 4.064494e-08
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.PMF with n=420, p=0.69 and k=237 does not equal the expectd 4.064494e-08"

        testCase "Binomial.PMF_n=0" <| fun () ->
            let testCase    = Discrete.Binomial.PMF 0.69 0 237
            let r_value     = 0
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.PMF with n=0, p=0.69 and k=237 does not equal the expectd 0"

        testCase "Binomial.PMF_k<0" <| fun () ->
            let testCase    = Discrete.Binomial.PMF 0.69 420 -10
            let r_value     = 0
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.PMF with n=420, p=0.69 and k=-10 does not equal the expectd 0"

        testCase "Binomial.CDF"<| fun () ->
            let testCase = Discrete.Binomial.CDF 0.69 420 237
            let r_value = 9.341312e-08
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.CDF with n=420, p=0.69 and k=237 does not equal the expectd 9.341312e-08"
        
        testCase "Binomial.CDF_n=0"<| fun () ->
            let testCase = Discrete.Binomial.CDF 0.69 0 237
            let r_value = 1.
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.CDF with n=0, p=0.69 and k=237 does not equal the expectd 1."

        testCase "Binomial.CDF_k=0"<| fun () ->
            let testCase = Discrete.Binomial.CDF 0.69 420 0
            let r_value = 2.354569e-214
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.CDF with n=420, p=0.69 and k=0 does not equal the expectd 2.354569e-214"
                
        testCase "Binomial.CDF_k<0"<| fun () ->
            let testCase = Discrete.Binomial.CDF 0.69 420 -10
            let r_value = 0. 
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.CDF with n=420, p=0.69 and k=-10 does not equal the expectd 0."
        
        testCase "Binomial.CDF_k-infinity"<| fun () ->
            let testCase = Discrete.Binomial.CDF 0.69 420 (-infinity)
            let r_value = 0.
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.CDF with n=420, p=0.69 and k=--infinity does not equal the expectd 0."
         
        testCase "Binomial.CDF_kinfinity"<| fun () ->
            let testCase = Discrete.Binomial.CDF 0.69 420 (infinity)
            let r_value = 1. 
            Expect.floatClose
                Accuracy.low
                testCase
                r_value
                "Binomial.CDF with n=420, p=0.69 and k=-infinity does not equal the expectd 1."   

        testCase "Binomial.Sample" <| fun () ->
            let testCase = 
                [
                    for i=0 to 49 do
                        Discrete.Binomial.Sample 0.01 100 
                ] |> List.distinct |> List.length
            let r_value = 4
            
            let testSolution =
                let help = [-2 .. 2]
                if (help |> List.map(fun x -> x+r_value)) |> List.contains r_value then
                    true
                else
                    false

            Expect.isTrue
                (testSolution)
                (sprintf"50 of 100 binominal values yields not a comparable similarity %A"testCase)
        
        testCase "Binomial.Sample_n=0" <| fun () ->
            let testCase = 
                [
                    for i=0 to 49 do
                        Discrete.Binomial.Sample 0.01 0 
                ] |> List.distinct
            let r_value = [0]
            
            Expect.isTrue
                (testCase=r_value)
                ("50 of 100 binominal values yields not a comparable similarity")
                      
    ] 



[<Tests>]
let PoissonDistributionTests =
     
    let lambda = 4.2
    
    let d      = DiscreteDistribution.poisson lambda

    let mean   = d.Mean      
    let var    = d.Variance  
    let cdf1   = d.CDF 2 // 0.21023798702309743
    let cdf2   = d.CDF 4 // 0.589827021310577643
    let cdf3   = d.CDF 7 // 0.936056660272578944
    let pmf1   = d.PMF 4 // 0.19442365170822165
    let pmf2   = d.PMF 5 // 0.1633158674349062
    let pmf3   = d.PMF 6 // 0.11432110720443435

    
    testList "Distributions.Discrete.Poisson" [

        testCase "Mean" <| fun () ->
            Expect.floatClose Accuracy.high mean lambda "Mean should be equal"

        testCase "Variance" <| fun () ->
            Expect.floatClose Accuracy.high var lambda "Variance should be equal"
                
        testCase "Cdf1" <| fun () ->
            Expect.floatClose Accuracy.high cdf1 0.21023798702309743 "Cdf should be equal"
                
        testCase "Cdf2" <| fun () ->
            Expect.floatClose Accuracy.high cdf2 0.589827021310577643 "Cdf should be equal"
                
        testCase "Cdf3" <| fun () ->
            Expect.floatClose Accuracy.high cdf3 0.93605666027257894 "Cdf should be equal"

        testCase "Pmf1" <| fun () ->
            Expect.floatClose Accuracy.high pmf1 0.19442365170822165 "Pdf should be equal"

        testCase "Pmf2" <| fun () ->
            Expect.floatClose Accuracy.high pmf2 0.1633158674349062 "Pdf should be equal"

        testCase "Pmf3" <| fun () ->
            Expect.floatClose Accuracy.high pmf3 0.11432110720443435 "Pdf should be equal"
        
        
        testCase "FitTest<30" <| fun () ->
            let lambda = 11.5
            let observations = Array.init 9999 (fun _ -> float (Discrete.Poisson.Sample lambda))
            let lambda' = Discrete.Poisson.Fit observations
            
            Expect.floatClose fittingAccuracy lambda lambda' 
                "Poisson Distribution Fit lambda < 30 (knuth) " 
        
        testCase "FitTest>30" <| fun () ->
            let lambda = 125.5
            let observations = Array.init 9999 (fun _ -> float (Discrete.Poisson.Sample lambda))
            let lambda' = Discrete.Poisson.Fit observations
            
            Expect.floatClose fittingAccuracy lambda lambda' 
                "Poisson Distribution Fit lambda > 30 (pma)" 
    ]


[<Tests>]
let BetaDistributionTests =

    let alpha = 0.42 
    let beta  = 1.57
    
    let d     = ContinuousDistribution.beta alpha beta

    let mean  = Continuous.Beta.Mean alpha beta     // 0.21105527638190955
    let var   = d.Variance // 0.055689279830523512
    let cdf   = d.CDF 0.27 // 0.69358638272337991
    let pdf   = d.PDF 0.27 // 0.94644031936694828


    testList "Distributions.Continuous.Beta" [
        
        testCase "Mean" <| fun () ->
            Expect.floatClose Accuracy.high mean 0.21105527638190955 "Mean should be equal"

        testCase "Variance" <| fun () ->
            Expect.floatClose Accuracy.high var 0.055689279830523512 "Variance should be equal"
                
        testCase "Cdf" <| fun () ->
            Expect.floatClose Accuracy.high cdf 0.69358638272337991 "Cdf should be equal"

        testCase "Pdf" <| fun () ->
            Expect.floatClose Accuracy.high pdf 0.94644031936694828 "Pdf should be equal"
        
        testCase "FitTest" <| fun () ->
            let observations = Array.init 99999 (fun _ -> float (Continuous.Beta.Sample alpha beta))
            let alpha',beta' = Continuous.Beta.Fit observations
            
            Expect.floatClose fittingAccuracy alpha alpha' 
                "alpha" 
            Expect.floatClose fittingAccuracy beta beta' 
                "beta"
    ]

[<Tests>]
let GammaDistributionTests =

    let alpha = 0.4 
    let beta  = 4.2
    
    let d     = ContinuousDistribution.gamma alpha beta

    let mean  = d.Mean     // 0.21105527638190955
    let var   = d.Variance // 0.055689279830523512
    let cdfs  = [| 0.; 0.251017; 0.328997; 0.38435; 0.428371; 0.465289;
                   0.497226; 0.525426; 0.55069; 0.573571 |] 

    let pdfs = [| 0.987114; 0.635929; 0.486871; 0.400046; 0.341683;
                  0.299071; 0.266236; 0.239956; 0.218323; 0.200126; |]



    testList "Distributions.Continuous.Gamma" [
        
        testCase "Mean" <| fun () ->
            Expect.floatClose Accuracy.high mean 0.21105527638190955 "Mean should be equal"

        testCase "Variance" <| fun () ->
            Expect.floatClose Accuracy.high var 0.055689279830523512 "Variance should be equal"
                
        testCase "Cdfs" <| fun () ->
            cdfs 
            |> Array.iteri (fun i v ->
                let cdf = d.CDF (float i / 10.0)
                Expect.floatClose Accuracy.low cdf cdfs[i] "Cdf should be equal"
                )
                 
        testCase "Pdfs" <| fun () ->
            cdfs 
            |> Array.iteri (fun i v ->
                let pdf = d.PDF ((float i + 1.) / 10.0)
                Expect.floatClose Accuracy.low pdf pdfs[i] "Cdf should be equal"
                )          
           
        //testCase "Pdf" <| fun () ->
        //    Expect.floatClose Accuracy.high pdf 0.987114 "Pdf should be equal"
        
        testCase "FitTest" <| fun () ->
            let observations = Array.init 99999 (fun _ -> float (Continuous.Beta.Sample alpha beta))
            let alpha',beta' = Continuous.Beta.Fit observations
            
            Expect.floatClose fittingAccuracy alpha alpha' 
                "alpha" 
            Expect.floatClose fittingAccuracy beta beta' 
                "beta"
    
        testCase "FitTest_from_observations" <| fun () ->
            let observations = [| 1275.56; 1239.44; 1237.92; 1237.22; 1237.1; 1238.41; 1238.62; 1237.05;
                1237.19; 1236.51; 1264.6; 1238.19; 1237.39; 1235.79; 1236.53; 1236.8; 1238.06; 
                1236.5; 1235.32; 1236.44; 1236.58; 1236.3; 1237.91; 1238.6; 1238.49; 1239.21; 
                1238.57; 1244.63; 1236.06; 1236.4; 1237.88; 1237.56; 1236.66; 1236.59; 1236.53; 
                1236.32; 1238.29; 1237.79; 1237.86; 1236.42; 1236.23; 1236.37; 1237.18; 1237.63; 
                1245.8; 1238.04; 1238.55; 1238.39; 1236.75; 1237.07; 1250.78; 1238.6; 1238.36; 
                1236.58; 1236.82; 1238.4; 1257.68; 1237.78; 1236.52; 1234.9; 1237.9; 1238.58; 
                1238.12; 1237.89; 1236.54; 1236.55; 1238.37; 1237.29; 1237.64; 1236.8; 1237.73; 
                1236.71; 1238.23; 1237.84; 1236.26; 1237.58; 1238.31; 1238.4; 1237.08; 1236.61; 
                1235.92; 1236.41; 1237.89; 1237.98; 1246.75; 1237.92; 1237.1; 1237.97; 1238.69; 
                1237.05; 1236.96; 1239.44; 1238.49; 1237.88; 1236.01; 1236.57; 1236.44; 1235.76; 
                1237.62; 1238; 1263.14; 1237.66; 1237; 1236; 1261.96; 1238.58; 1237.77; 1237.06; 
                1236.31; 1238.63; 1237.23; 1236.85; 1236.23; 1236.46; 1236.9; 1237.85; 1238; 
                1237.02; 1236.19; 1236.05; 1235.73; 1258.3; 1235.98; 1237.76; 1246.93; 1239.1; 
                1237.72; 1237.67; 1236.79; 1237.61; 1238.41; 1238.29; 1238.11; 1237; 1236.52; 
                1236.6; 1236.31; 1237.77; 1238.58; 1237.88; 1247.35; 1236.14; 1236.83; 1236.15; 
                1237.93; 1238.16; 1237.34; 1236.78; 1238.66; 1237.76; 1237.19; 1236.7; 1236.04; 
                1236.66; 1237.86; 1238.54; 1238.05; 1238.41; 1236.94; 1240.95; 1261.01; 1237.72; 
                1237.91; 1238.2; 1235.68; 1236.89; 1235.12; 1271.31; 1236.97; 1270.76; 1238.52; 
                1238.19; 1238.6; 1237.16; 1236.72; 1236.71; 1237.14; 1238.48; 1237.95; 1237.42; 
                1235.86; 1236.39; 1236.13; 1236.58; 1237.95; 1237.76; 1237.39; 1238.16; 1236.31; 
                1236.41; 1236.12; 1238.7; 1236.48; 1237.84; 1236.38; 1237.95; 1238.48; 1236.51; 
                1236.56 |]
            let testGamma = Continuous.Gamma.Estimate observations
            let mean = 1238.8734170854279
            Expect.floatClose
                fittingAccuracy
                testGamma.Mean
                mean
                "Gamma Distribution Fit" 
    
    
    
    ]