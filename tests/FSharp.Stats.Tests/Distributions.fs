module DistributionsTests 
open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open Distance.OneDimensional
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
                let testCase = Continuous.chiSquaredCheckParam 10.
                Expect.isTrue (testCase = ()) "Should be unit"
            testCase "CheckParam0" <| fun () ->
                let testCase = Continuous.chiSquaredCheckParam 0.
                Expect.isTrue (testCase = ()) "Should be unit"
            testCase "CheckParamInfinity" <| fun () ->
                let testCase = Continuous.chiSquaredCheckParam infinity
                Expect.isTrue (testCase = ()) "Should be unit"
            testCase "CheckParam-1" <| fun () ->
                Expect.throws (fun () -> Continuous.chiSquaredCheckParam -1.) "Should throw"
            testCase "CheckParam-infinity" <| fun () ->
                Expect.throws (fun () -> Continuous.chiSquaredCheckParam -infinity) "Should throw"
            testCase "CheckParamNan" <| fun () ->
                Expect.throws (fun () -> Continuous.chiSquaredCheckParam nan) "Should throw"
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
            testCase "Support0" <| fun () ->
                let testCase = Continuous.ChiSquared.Support 0.
                Expect.isTrue (testCase = (0., infinity)) "Should be equal"
            testCase "Support1" <| fun () ->
                let testCase = Continuous.ChiSquared.Support 1.
                Expect.isTrue (testCase = (0., infinity)) "Should be equal"
            testCase "SupportInfinity" <| fun () ->
                let testCase = Continuous.ChiSquared.Support infinity
                Expect.isTrue (testCase = (0., infinity)) "Should be equal"

            // is based on the functions from before but nevertheless should be tested, too
            testCase "chiSquared-1" <| fun () ->
                let testCase = Continuous.chiSquared -1.
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
                let testCase = Continuous.chiSquared -infinity
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
                let testCase = Continuous.chiSquared nan
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
                let testCase = Continuous.chiSquared 0.
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
                let testCase = Continuous.chiSquared 1.
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
                let testCase = Continuous.chiSquared infinity
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
    testList "Distributions.chi" [
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
    let mvn = Continuous.multivariateNormal (vector [0.;0.;0.;0.;0.]) (Matrix.identity 5)
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

    let hypergeoDistribution_basicCase = Distributions.Discrete.hypergeometric 50 40 5
    let hypergeoDistribution_K_equal_n = Distributions.Discrete.hypergeometric 50 20 20
    let hypergeoDistribution_max_K = Distributions.Discrete.hypergeometric 50 50 20
    let hypergeoDistribution_max_n = Distributions.Discrete.hypergeometric 50 20 50
    let hypergeoDistribution_max_K_n = Distributions.Discrete.hypergeometric 50 50 50
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
            let N_isZero = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam 0 1 1
            let N_isNegative = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam -2 1 1
            let N_isPositive = Distributions.Discrete.hypergeoCheckParam 2 1 1
            //
            let K_isZero = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam 2 0 1
            let K_isNegative = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam 2 -2 1
            let K_positiveBiggerN = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam 2 3 1
            let K_positiveEqualN = Distributions.Discrete.hypergeoCheckParam 2 2 1
            let K_positiveSmallerN = Distributions.Discrete.hypergeoCheckParam 2 1 1
            //
            let n_isZero = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam 2 1 0
            let n_isNegative = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam 2 1 -2
            let n_positiveBiggerN = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam 2 1 3
            let n_positiveEqualN = Distributions.Discrete.hypergeoCheckParam 2 1 2
            let n_positiveSmallerN = Distributions.Discrete.hypergeoCheckParam 2 1 1
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
            let k_isNegative = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam_k 4 2 2 -2
            let k_isPositive = Distributions.Discrete.hypergeoCheckParam_k 4 2 2 1
            let k_isPositive_allEqual = Distributions.Discrete.hypergeoCheckParam_k 4 2 2 2
            let k_isPositiveBiggerN = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam_k 4 2 2 5
            let k_isPositiveBiggerK = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam_k 4 2 3 3
            let k_isPositiveBigger_n = fun (x:unit) -> Distributions.Discrete.hypergeoCheckParam_k 4 3 2 3
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
        test "PDF" {
            // test k = 0; Accuracy.medium, because online calculator has not enough decimal places.
            Expect.floatClose Accuracy.medium (hypergeoDistribution_basicCase.PDF 0) 0.00011894 "hyperDistribution_basicCase k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_K_equal_n.PDF 0) 0.0000006375 "hyperDistribution_K_equal_n k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K.PDF 0) 0. "hyperDistribution_max_K k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_n.PDF 0) 0. "hyperDistribution_max_n k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K_n.PDF 0) 0. "hyperDistribution_max_K_n k=0"
            // test any k 
            Expect.floatClose Accuracy.medium (hypergeoDistribution_basicCase.PDF 3) 0.20984 "hyperDistribution_basicCase k=3"
            // Accuracy.low, because online calculator has not enough decimal places.
            Expect.floatClose Accuracy.low (hypergeoDistribution_K_equal_n.PDF 6) 0.1196 "hyperDistribution_K_equal_n k=6"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K.PDF 10) 0. "hyperDistribution_max_K k=10"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_n.PDF 13) 0. "hyperDistribution_max_n k=44"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K_n.PDF 50) 1.0 "hyperDistribution_max_K_n k=50"
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
        // No idea what this is meant for, but its Syntax differs from Bernoulli.Support
        test "Support" {
            /// 40 20 5 do not matter as long as they don't fail "hypergeoCheckParam"
            let s = Distributions.Discrete.Hypergeometric.Support 40 20 5
            Expect.equal s (0., infinity) ""
        }
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
    testList "Distributions.exponential" [

        let createExpDistCDF  lambda x = FSharp.Stats.Distributions.Continuous.Exponential.CDF lambda x    
        let createExpDistPDF  lambda x = Distributions.Continuous.Exponential.PDF lambda x
        
        testCase "exp check param" <| fun () -> 
            Expect.throws (fun () -> (Distributions.Continuous.expCheckParam 0. )) "Should fail when lamda  =  0.0"
            Expect.throws (fun () -> (Distributions.Continuous.expCheckParam -3. )) "Should fail when lamda  < 0."
            Expect.throws (fun () -> (Distributions.Continuous.expCheckParam -infinity )) "Should fail when lamda < 0.0  "  
        
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

    let bernoulliDistribution_basicCase = Distributions.Discrete.bernoulli test_basicNumber
    let bernoulliDistribution_nan = Distributions.Discrete.bernoulli nan
    let bernoulliDistribution_zero = Distributions.Discrete.bernoulli 0.0
    let bernoulliDistribution_one = Distributions.Discrete.bernoulli 1.0

    // 2022-06-22
    // Wikipedia: https://de.wikipedia.org/wiki/Bernoulli-Verteilung#Definition 
    // "p is element of closed intervall between 0. and 1."
    testList "Distributions.Discrete.Bernoulli" [
        test "bernCheckParam" {
            let test_lowerThan0 = fun (x: unit) -> Distributions.Discrete.bernCheckParam -0.1
            let test_highterThan1 = fun (x: unit) -> Distributions.Discrete.bernCheckParam 1.1
            let test_basic = Distributions.Discrete.bernCheckParam test_basicNumber
            let test_zero = Distributions.Discrete.bernCheckParam 0.
            let test_one = Distributions.Discrete.bernCheckParam 1.
            let test_nan = Distributions.Discrete.bernCheckParam nan // 
            let test_infinity = fun (x: unit) -> Distributions.Discrete.bernCheckParam infinity
            let test_negativeInfinity = fun (x: unit) -> Distributions.Discrete.bernCheckParam -infinity
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
            let test_ZeroAndOne (bd: Distributions.Distribution<float,float>) = 
                let propabilitySuccess = bd.PDF 1.0
                let propabilityFailure = bd.PDF 0.0
                Expect.equal propabilitySuccess (bd.Mean) $"test_ZeroAndOne.propabilitySuccess for {bd.Mean}"
                Expect.floatClose Accuracy.high propabilityFailure (1.0 - bd.Mean) $"test_ZeroAndOne.propabilityFailure for {bd.Mean}"
            let test_ZeroPDFCases (bd: Distributions.Distribution<float,float>) =
                Expect.equal (bd.PDF 0.1) 0.0 $"test_ZeroPDFCases 0.1 for {bd.Mean}"
                Expect.equal (bd.PDF -0.1) 0.0 $"test_ZeroPDFCases -0.1 for {bd.Mean}"
                Expect.equal (bd.PDF 1.1) 0.0 $"test_ZeroPDFCases 1.1 for {bd.Mean}"
                Expect.equal (bd.PDF nan) 0.0 $"test_ZeroPDFCases nan for {bd.Mean}"
                Expect.equal (bd.PDF infinity) 0.0 $"test_ZeroPDFCases infinity for {bd.Mean}"
                Expect.equal (bd.PDF -infinity) 0.0 $"test_ZeroPDFCases -infinity for {bd.Mean}"
            test_ZeroPDFCases bernoulliDistribution_basicCase
            test_ZeroPDFCases bernoulliDistribution_nan
            test_ZeroPDFCases bernoulliDistribution_zero
            test_ZeroPDFCases bernoulliDistribution_one
            //
            test_ZeroAndOne bernoulliDistribution_basicCase
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.PDF 0.0)) $"test_ZeroAndOne.propabilitySuccess for nan"
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.PDF 1.0)) $"test_ZeroAndOne.propabilityFailure for nan"
            test_ZeroAndOne bernoulliDistribution_zero
            test_ZeroAndOne bernoulliDistribution_one
        }
        test "CDF" {
            // For P(x>=R) and R∈{0,1}, where R is the random outcome of the bernoulli distribution, any value below 0 has a probability of 0 to be greater or equal to R
            let test_ZeroCDFCases (bd: Distributions.Distribution<float,float>) = 
                Expect.equal (bd.CDF -0.1) 0.0 $"test_ZeroCDFCases -0.1 for {bd.Mean}"
                Expect.equal (bd.CDF -infinity) 0.0 $"test_ZeroCDFCases -infinity for {bd.Mean}"
                Expect.equal (bd.CDF nan) 0.0 $"test_ZeroCDFCases -infinity for {bd.Mean}"
            let test_OneCDFCases (bd: Distributions.Distribution<float,float>) = 
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
        // Tbh. i have no idea what this is for
        test "Support" {
            // insert any number which does not throw an error in "bernCheckParam".
            Expect.sequenceEqual (Distributions.Discrete.Bernoulli.Support 0.2) [0.0; 1.0] ""
        }
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
