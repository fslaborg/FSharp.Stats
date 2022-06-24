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
            //testCase "CDF.testCaseDof1XNan" <| fun () ->
            //    let testCase = 1. - (Continuous.ChiSquared.CDF 1. nan)
            //    Expect.isTrue (isNan testCase) "Should be NaN"
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
                //Expect.floatClose Accuracy.veryHigh (testCase.CDF nan) 0. "Should be equal"
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
                //Expect.isTrue (testCase.CDF nan = infinity) "Should be equal"
                Expect.isTrue (testCase.PDF 0. |> isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 1. |> isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 10. |> isNan) "Should be equal"
                Expect.isTrue (testCase.PDF infinity |> isNan) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -1.) 0. "Should be equal"
                Expect.isTrue (isNan <| testCase.PDF nan) "Should be equal"
        ]
    ]

[<Tests>]
let studentizedRangeTests =
    //TestCases from critical q value tables from: Lawal B, Applied Statistical Methods in Agriculture, Health and Life Sciences, DOI 10.1007/978-3-319-05555-8, 2014
    testList "Distributions.studentizedRange" [
        testCase "CDF.testCase_0.95_1" <| fun () ->
            let testCase = 1. - (Continuous.StudentizedRange.CDF 3.46 2. 6. 1. None false)
            Expect.isTrue (Math.Round(testCase,4) = 0.05) "Should be equal"
        testCase "CDF.testCase_0.95_2" <| fun () ->
            let testCase = 1. - (Continuous.StudentizedRange.CDF 2.83 2. 60. 1. None false)
            Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"
        testCase "CDF.testCase_0.95_3" <| fun () ->
            let testCase = 1. - (Continuous.StudentizedRange.CDF 7.59 20. 6. 1. None false)
            Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"
        testCase "CDF.testCase_0.95_4" <| fun () ->
            let testCase = 1. - (Continuous.StudentizedRange.CDF 5.24 20. 60. 1. None false)
            Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"            
    //TestCases from R ptukey(q, 4, 36, nranges = 1, lower.tail = TRUE, log.p = FALSE)
    //https://keisan.casio.com/exec/system/15184848911695
        testCase "CDF.testCase_r1" <| fun () ->
            let testCase = Continuous.StudentizedRange.CDF 3. 4. 36. 1. None false
            Expect.floatClose Accuracy.medium testCase 0.8342594 "Should be equal"
        testCase "CDF.testCase_r2" <| fun () ->
            let testCase = Continuous.StudentizedRange.CDF 6. 4. 36. 1. None false
            Expect.floatClose Accuracy.medium testCase 0.9991826 "Should be equal"
        testCase "CDF.testCase_r3" <| fun () ->
            let testCase = Continuous.StudentizedRange.CDF 9. 4. 36. 1. None false
            Expect.floatClose Accuracy.medium testCase 0.9999987 "Should be equal"
        testCase "CDF.testCase_r4" <| fun () ->
            let testCase = Continuous.StudentizedRange.CDF 11. 4. 36. 1. None false
            Expect.floatClose Accuracy.medium testCase 1. "Should be equal"         
    ]

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