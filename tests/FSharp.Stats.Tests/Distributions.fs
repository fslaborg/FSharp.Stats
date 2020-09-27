namespace FSharp.Stats.Tests
open Expecto

module DistributionsTests =
    open System
    open FSharp.Stats.Distributions
    open Distance.OneDimensional
    [<Tests>]
    let testDistanceFunctions =
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
    let testChiSquared =
        //TestCases from Williams RBG, Introduction to Statistics for Geographers and Earth Scientist, 1984, DOI 10.1007/978-1-349-06815-9 p 333
        testList "Distributions.ChiSquared" [
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
        ]

    [<Tests>]
    let testStudentizedRange =
        //TestCases from Lawal B, Applied Statistical Methods in Agriculture, Health and Life Sciences, DOI 10.1007/978-3-319-05555-8, 2014
        testList "Distributions.studentizedRange" [
            testCase "CDF.testCase_0.95_1" <| fun () ->
                let testCase = 1. - (Continuous.StudentizedRange.CDF 3.46 2. 6. 1. None true)
                Expect.isTrue (Math.Round(testCase,4) = 0.05) "Should be equal"
            testCase "CDF.testCase_0.95_2" <| fun () ->
                let testCase = 1. - (Continuous.StudentizedRange.CDF 2.83 2. 60. 1. None true)
                Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"
            testCase "CDF.testCase_0.95_3" <| fun () ->
                let testCase = 1. - (Continuous.StudentizedRange.CDF 7.59 20. 6. 1. None true)
                Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"
            testCase "CDF.testCase_0.95_4" <| fun () ->
                let testCase = 1. - (Continuous.StudentizedRange.CDF 5.24 20. 60. 1. None true)
                Expect.isTrue (Math.Round(testCase,3) = 0.05) "Should be equal"            
        ]
