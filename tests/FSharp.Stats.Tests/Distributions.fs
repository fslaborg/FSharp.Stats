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
