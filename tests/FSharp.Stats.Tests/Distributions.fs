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