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
let FDistributionTests =
    // Values taken from R 4.0.3 and Wolfram alpha 23.06.2022
    // Weisstein, Eric W. "F-Distribution." From MathWorld--A Wolfram Web Resource. https://mathworld.wolfram.com/F-Distribution.html

    testList "Distributions.Continuous.F" [
        testCase "fCheckParam_dof1<0" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckParam -0.5 420.))
                "fCheckParam does not fail with dof1<0"
        
        testCase "fCheckParam_dof2<0" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckParam 420. -0.5))
                "fCheckParam does not fail with dof2<0"
        
        testCase "fCheckParam_dof1=0" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckParam 0. 420.))
                "fCheckParam does not fail with dof1<0"
        
        testCase "fCheckParam_dof2=0" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckParam 420. 0.))
                "fCheckParam does not fail with dof2<0"

        testCase "fCheckParam_dof1=nan" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckParam nan 420.))
                "fCheckParam does not fail with dof1=nan"
        
        testCase "fCheckParam_dof2=nan" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckParam 420. nan))
                "fCheckParam does not fail with dof2=nan"
     
        testCase "fCheckParam_dof1=-infinity" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckParam -infinity 420.))
                "fCheckParam does not fail with dof1=-infinity"
        
        testCase "fCheckParam_dof2=-infinity" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckParam 420. -infinity))
                "fCheckParam does not fail with dof2=-infinity"
 
        testCase "fCheckParam_dof1=infinity" <| fun () ->
            Expect.isTrue
                ((Continuous.fCheckParam infinity 420.|> fun x -> true))
                "fCheckParam does fail with dof1=infinity"
        
        testCase "fCheckParam_dof2=infinity" <| fun () ->
            Expect.isTrue
                ((Continuous.fCheckParam 420. infinity|> fun x -> true))
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
        
        testCase "fCheckX" <| fun () ->
            Expect.throws
                (fun () -> (Continuous.fCheckX -10.))
                "fCheckX does not fail with negative values"
            Expect.throws
                (fun () -> (Continuous.fCheckX nan))
                "fCheckX does not fail with nan"
            Expect.throws
                (fun () -> (Continuous.fCheckX -infinity))
                "fCheckX does not fail with -infinity"
            Expect.isTrue
                ((Continuous.fCheckX 10.)|> fun x -> true)
                "fCheckX fails with positive values"
            Expect.isTrue
                ((Continuous.fCheckX 0)|> fun x -> true)
                "fCheckX fails with 0"
            Expect.isTrue
                ((Continuous.fCheckX infinity)|> fun x -> true)
                "fCheckX fails with infinity"
                
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

