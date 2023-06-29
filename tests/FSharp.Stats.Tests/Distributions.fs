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



//[<Tests>]
//let GammaDistributionTests =

//    let alpha = 0.4 
//    let beta  = 4.2
    
//    let d     = ContinuousDistribution.gamma alpha beta

//    let mean  = d.Mean     
//    let var   = d.Variance 
//    let cdfs  = [| 0.; 0.251017; 0.328997; 0.38435; 0.428371; 0.465289;
//                   0.497226; 0.525426; 0.55069; 0.573571 |] 

//    let pdfs = [| 0.987114; 0.635929; 0.486871; 0.400046; 0.341683;
//                  0.299071; 0.266236; 0.239956; 0.218323; 0.200126; |]



//    testList "Distributions.Continuous.Gamma" [
        
//        //testCase "Mean" <| fun () ->
//        //    Expect.floatClose Accuracy.high mean 0.21105527638190955 "Mean should be equal"

//        //testCase "Variance" <| fun () ->
//        //    Expect.floatClose Accuracy.high var 0.055689279830523512 "Variance should be equal"
                
//        testCase "Cdfs" <| fun () ->
//            cdfs 
//            |> Array.iteri (fun i v ->
//                let cdf = d.CDF (float i / 10.0)
//                Expect.floatClose Accuracy.low cdf cdfs[i] "Cdf should be equal"
//                )
                 
//        testCase "Pdfs" <| fun () ->
//            cdfs 
//            |> Array.iteri (fun i v ->
//                let pdf = d.PDF ((float i + 1.) / 10.0)
//                Expect.floatClose Accuracy.low pdf pdfs[i] "Cdf should be equal"
//                )          
           
//        //testCase "Pdf" <| fun () ->
//        //    Expect.floatClose Accuracy.high pdf 0.987114 "Pdf should be equal"
        
//        testCase "FitTest" <| fun () ->
//            let observations = Array.init 999999 (fun _ -> float (Continuous.Gamma.Sample alpha beta))
//            let alpha',beta' = Continuous.Gamma.Fit observations
            
//            Expect.floatClose fittingAccuracy alpha alpha' 
//                "alpha" 
//            Expect.floatClose fittingAccuracy beta beta' 
//                "beta"
    
//        testCase "FitTest_from_observations" <| fun () ->
//            let observations = [| 1275.56; 1239.44; 1237.92; 1237.22; 1237.1; 1238.41; 1238.62; 1237.05;
//                1237.19; 1236.51; 1264.6; 1238.19; 1237.39; 1235.79; 1236.53; 1236.8; 1238.06; 
//                1236.5; 1235.32; 1236.44; 1236.58; 1236.3; 1237.91; 1238.6; 1238.49; 1239.21; 
//                1238.57; 1244.63; 1236.06; 1236.4; 1237.88; 1237.56; 1236.66; 1236.59; 1236.53; 
//                1236.32; 1238.29; 1237.79; 1237.86; 1236.42; 1236.23; 1236.37; 1237.18; 1237.63; 
//                1245.8; 1238.04; 1238.55; 1238.39; 1236.75; 1237.07; 1250.78; 1238.6; 1238.36; 
//                1236.58; 1236.82; 1238.4; 1257.68; 1237.78; 1236.52; 1234.9; 1237.9; 1238.58; 
//                1238.12; 1237.89; 1236.54; 1236.55; 1238.37; 1237.29; 1237.64; 1236.8; 1237.73; 
//                1236.71; 1238.23; 1237.84; 1236.26; 1237.58; 1238.31; 1238.4; 1237.08; 1236.61; 
//                1235.92; 1236.41; 1237.89; 1237.98; 1246.75; 1237.92; 1237.1; 1237.97; 1238.69; 
//                1237.05; 1236.96; 1239.44; 1238.49; 1237.88; 1236.01; 1236.57; 1236.44; 1235.76; 
//                1237.62; 1238; 1263.14; 1237.66; 1237; 1236; 1261.96; 1238.58; 1237.77; 1237.06; 
//                1236.31; 1238.63; 1237.23; 1236.85; 1236.23; 1236.46; 1236.9; 1237.85; 1238; 
//                1237.02; 1236.19; 1236.05; 1235.73; 1258.3; 1235.98; 1237.76; 1246.93; 1239.1; 
//                1237.72; 1237.67; 1236.79; 1237.61; 1238.41; 1238.29; 1238.11; 1237; 1236.52; 
//                1236.6; 1236.31; 1237.77; 1238.58; 1237.88; 1247.35; 1236.14; 1236.83; 1236.15; 
//                1237.93; 1238.16; 1237.34; 1236.78; 1238.66; 1237.76; 1237.19; 1236.7; 1236.04; 
//                1236.66; 1237.86; 1238.54; 1238.05; 1238.41; 1236.94; 1240.95; 1261.01; 1237.72; 
//                1237.91; 1238.2; 1235.68; 1236.89; 1235.12; 1271.31; 1236.97; 1270.76; 1238.52; 
//                1238.19; 1238.6; 1237.16; 1236.72; 1236.71; 1237.14; 1238.48; 1237.95; 1237.42; 
//                1235.86; 1236.39; 1236.13; 1236.58; 1237.95; 1237.76; 1237.39; 1238.16; 1236.31; 
//                1236.41; 1236.12; 1238.7; 1236.48; 1237.84; 1236.38; 1237.95; 1238.48; 1236.51; 
//                1236.56 |]
//            let alpha, beta = Continuous.Gamma.Fit observations
//            //let mean = 1238.8734170854279
//            let alpha' = 41566.439533445438
//            let beta'  = 0.029804655654680219
            
//            Expect.floatClose fittingAccuracy alpha alpha'
//                "Gamma Distribution Fit" 
//            Expect.floatClose fittingAccuracy beta beta'
//                "Gamma Distribution Fit" 
//    //0.10000000000000000555; relative=0.10000000000000000555}, 
//    //but was 1238.8734068085332183. actual=1.0276894821207402346e-05 expected=1238.8734170854279455
    
//    ]