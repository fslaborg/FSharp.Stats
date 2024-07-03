module DistributionsContinuousTests

open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Distributions.Continuous

// Defining an accuracy appropriate for testing random sampling and inference
let fittingAccuracy : Accuracy = {absolute= 0.1 ;relative= 0.1}


[<Tests>]
let GammaDistributionTests =

    testList "Distributions.Continuous.Gamma" [
        let alpha = 0.4 
        let beta  = 4.2
    
        let d     = Gamma.Init alpha beta

        let mean  = d.Mean     
        let var   = d.Variance 
        let cdfs  = [| 0.; 0.251017; 0.328997; 0.38435; 0.428371; 0.465289;
                       0.497226; 0.525426; 0.55069; 0.573571 |] 

        let pdfs = [| 0.987113653; 0.635929273; 0.486870787; 0.400046182; 0.341683319;
                      0.299071263; 0.266235685; 0.239955525; 0.218322701; 0.200126249;
                      0.184555971; 0.171046668; 0.159190450; 0.148684554; 0.139298865;
                      0.130854902; 0.123211796; 0.116256647; 0.109897748; 0.104059710;
                      0.098679897; 0.093705765; 0.089092854; 0.084803247; 0.080804376;
                      0.077068078; 0.073569861; 0.070288299; 0.067204554; 0.064301989;
                      0.061565838; 0.058982949; 0.056541557; 0.054231102; 0.052042076;
                      0.049965886; 0.047994748; 0.046121587; 0.044339960; 0.042643979;
                      0.041028256; 0.039487846; 0.038018205; 0.036615142; 0.035274793;
                      0.033993583; 0.032768200; 0.031595571; 0.030472842; 0.029397355;
                      0.028366635; 0.027378369; 0.026430398; 0.025520703; 0.024647389;
                      0.023808683; 0.023002918; 0.022228528; 0.021484040; 0.020768066;
                      0.020079300; 0.019416507; 0.018778524; 0.018164249; 0.017572643;
                      0.017002719; 0.016453546; 0.015924240; 0.015413961; 0.014921914;
                      0.014447344; 0.013989532; 0.013547795; 0.013121484; 0.012709981;
                      0.012312696; 0.011929068; 0.011558563; 0.011200670; 0.010854903;
                      0.010520795; 0.010197904; 0.009885805; 0.009584092; 0.009292377;
                      0.009010290; 0.008737475; 0.008473592; 0.008218316; 0.007971333;
                      0.007732346; 0.007501068; 0.007277223; 0.007060548; 0.006850789;
                      0.006647704; 0.006451059; 0.006260630; 0.006076203; 0.005897569; |]

        
        //testCase "Mean" <| fun () ->
        //    Expect.floatClose Accuracy.high mean 0.21105527638190955 "Mean should be equal"

        //testCase "Variance" <| fun () ->
        //    Expect.floatClose Accuracy.high var 0.055689279830523512 "Variance should be equal"
                
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.Gamma.Init 3. 0.3).Parameters with
                | Gamma x -> x.Alpha,x.Beta
                | _ -> (nan,nan)
            Expect.equal param (3.,0.3) "Distribution parameters are incorrect."
            
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
            let observations = Array.init 999999 (fun _ -> float (Continuous.Gamma.Sample alpha beta))
            let alpha',beta' = Continuous.Gamma.Fit observations
            
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
            let alpha, beta = Continuous.Gamma.Fit observations
            //let mean = 1238.8734170854279
            let alpha' = 41566.439533445438
            let beta'  = 0.029804655654680219
            
            Expect.floatClose fittingAccuracy alpha alpha'
                "Gamma Distribution Fit" 
            Expect.floatClose fittingAccuracy beta beta'
                "Gamma Distribution Fit" 
   
    ]


[<Tests>]
let BetaDistributionTests =
    testList "Distributions.Continuous.Beta" [
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.Beta.Init 3. 0.3).Parameters with
                | Beta x -> x.Alpha,x.Beta
                | _ -> (nan,nan)
            Expect.equal param (3.,0.3) "Distribution parameters are incorrect."
        
        let pdf_expect1 = 0.550369534108
        let pdf_expect2 = 5.58793544769e-08
        let pdf_expect3 = 30.
        let pdf_expect4 = 0.
        let pdf_expect5 = 0.
        let pdf_expect6 = 600
        let pdf_expect7 = 0
        let pdf_expect8 = 0
        let pdf_expect9 = 2.76522710171e-199
        let pdf_expect10 = 0.000725971756359

        let cdf_expect1 = 0.011909896429
        let cdf_expect2 = 0.999999999069
        let cdf_expect3 = 0.
        let cdf_expect4 = 1.
        let cdf_expect5 = 0.
        let cdf_expect6 = 1.
        let cdf_expect7 = 0.
        let cdf_expect8 = 1.
        let cdf_expect9 = 0.544007501411
        let cdf_expect10 = 1.
            
        // tested against R dbeta
        testCase "PDF" <| fun () ->

            let pdf_actual1 = (Beta.Init 50. 30.).PDF 0.5
            let pdf_actual2 = (Beta.Init 1. 30.).PDF 0.5
            let pdf_actual3 = (Beta.Init 1. 30.).PDF 0.
            let pdf_actual4 = (Beta.Init 1. 3.).PDF 1.
            let pdf_actual5 = (Beta.Init 600. 1.).PDF 0.
            let pdf_actual6 = (Beta.Init 600. 1.).PDF 1.
            let pdf_actual7 = (Beta.Init 600. 800.).PDF 0.
            let pdf_actual8 = (Beta.Init 600. 800.).PDF 1.
            let pdf_actual9 = (Beta.Init 600. 800.).PDF 0.11
            let pdf_actual10 = (Beta.Init 600. 800.).PDF 0.49
            Expect.floatClose Accuracy.high pdf_actual1 pdf_expect1 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual2 pdf_expect2 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual3 pdf_expect3 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual4 pdf_expect4 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual5 pdf_expect5 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual6 pdf_expect6 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual7 pdf_expect7 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual8 pdf_expect8 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual9 pdf_expect9 "Beta PDF was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual10 pdf_expect10 "Beta PDF was not determined correctly."
        
        // tested against R dbeta
        testCase "PDFLn" <| fun () ->
            
            let pdf_actual1 = (Beta.PDFLn 50. 30. 0.5)  |> exp
            let pdf_actual2 = (Beta.PDFLn 1. 30. 0.5)   |> exp
            let pdf_actual3 = (Beta.PDFLn 1. 30. 0.)    |> exp
            let pdf_actual4 = (Beta.PDFLn 1. 3. 1.)     |> exp
            //higher alpha and beta values are called already when PDF is used
            Expect.floatClose Accuracy.high pdf_actual1 pdf_expect1 "Beta PDFLn was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual2 pdf_expect2 "Beta PDFLn was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual3 pdf_expect3 "Beta PDFLn was not determined correctly."
            Expect.floatClose Accuracy.high pdf_actual4 pdf_expect4 "Beta PDFLn was not determined correctly."
            
        // tested against R pbeta
        testCase "CDF" <| fun () ->
            let cdf_actual1 = (Beta.Init 50. 30.).CDF 0.5
            let cdf_actual2 = (Beta.Init 1. 30.).CDF 0.5
            let cdf_actual3 = (Beta.Init 1. 30.).CDF 0.
            let cdf_actual4 = (Beta.Init 1. 3.).CDF 1.
            let cdf_actual5 = (Beta.Init 600. 1.).CDF 0.
            let cdf_actual6 = (Beta.Init 600. 1.).CDF 1.
            let cdf_actual7 = (Beta.Init 600. 800.).CDF 0.
            let cdf_actual8 = (Beta.Init 600. 800.).CDF 1.
            let cdf_actual9 = (Beta.Init 600. 800.).CDF 0.43
            let cdf_actual10 = (Beta.Init 600. 800.).CDF 1.49

            Expect.floatClose Accuracy.high cdf_actual1 cdf_expect1 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual2 cdf_expect2 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual3 cdf_expect3 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual4 cdf_expect4 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual5 cdf_expect5 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual6 cdf_expect6 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual7 cdf_expect7 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual8 cdf_expect8 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual9 cdf_expect9 "Beta CDF was not determined correctly."
            Expect.floatClose Accuracy.high cdf_actual10 cdf_expect10 "Beta CDF was not determined correctly."
    
    
        let alpha = 0.42 
        let beta  = 1.57
    
        let d     = Continuous.Beta.Init alpha beta

        let mean  = Continuous.Beta.Mean alpha beta     // 0.21105527638190955
        let var   = d.Variance // 0.055689279830523512
        let cdf   = d.CDF 0.27 // 0.69358638272337991
        let pdf   = d.PDF 0.27 // 0.94644031936694828


        
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
let chiSquaredTests =
    testList "ChiSquaredTests" [
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.ChiSquared.Init 3.).Parameters with
                | ChiSquared x -> x.DOF
                | _ -> (nan)
            Expect.equal param 3. "Distribution parameters are incorrect."
            
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
                Expect.isTrue (Ops.isNan testCase) "Should be NaN"
            testCase "CDF.testCaseDof1XNan" <| fun () ->
                let testCase = 1. - (Continuous.ChiSquared.CDF 1. nan)
                Expect.isTrue (Ops.isNan testCase) "Should be NaN"
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
                let testCase = Continuous.ChiSquared.Init -1.
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
                let testCase = Continuous.ChiSquared.Init -infinity
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
                let testCase = Continuous.ChiSquared.Init nan
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
                let testCase = Continuous.ChiSquared.Init 0.
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
                let testCase = Continuous.ChiSquared.Init 1.
                Expect.floatClose Accuracy.veryHigh testCase.Mean 1. "Should be equal"
                Expect.floatClose Accuracy.veryHigh testCase.Variance 2. "Should be equal"
                Expect.floatClose Accuracy.veryHigh testCase.StandardDeviation (sqrt 2.) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF 0.) 0. "Should be equal"
                Expect.floatClose Accuracy.medium (testCase.CDF 1.) 0.682689 "Should be equal"
                Expect.floatClose Accuracy.low (testCase.CDF 10.) 0.998 "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF infinity) 1. "Should be equal"
                Expect.isTrue (testCase.CDF -1. |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.CDF -infinity |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.CDF nan |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 0. = infinity) "Should be equal"
                Expect.floatClose Accuracy.medium (testCase.PDF 1.) 0.24197 "Should be equal"
                Expect.floatClose Accuracy.low (testCase.PDF 10.) 0.00085 "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -1.) 0. "Should be equal"
                Expect.isTrue (Ops.isNan <| testCase.PDF nan) "Should be equal"
            testCase "chiSquaredInfinity" <| fun () ->
                let testCase = Continuous.ChiSquared.Init infinity
                Expect.isTrue (testCase.Mean = infinity) "Should be equal"
                Expect.isTrue (testCase.Variance = infinity) "Should be equal"
                Expect.isTrue (testCase.StandardDeviation = infinity) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF 0.) 0. "Should be equal"
                Expect.isTrue (testCase.CDF 1. |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.CDF 10. |> Ops.isNan) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.CDF infinity) 1. "Should be equal"
                Expect.isTrue (testCase.CDF -1. |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.CDF -infinity |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.CDF nan |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 0. |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 1. |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.PDF 10. |> Ops.isNan) "Should be equal"
                Expect.isTrue (testCase.PDF infinity |> Ops.isNan) "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -infinity) 0. "Should be equal"
                Expect.floatClose Accuracy.veryHigh (testCase.PDF -1.) 0. "Should be equal"
                Expect.isTrue (Ops.isNan <| testCase.PDF nan) "Should be equal"
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
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.Chi.Init 3.).Parameters with
                | Chi x -> x.DOF
                | _ -> nan
            Expect.equal param 3. "Distribution parameters are incorrect."
            
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
    let mvn = Continuous.MultivariateNormal.Init (vector [0.;0.;0.;0.;0.]) (Matrix.identity 5)
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
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.MultivariateNormal.Init (vector [1.;0.4]) (matrix [[0.3;2.3];[1.2;4.3]])).Parameters with
                | MultivariateNormal x -> x.Mean,x.StandardDeviation
                | _ -> (vector [],matrix[])
            Expect.equal param ((vector [1.;0.4]),(matrix [[0.3;2.3];[1.2;4.3]])) "Distribution parameters are incorrect."
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
let normalTests =

    testList "Distributions.Continuous.Normal" [
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.Normal.Init 0.1 0.6).Parameters with
                | Normal x -> x.Mean,x.StandardDeviation
                | _ -> (nan,nan)
            Expect.equal param (0.1,0.6) "Distribution parameters are incorrect."
        testCase "InvCDF" <| fun () ->
            //tested against Wichura, Algorithm AS 241: The Percentage Points of the Normal Distribution., 1988 
            let expected_1 = -0.6744897501960817
            let actual___1 = Distributions.Continuous.Normal.InvCDF 0. 1. 0.25
            let expected_2 = -3.090232306167814
            let actual___2 = Distributions.Continuous.Normal.InvCDF 0. 1. 0.001
            let expected_3 = -9.262340089798408
            let actual___3 = Distributions.Continuous.Normal.InvCDF 0. 1. 1e-20
            let expected_4 = infinity
            let actual___4 = Distributions.Continuous.Normal.InvCDF -300. 100. 1
            let expected_5 = -infinity
            let actual___5 = Distributions.Continuous.Normal.InvCDF -300. 100. 0
            let expected_6 = -300_000.
            let actual___6 = Distributions.Continuous.Normal.InvCDF -300_000. 5000. 0.5
            // tested against python scipy.stats.norm.ppf()
            let expected_7 = -288368.2606297958
            let actual___7 = Distributions.Continuous.Normal.InvCDF -300_000. 5000. 0.99
            
            Expect.floatClose Accuracy.high actual___1 expected_1 "InvCDF1 gives wrong result" 
            Expect.floatClose Accuracy.high actual___2 expected_2 "InvCDF2 gives wrong result" 
            Expect.floatClose Accuracy.high actual___3 expected_3 "InvCDF3 gives wrong result" 
            Expect.equal actual___4 expected_4 "InvCDF4 gives wrong result" 
            Expect.equal actual___5 expected_5 "InvCDF5 gives wrong result" 
            Expect.floatClose Accuracy.high actual___6 expected_6 "InvCDF6 gives wrong result" 
            Expect.floatClose Accuracy.high actual___7 expected_7 "InvCDF7 gives wrong result"
        
                      
    ] 

[<Tests>]
let logNormalTests =

    testList "Distributions.Continuous.LogNormal" [
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.LogNormal.Init 0.1 0.6).Parameters with
                | LogNormal x -> x.Mean,x.StandardDeviation
                | _ -> (nan,nan)
            Expect.equal param (0.1,0.6) "Distribution parameters are incorrect."
        testCase "InvCDF" <| fun () ->
            //tested against qlnorm from R
            let expected_1 = 0.5094162838632775
            let actual___1 = Distributions.Continuous.LogNormal.InvCDF 0. 1. 0.25
            let expected_2 = 0.04549138524765352
            let actual___2 = Distributions.Continuous.LogNormal.InvCDF 0. 1. 0.001
            let expected_3 = 9.493291347224372e-05
            let actual___3 = Distributions.Continuous.LogNormal.InvCDF 0. 1. 1e-20
            let expected_4 = infinity
            let actual___4 = Distributions.Continuous.LogNormal.InvCDF 300. 100. 1
            let expected_5 = 0.
            let actual___5 = Distributions.Continuous.LogNormal.InvCDF 300. 100. 0
            let expected_6 = 10686474581524.46
            let actual___6 = Distributions.Continuous.LogNormal.InvCDF 30. 5000. 0.5
            
            Expect.floatClose Accuracy.high actual___1 expected_1 "InvCDF1 gives wrong result" 
            Expect.floatClose Accuracy.high actual___2 expected_2 "InvCDF2 gives wrong result" 
            Expect.floatClose Accuracy.high actual___3 expected_3 "InvCDF3 gives wrong result" 
            Expect.equal actual___4 expected_4 "InvCDF4 gives wrong result" 
            Expect.equal actual___5 expected_5 "InvCDF5 gives wrong result" 
            Expect.floatClose Accuracy.high actual___6 expected_6 "InvCDF6 gives wrong result"         
    ] 






[<Tests>]
let FDistributionTests =
    // Values taken from R 4.0.3 and Wolfram alpha 23.06.2022
    // Weisstein, Eric W. "F-Distribution." From MathWorld--A Wolfram Web Resource. https://mathworld.wolfram.com/F-Distribution.html

    testList "Distributions.Continuous.F" [
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.F.Init 3 4).Parameters with
                | F x -> x.DOF1,x.DOF2
                | _ -> (nan,nan)
            Expect.equal param (3.,4.) "Distribution parameters are incorrect."
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
                ((Ops.isNan testcase)&& Ops.isNan(r_value)&&Ops.isNan(testcase2)&&Ops.isNan(testcase3))
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
                ((Ops.isNan testcase)&& Ops.isNan(r_value))
                (sprintf "Continuous.F.Mean with dof<=2 does not return nan %A" testcase  )
        
        testCase "Continuous.F.Mean_dof1&2=Infininty" <| fun () ->
            let dof1 = infinity
            let dof2 = infinity
            let testcase    = Continuous.F.Mean dof1 dof2
            let r_value     = nan
            Expect.isTrue
                ((Ops.isNan testcase)&& Ops.isNan(r_value))
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
                List.map(fun dof2 -> Continuous.F.Variance dof1 dof2 |> Ops.isNan)
            let r_value     = nan

            Expect.isTrue
                (Ops.isNan(r_value)&& (List.contains false testcase|> not))
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
                List.map(fun dof2 -> Continuous.F.StandardDeviation dof1 dof2 |> Ops.isNan)
            let r_value     = nan

            Expect.isTrue
                (Ops.isNan(r_value)&& (List.contains false testcase|> not))
                (sprintf "Continuous.F.Variance with dof<=2 does not return nan")
        
        testCase "Continuous.F.Sample" <| fun () ->
            let dof1        = 10000.
            let dof2        = 10000.
            let testcase    = 
                [for i=0 to 10000 do Continuous.F.Sample dof1 dof2]
                |> List.mean
                |> Ops.roundTo 5
                
            let r_value     = 
                Ops.roundTo 5 (1.000359)

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
                (Ops.isNan(testcase_1)&&Ops.isNan(r_value_1))
                "Continuous.F.CDF with dof2=infinity does not yield the expected value"
            Expect.isTrue
                (Ops.isNan(testcase_2)&&Ops.isNan(r_value_2))
                "Continuous.F.CDF with dof1=infinity does not yield the expected value"
            Expect.isTrue
                (Ops.isNan(testcase_3)&&Ops.isNan(r_value_3))
                "Continuous.F.CDF with dof1&dof2=infinity does not yield the expected value"

        testCase "Continuous.F.Support" <| fun () ->
            let dof1            = 10.
            let dof2            = 25.
            let testcase    = 
                Continuous.F.Support dof1
            let r_value     = Interval.CreateRightOpen<float>(0., System.Double.PositiveInfinity)

            Expect.isTrue
                ((testcase.GetStart() = r_value.GetStart()) &&
                ( testcase.GetEnd() =  r_value.GetEnd()))
                "Continuous.F.Support does not return the expected Tupel"
        
        testCase "Continuous.F.Support_infinity" <| fun () ->
            let dof1            = infinity
            let dof2            = infinity
            let testcase    = 
                Continuous.F.Support dof1
            let r_value     = Interval.CreateRightOpen<float>(0., Double.PositiveInfinity)

            Expect.isTrue
                ((testcase.GetStart() = r_value.GetStart()) &&
                (testcase.GetEnd() = r_value.GetEnd()))
                "Continuous.F.Support does not return the expected Tupel"

        testCase "Continuous.F.Support_when_dof1_equals_1" <| fun() ->
            // Arrange
            let dof1 = 1.0

            // Act
            let result =
                F.Support dof1

            // Assert
            match result with
            | Interval.Open (start, end_) ->
                Expect.equal
                    start
                    0.0
                    "Expectation on \"Start\":"
                Expect.equal
                    end_
                    Double.PositiveInfinity
                    "Expectation on \"End\":"
            | other ->
                Expect.isTrue
                    false
                    (sprintf "Expected a fully open range (true), but it was %A (false)" other)

        testCase "Continuous.F.Support_when_dof1_is_not_equal_to_1" <| fun() ->
            // Arrange
            let dof1 = 2.0

            // Act
            let result =
                F.Support dof1

            // Assert
            match result with
            | Interval.RightOpen (start, end_) ->
                Expect.equal
                    start
                    0.0
                    "Expectation on \"Start\":"
                Expect.equal
                    end_
                    Double.PositiveInfinity
                    "Expectation on \"End\":"
            | other ->
                Expect.isTrue
                    false
                    (sprintf "Expected a right open range (true), but it was %A (false)" other)

    ]
    


let exponentialTests =
    // references is R V. 2022.02.3 Build 492
    // PDF is used with expPDF <- dexp(3,0.59)
    // CDF is created with expCDF <- pexp(3, 0.59)
    testList "Distributions.Continuous.Exponential" [
        testCase "Parameters" <| fun () ->
            let param = 
                match (Continuous.Exponential.Init 4.4).Parameters with
                | Exponential x -> x.Lambda
                | _ -> nan
            Expect.equal param (4.3) "Distribution parameters are incorrect."

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