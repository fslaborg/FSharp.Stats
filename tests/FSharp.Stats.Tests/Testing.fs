module TestingTests
open Expecto
open System
open FSharp.Stats.Testing
open FSharp.Stats
open TestExtensions


[<Tests>]
let testPostHocTests =
    //Tests taken from:
    //https://www.icalcu.com/stat/anova-tukey-hsd-calculator.html
    testList "Testing.PostHoc" [
        (*
        // Test ommitted due to extremely long runtime of CodeCov.
        testCase "tukeyHSD" <| fun () ->
            let dataA = [|3.;3.;4.;5.;2.;5.;5.;4.;4.;2.;2.;2.;4.;3.;5.;3.;4.;5.;3.;5.;                   |]
            let dataB = [|10.;7.;9.;6.;7.;7.;6.;7.;10.;7.;8.;8.;8.;6.;10.;9.;9.;6.;9.;8.;                |]
            let dataC = [|6.;5.;6.;4.;4.;6.;1.;4.;6.;5.;4.;7.;4.;2.;1.;1.;3.;4.;5.;3.;                   |]
            let dataD = [|10.;5.;6.;5.;8.;5.;6.;9.;3.;10.;5.;9.;5.;5.;6.;10.;9.;6.;9.;10.;               |]
            let dataE = [|14.;17.;14.;13.;18.;12.;17.;11.;12.;11.;12.;10.;17.;19.;18.;18.;15.;14.;18.;16.|]

            let data = [|dataA;dataB;dataC;dataD;dataE|]
                
            let contrastMatrix = 
                [|                
                    //[|-1.;1.;0.;0.;0.;|] pvalue = zero
                    [|-1.;0.;1.;0.;0.;|]
                    [|-1.;0.;0.;1.;0.;|]
                    //[|-1.;0.;0.;0.;1.;|] pvalue = zero
                    [|0.;-1.;1.;0.;0.;|]
                    [|0.;-1.;0.;1.;0.;|]
                    //[|0.;-1.;0.;0.;1.;|] pvalue = zero
                    [|0.;0.;-1.;1.;0.;|]
                    //[|0.;0.;-1.;0.;1.;|] pvalue = zero
                    //[|0.;0.;0.;-1.;1.;|] pvalue = zero
                |]

            let pValues = 
                PostHoc.tukeyHSD contrastMatrix data 
                |> Array.map (fun x -> x.Significance)

            //pvalues from R: TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)
            let rpval = [0.9685630;0.0000045;0.0000003;0.7072882;0.0000618]
                
            Expect.floatClose Accuracy.low rpval.[0] pValues.[0] "p values should be equal."
            Expect.floatClose Accuracy.low rpval.[1] pValues.[1] "p values should be equal."
            Expect.floatClose Accuracy.low rpval.[2] pValues.[2] "p values should be equal."
            Expect.floatClose Accuracy.low rpval.[3] pValues.[3] "p values should be equal."
            Expect.floatClose Accuracy.low rpval.[4] pValues.[4] "p values should be equal."
        *)
        testCase "dunnett" <| fun () ->
            let data = 
                [|
                    [|1.84;2.49;1.50;2.42;|]
                    [|2.43;1.85;2.42;2.73;|]
                    [|3.95;3.67;3.23;2.31;|]
                    [|3.21;3.20;2.32;3.30;|]
                    [|3.21;3.13;2.32;3.30;3.20;2.42;|]
                |]

            //first sample is control
            let contrastMatrix = 
                [|                
                    [|-1.;1.;0.;0.;0.|]
                    [|-1.;0.;1.;0.;0.|]
                    [|-1.;0.;0.;1.;0.|]
                    [|-1.;0.;0.;0.;1.|]
                |]

            let dunnettResult = 
                PostHoc.dunnetts contrastMatrix data Tables.dunnettsTwoSided095

            //result from: SPSS Dunnett's test version 27
            let pval = [0.811;0.010;0.050;0.049]
            let dmean = [0.295;1.2275;0.945;0.8675]
                
            Expect.equal dunnettResult.[0].Significance (pval.[0]<0.05) "Significance should be equal."
            Expect.equal dunnettResult.[1].Significance (pval.[1]<0.05) "Significance should be equal."
            Expect.equal dunnettResult.[2].Significance (pval.[2]<0.05) "Significance should be equal."
            Expect.equal dunnettResult.[3].Significance (pval.[3]<0.05) "Significance should be equal."
            Expect.floatClose Accuracy.high dunnettResult.[0].L dmean.[0] "Mean differences should be equal."
            Expect.floatClose Accuracy.high dunnettResult.[1].L dmean.[1] "Mean differences should be equal."
            Expect.floatClose Accuracy.high dunnettResult.[2].L dmean.[2] "Mean differences should be equal."
            Expect.floatClose Accuracy.high dunnettResult.[3].L dmean.[3] "Mean differences should be equal."
    ]

[<Tests>]
let hTestTests = 
    // H-Test with ties tested against r implementation kruskal.test(weight ~ group, data = my_data)
    let groupA = [4.17; 5.18;  5.18;  6.11;  4.50;  4.61;  5.17;  4.53;  5.33;  5.18;] 
    let groupB = [4.81; 4.17;  4.41;  3.59;  5.87;  3.83;  6.03;  4.89;  4.32;  4.69;] 
    let groupC = [6.31; 5.12;  5.00;  5.00;  5.00;  5.29;  5.00;  6.15;  5.80;  5.26;]   
    let samples = [groupA;groupB;groupC]
        
    // calculation of the H test 
    let hResult = 
        HTest.createHTest samples 
        
    testList "Testing.HTest" [
        testCase "createHTest" <| fun () -> 
            Expect.isTrue (0.03781 = Math.Round(hResult.PValueRight,5)) "pValue should be equal."
            Expect.isTrue (6.5502  = Math.Round(hResult.Statistic,4)) "statistic should be equal."
                
    ]

[<Tests>]
let friedmanTestTests = 
    // Friedman-Test testes against dataset from https://www.methodenberatung.uzh.ch/de/datenanalyse_spss/unterschiede/zentral/friedman.html#3.2._Ergebnisse_des_Friedman-Tests and p-values obtained from distcalc and https://www.socscistatistics.com/pvalues/chidistribution.aspx 
    let A = [|275.;273.;288.;273.;244.|]
    let B = [|292.;283.;284.;285.;329.|]
    let C = [|281.;274.;298.;270.;252.|]
    let D = [|284.;275.;271.;272.;258.|]
    let E = [|285.;294.;307.;278.;275.|]
    let F = [|283.;279.;301.;276.;279.|]
    let G = [|290.;265.;298.;291.;295.|]
    let H = [|294.;277.;295.;290.;271.|]
    let I = [|300.;304.;293.;279.;271.|]
    let J = [|284.;297.;352.;292.;284.|]
    let samples = seq{A;B;C;D;E;F;G;H;I;J}

    // modified dataset from UZH for 3x equal ranks 
    let A2 = [|275.;273.;288.;273.;273.|]
    let B2 = [|292.;283.;284.;285.;329.|]
    let C2 = [|281.;274.;298.;270.;252.|]
    let D2 = [|284.;275.;271.;272.;258.|]
    let E2 = [|285.;294.;307.;278.;275.|]
    let F2 = [|283.;279.;301.;276.;279.|]
    let G2 = [|290.;265.;298.;291.;295.|]
    let H2 = [|294.;277.;295.;290.;271.|]
    let I2 = [|300.;304.;293.;279.;271.|]
    let J2 = [|284.;297.;284.;292.;284.|]
    let samples2 = seq{A2;B2;C2;D2;E2;F2;G2;H2;I2;J2}


    //calculation of friedman test
    let friedmanResult1 = 
        FriedmanTest.createFriedmanTest samples 
        
    let friedmanResult2 = 
        FriedmanTest.createFriedmanTest samples2 

    testList "Testing.FriedmanTest" [
        testCase "createFriedmanTest2equal" <| fun () -> 
            Expect.floatClose Accuracy.low friedmanResult1.Statistic 13.259 "statistics should be equal."
            Expect.floatClose Accuracy.low friedmanResult1.PValueRight 0.010077 "pValue should be equal."
        testCase "createFriedmanTest3equal" <| fun () -> 
            Expect.floatClose Accuracy.low friedmanResult2.Statistic 9.738 "statistics should be equal."
            Expect.floatClose Accuracy.low friedmanResult2.PValueRight 0.04508 "pValue should be equal."
        ]

[<Tests>]
let wilcoxonTestTests = 
    // tested against SciPy Version 1.7.1
    let before = seq{78.;24.;64.;45.;64.;52.;30.;50.;64.;50.;78.;22.;84.;40.;90.;72.}
    let after = seq{78.;24.;62.;48.;68.;56.;25.;44.;56.;40.;68.;36.;68.;20.;58.;32.}
    let differences = seq{0.;0.;2.;-3.;-4.;-4.;5.;6.;8.;10.;10.;-14.;16.;20.;32.;40.}
    // with continuity correction:
    let wilcoxon1 = WilcoxonTest.createWilcoxonTest before after true 
    let wilcoxon2 = WilcoxonTest.createWilcoxonTest before after false
    let wilcoxon3 = WilcoxonTest.createWilcoxonTestFromDifferences differences true 
    let wilcoxon4 = WilcoxonTest.createWilcoxonTestFromDifferences differences false

    testList "Testing.WilcoxonTest" [
        testCase "wilcoxonWithCorrection" <| fun () -> 
            Expect.floatClose Accuracy.low wilcoxon1.PValueTwoTailed 0.0382 "pValue should be equal."
        testCase "wilcoxonWithoutCorrection" <| fun () -> 
            Expect.floatClose Accuracy.low wilcoxon2.PValueTwoTailed 0.03537 "pValue should be equal."
        testCase "wilcoxonDifferencesWithCorrection" <| fun () -> 
            Expect.floatClose Accuracy.low wilcoxon3.PValueTwoTailed 0.0382 "pValue should be equal."
        testCase "wilcoxonDifferencesWithoutCorrection" <| fun () -> 
            Expect.floatClose Accuracy.low wilcoxon4.PValueTwoTailed 0.03537 "pValue should be equal."
        testCase "wilcoxonOneSidedWithCorrection" <| fun () -> 
            Expect.floatClose Accuracy.low wilcoxon1.PValueLeft 0.019102 "pValue should be equal"
        testCase "wilcoxonOneSidedWithoutCorrection" <| fun () -> 
            Expect.floatClose Accuracy.low wilcoxon2.PValueRight 0.9823 "pValue should be equal"    
            
        ]


[<Tests>]
let tTestTests = 
    // tested in SPSS version 27
    let groupA = vector [-5.;-3.;-3.;-4.;-5.;] 
    let groupB = vector [-2.;-4.;-4.;-6.;-6.;-6.;-5.;] 
    let groupC = vector [-3.;-7.;-8.;-4.;-2.; 1.;-1.;]   
    let groupD = vector [1.;-1.;0.;2.;2.;]   
        
    let meanA = Seq.mean groupA
    let meanB = Seq.mean groupB
    let varA = Seq.var groupA
    let varB = Seq.var groupB
    let nA = float (Seq.length groupA)
    let nB = float (Seq.length groupB)

    // calculation of the H test 
    let tTest1 = TTest.twoSample true groupA groupB
    let tTest2 = TTest.twoSampleFromMeanAndVar true (meanA,varA,nA) (meanB,varB,nB) 
    let tTest3 = TTest.twoSample false groupA groupB
    let tTest4 = TTest.oneSample groupD 0.5

    testList "Testing.TTest" [
        testCase "twoSample" <| fun () -> 
            Expect.floatClose Accuracy.low tTest1.PValue 0.377 "pValue should be equal."
            Expect.floatClose Accuracy.low tTest1.DegreesOfFreedom 10. "df should be equal."
            Expect.floatClose Accuracy.low tTest1.Statistic 0.924 "t statistic should be equal."
            Expect.floatClose Accuracy.low tTest3.PValue 0.345 "pValue should be equal."
            Expect.floatClose Accuracy.low tTest3.DegreesOfFreedom 9.990 "df should be equal."
        
        testCase "twoSampleFromMeanAndVar" <| fun () -> 
            Expect.equal tTest1 tTest2 "results should be equal."
            
           // tested with R function (t.test(batcha, batchb, var.equal=TRUE))
            let sample1 = [1.;2.;3;]
            let sample2 = [1.;3.;5.;7;]
            let mean = Seq.mean sample1
            let mean2 = Seq.mean sample2
            let var1 = Seq.var sample1
            let var2 = Seq.var sample2
            let ttestTwoSample = Testing.TTest.twoSampleFromMeanAndVar true (mean,var1,3) (mean2,var2,4) 
            let expectedPval = 0.26716219523142071
            let expectedStatistic = -1.24837556786471859
            Expect.floatClose Accuracy.high ttestTwoSample.PValue expectedPval "pValue should be equal."
            Expect.floatClose Accuracy.high ttestTwoSample.Statistic expectedStatistic "t statistic should be equal."

            let sample3 = [-1.;3.;-5.;7;]
            let mean3 = Seq.mean sample2
            let var3 = Seq.var sample2
            let ttestTwoSample2 = Testing.TTest.twoSampleFromMeanAndVar true (mean,var1,3) (mean3,var3,4) 
            let expectedPval2 = 0.75954440793496059
            let expectedStatistic2 = -0.323310403056781825
            Expect.floatClose Accuracy.high ttestTwoSample2.PValue expectedPval "pValue should be equal."
            Expect.floatClose Accuracy.high ttestTwoSample2.Statistic expectedStatistic "t statistic should be equal."

            let ttestTwoSample3 = Testing.TTest.twoSampleFromMeanAndVar true (nan,var2,3) (mean2,var2,4) 
            Expect.isTrue (nan.Equals(ttestTwoSample3.PValue)) "pValue should be nan."
            Expect.isTrue (nan.Equals(ttestTwoSample3.Statistic)) "t statistic should be nan."
        
        
        testCase "oneSample" <| fun () -> 
            Expect.floatClose Accuracy.low tTest4.PValue 0.634 "pValue should be equal."
            Expect.equal tTest4.DegreesOfFreedom 4. "df should be equal."
            Expect.floatClose Accuracy.low tTest4.Statistic 0.514 "t statistic should be equal."
        
        //tested with R function (t.test(c(-1,-2,-3), mu = -3, alternative = "two.sided"))
        testCase "oneSampleFromMeanandStDev" <| fun () -> 
            let sample = [1.;2.;3;]
            let mean = Seq.mean sample
            let stdev = Seq.stDev sample
            let ttest = Testing.TTest.oneSampleFromMeanAndStDev(mean,stdev,3) -3.
            let expectedPval = 0.013072457560346513
            let expectedStatistic = 8.6602540378443873
            Expect.floatClose Accuracy.high ttest.PValue expectedPval "pValue should be equal."
            Expect.floatClose Accuracy.high ttest.Statistic expectedStatistic "t statistic should be equal."

            let sample = [-1.;-2.;-3;]
            let mean3 = Seq.mean sample
            let stdev1 = Seq.stDev sample
            let ttest2 = Testing.TTest.oneSampleFromMeanAndStDev(mean3,stdev1,3) 0.
            let expectedPval1 = 0.074179900227448525
            let expectedStatistic2 = -3.46410161513775483
            Expect.floatClose Accuracy.high ttest2.PValue expectedPval1 "pValue should be equal."
            Expect.floatClose Accuracy.high ttest2.Statistic expectedStatistic2 "t statistic should be equal."

            let mean2 = nan
            let ttest3 = Testing.TTest.oneSampleFromMeanAndStDev(mean2,stdev,3) 0.
            Expect.isTrue (nan.Equals(ttest3.PValue)) "pValue should be nan."
            Expect.isTrue (nan.Equals(ttest3.Statistic)) "t statistic should be nan."
           
    ]
        
        
     
[<Tests>]
let chiSquaredTests = 
    // ChiSquared https://www.graphpad.com/quickcalcs/chisquared2/
    // example from R
    // obs <- c(315, 101, 108, 32)
    // exp <- c(0.5625, 0.1875, 0.1875, 0.0625) 
    // chisq.test(obs, p = exp)
    let testCase1 =
        let expected = [312.75;104.25;104.25;34.75]
        let observed = [315.;101.;108.;32.]
        let df = expected.Length - 1
        ChiSquareTest.compute df expected observed

    //obs <- c(315, 101, 80, 32, 50)
    //exp <- c(0.5625, 0.1875, 0.0875, 0.0625,0.1) 
    //chisq.test(obs, p = exp)
    let testCase2 =
        let expected = [325.125;108.375;50.575;36.125;57.8]
        let observed = [315.;101.;80.;32.;50.] 
        let df = expected.Length - 1
        ChiSquareTest.compute df expected observed
        
    testList "Testing.ChiSquaredTest" [
        testCase "compute" <| fun () -> 
            Expect.isTrue (0.9254 = Math.Round(testCase1.PValueRight,4)) "pValue should be equal."
            Expect.isTrue (0.4700 = Math.Round(testCase1.Statistic,4)) "statistic should be equal."
            Expect.isTrue (0.000638 = Math.Round(testCase2.PValueRight,6)) "pValue should be equal."
            Expect.isTrue (19.461 = Math.Round(testCase2.Statistic,3)) "statistic should be equal."
            
    ]

[<Tests>]
let pearsonTests = 
    // examples from R
    // cor.test(x,y)
    let testCase1 =
        let seq1 = [44.4; 45.9; 41.9; 53.3; 44.7; 44.1; 50.7; 45.2; 60.1;]
        let seq2 = [ 2.6;  3.1;  2.5;  5.0;  3.6;  4.0;  5.2;  2.8;  3.8;]
        Correlation.testPearson seq1 seq2

    let testCase2 =
        let seq1 = [312.7; 104.2; 104.; 34.7]
        let seq2 = [315.5; 101.3; 108.; 32.2]
        Correlation.testPearson seq1 seq2
        
    testList "Testing.Correlation" [
        testCase "testPearson" <| fun () -> 
            Expect.isTrue (0.108173054 = Math.Round(testCase1.PValue,9)) "pValue should be equal"
            Expect.isTrue (0.000294627 = Math.Round(testCase2.PValue,9)) "pValue should be equal"
    ]


[<Tests>]
let benjaminiHochbergTests =
    
    let largeSetWithIds = readCsv @"benjaminiHochberg_Input.csv"
    let largeSet        = largeSetWithIds |> Array.map snd

    let largeSetWithIds_Expected = readCsv @"benjaminiHochberg_AdjustedWithR.csv"
    let largeSet_Expected        = largeSetWithIds_Expected |> Array.map snd

    testList "Testing.MultipleTesting.BenjaminiHochberg" [
        
        testCase "testBHLarge" (fun () -> 
            Expect.sequenceEqual 
                (largeSet |> MultipleTesting.benjaminiHochbergFDR |> Seq.map (fun x -> Math.Round(x,9))) 
                (largeSet_Expected |> Seq.map (fun x -> Math.Round(x,9)))
                "adjusted pValues should be equal to the reference implementation."
        )

        testCase "testBHLargeNaN" (fun () -> 
            Expect.sequenceEqual 
                ([nan; nan; yield! largeSet] |> MultipleTesting.benjaminiHochbergFDR |> Seq.skip 2 |> Seq.map (fun x -> Math.Round(x,9))) 
                (largeSet_Expected |> Seq.map (fun x -> Math.Round(x,9)))
                "adjusted pValues should be equal to the reference implementation, ignoring nan."
        )

        testCase "testBHLargeBy" (fun () -> 
            Expect.sequenceEqual 
                (
                    largeSetWithIds 
                    |> MultipleTesting.benjaminiHochbergFDRBy id 
                    |> Seq.sortBy fst
                    |> Seq.map (fun (x,y) -> x, Math.Round(y,9))
                ) 
                (
                    largeSetWithIds_Expected 
                    |> Seq.sortBy fst
                    |> Seq.map (fun (x,y) -> x, Math.Round(y,9))
                )
                "adjusted pValues with keys should be equal to the reference implementation."
        )

        testCase "testBHLargeNaNBy" (fun () -> 
            Expect.sequenceEqual 
                (
                    [("A0",nan); ("A0",nan); yield! largeSetWithIds] 
                    |> MultipleTesting.benjaminiHochbergFDRBy id 
                    |> Seq.sortBy fst 
                    |> Seq.skip 2 
                    |> Seq.map (fun (x,y) -> x, Math.Round(y,9))
                ) 
                (
                    largeSetWithIds_Expected
                    |> Seq.sortBy fst
                    |> Seq.map (fun (x,y) -> x, Math.Round(y,9))
                )
                "adjusted pValues with keys should be equal to the reference implementation, ignoring nan."
        )
            
    ]



[<Tests>]
let qValuesTest =
  
    let largeSetWithIds = readCsv @"benjaminiHochberg_Input.csv"
    let largeSet        = largeSetWithIds |> Array.map snd
    
    let largeSetWithIds_Expected = readCsv @"qvaluesWithR.csv"
    let largeSet_Expected        = largeSetWithIds_Expected |> Array.map snd

    let largeSetWithIds_ExpectedRobust = readCsv @"qvaluesRobustWithR.csv"
    let largeSet_ExpectedRobust        = largeSetWithIds_ExpectedRobust |> Array.map snd

    testList "Testing.MultipleTesting.Qvalues" [
      
        testCase "ofPValues" (fun () -> 
            //tested against r qvalue package 2.26.0
            //pi0 estimation is in closed form in r package and therefore cannot be tested 
            //qvalue::qvalue(pvals,pi0=0.48345)
            let pi0 = 0.48345
            Expect.sequenceEqual 
                (largeSet |> MultipleTesting.Qvalues.ofPValues pi0 |> Seq.map (fun x -> Math.Round(x,9))) 
                (largeSet_Expected |> Seq.map (fun x -> Math.Round(x,9)))
                "qValues should be equal to the reference implementation."
        )

        testCase "ofPValuesRobust" (fun () -> 
            //tested against r qvalue package 2.26.0
            //pi0 estimation is in closed form in r package and therefore cannot be tested 
            //qvalue::qvalue(pvals,pi0=0.48345,pfdr=TRUE)
            let pi0 = 0.48345
            Expect.sequenceEqual 
                (largeSet |> MultipleTesting.Qvalues.ofPValuesRobust pi0 |> Seq.map (fun x -> Math.Round(x,9))) 
                (largeSet_ExpectedRobust |> Seq.map (fun x -> Math.Round(x,9)))
                "qValues Robust should be equal to the reference implementation."
        )

    ]


let createMetricTestInt metricName actual expected = testCase metricName (fun () -> Expect.equal actual expected (sprintf "Metric %s was calculated incorrectly." metricName))
let createMetricTestFloat accuracy metricName actual expected = testCase metricName (fun () -> Expect.floatClose accuracy actual expected (sprintf "Metric %s was calculated incorrectly." metricName))

[<Tests>]
let binaryConfusionMatrixTests =

    // binary classification
    //              | Predicted |
    //              |  P  |  N  |
    // | Actual | P |  3  |  1  |
    //          | N |  1  |  2  |
    
    let tp = 3.
    let tn = 2.
    let fp = 1.
    let fn = 1.
    
    let binaryCM = BinaryConfusionMatrix.create(int tp,int tn,int fp,int fn)
    let ofPredictions1 = BinaryConfusionMatrix.ofPredictions(1,[1;1;1;1;0;0;0],[1;1;1;0;1;0;0])
    let ofPredictions2 = BinaryConfusionMatrix.ofPredictions([true;true;true;true;false;false;false],[true;true;true;false;true;false;false])

    let expectedCM = {
        TP = 3
        TN = 2
        FP = 1
        FN = 1
    }

    testList "Testing.BinaryConfusionMatrix" [

        testCase "create" (fun _ -> Expect.equal binaryCM expectedCM "binary confusion matrix incorrectly created")
        testCase "ofPredictions1" (fun _ -> Expect.equal ofPredictions1 expectedCM "binary confusion matrix created incorrectly from observations with positive label")
        testCase "ofPredictions2" (fun _ -> Expect.equal ofPredictions2 expectedCM "binary confusion matrix created incorrectly from boolean observations")

        createMetricTestInt "TruePositives" binaryCM.TP 3
        createMetricTestInt "TrueNegatives" binaryCM.TN 2
        createMetricTestInt "FalsePositives" binaryCM.FP 1
        createMetricTestInt "FalseNegatives" binaryCM.FN 1
        
        testCase "thresholdMap implicit thresholds 1" (fun _ ->
            let actual = BinaryConfusionMatrix.thresholdMap(
                [true;true;true;true;false;false;false],
                [0.9 ;0.6 ;0.7 ; 0.2 ; 0.7; 0.3 ; 0.1]
            )
            let expected = [
                1.9, BinaryConfusionMatrix.create(0,3,0,4)
                0.9, BinaryConfusionMatrix.create(1,3,0,3)
                0.7, BinaryConfusionMatrix.create(2,2,1,2)
                0.6, BinaryConfusionMatrix.create(3,2,1,1)
                0.3, BinaryConfusionMatrix.create(3,1,2,1)
                0.2, BinaryConfusionMatrix.create(4,1,2,0)
                0.1, BinaryConfusionMatrix.create(4,0,3,0)
            ]
            Expect.sequenceEqual actual expected "binary threshold map not correctly created from binary predictions"
        )

        testCase "thresholdMap explicit thresholds 1" (fun _ ->
            let actual = BinaryConfusionMatrix.thresholdMap(
                [true;true;true;true;false;false;false],
                [0.9 ;0.6 ;0.7 ; 0.2 ; 0.7; 0.3 ; 0.1],
                [1.; 0.9; 0.8; 0.7; 0.6; 0.5; 0.4; 0.3; 0.2; 0.1; 0.]
            )
            let expected = [|
                1.9, BinaryConfusionMatrix.create(0,3,0,4)
                1.0, BinaryConfusionMatrix.create(0,3,0,4)
                0.9, BinaryConfusionMatrix.create(1,3,0,3)
                0.8, BinaryConfusionMatrix.create(1,3,0,3)
                0.7, BinaryConfusionMatrix.create(2,2,1,2)
                0.6, BinaryConfusionMatrix.create(3,2,1,1)
                0.5, BinaryConfusionMatrix.create(3,2,1,1)
                0.4, BinaryConfusionMatrix.create(3,2,1,1)
                0.3, BinaryConfusionMatrix.create(3,1,2,1)
                0.2, BinaryConfusionMatrix.create(4,1,2,0)
                0.1, BinaryConfusionMatrix.create(4,0,3,0)
                0. , BinaryConfusionMatrix.create(4,0,3,0)
            |]
            Expect.sequenceEqual actual expected "binary threshold map not correctly created from binary predictions"
        )

        testCase "thresholdMap: floating point error affects custom thresholds" (fun _ ->
            let actual = BinaryConfusionMatrix.thresholdMap(
                [true;true;true;true;false;false;false],
                [0.9 ;0.6 ;0.7 ; 0.2 ; 0.7; 0.3 ; 0.1],
                // these values are not exact due to floating point errors in addition. For example, the 0.7 is actually 0.70000000000000006661338147750939 which is > 0.7 and therefore produces an unexpected result
                [0. .. 0.1 .. 1.] |> List.rev 
            )
            let expected = [|
                1.9, BinaryConfusionMatrix.create(0,3,0,4)
                1.0, BinaryConfusionMatrix.create(0,3,0,4)
                0.9, BinaryConfusionMatrix.create(1,3,0,3)
                0.8, BinaryConfusionMatrix.create(1,3,0,3)
                0.7, BinaryConfusionMatrix.create(2,2,1,2)
                0.6, BinaryConfusionMatrix.create(3,2,1,1)
                0.5, BinaryConfusionMatrix.create(3,2,1,1)
                0.4, BinaryConfusionMatrix.create(3,2,1,1)
                0.3, BinaryConfusionMatrix.create(3,1,2,1)
                0.2, BinaryConfusionMatrix.create(4,1,2,0)
                0.1, BinaryConfusionMatrix.create(4,0,3,0)
                0. , BinaryConfusionMatrix.create(4,0,3,0)
            |]
            Expect.isFalse (actual = expected) "expected list comprehension threshold to produce slightly incorrent thresholds"
        )
    ]


[<Tests>]
let multiLabelConfusionMatrixTests =

    // multi label classification 
    //              |    Predicted  |
    //              |  A  |  B  | C |
    // | Actual | A |  3  |  1  | 1 |
    //          | B |  1  |  2  | 0 |
    //          | C |  2  |  0  | 4 |

    let c: Matrix<int> = 
        [
            [3; 1; 1]
            [1; 2; 0]
            [2; 0; 4]
        ]
        |> array2D
        |> Matrix.Generic.ofArray2D
    
    let expectedMLCM = 
        {
            Labels = [|"A"; "B"; "C"|]
            Confusion = 
                [
                    [3; 1; 1]
                    [1; 2; 0]
                    [2; 0; 4]
                ]
                |> array2D
                |> Matrix.Generic.ofArray2D
        }

    let multiLabelCM = MultiLabelConfusionMatrix.create([|"A";"B";"C"|], c)
    let ofPredictions = 
        MultiLabelConfusionMatrix.ofPredictions(
            [|"A"; "B"; "C"|],
            [|"A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "C"; "C"; "C"; "C"; "C"; "C"|],
            [|"A"; "A"; "A"; "B"; "C"; "B"; "B"; "A"; "C"; "C"; "C"; "C"; "A"; "A"|]
        )
    let allVsAll = multiLabelCM |> MultiLabelConfusionMatrix.allVsAll
    let expectedAllVsAll =
        [
            "A", BinaryConfusionMatrix.create(3,6,3,2)
            "B", BinaryConfusionMatrix.create(2,10,1,1)
            "C", BinaryConfusionMatrix.create(4,7,1,2)
        ]

    testList "Testing.MultiLabelConfusionMatrix" [

        testCase "create" (fun _ -> Expect.equal multiLabelCM expectedMLCM "multi label confusion matrix incorrectly created")
        testCase "ofPredictions" (fun _ -> Expect.equal ofPredictions expectedMLCM "multi label confusion matrix created incorrectly from observations with positive label")

        testCase "oneVsAll1" (fun _ -> Expect.equal (snd expectedAllVsAll[0]) (multiLabelCM |> MultiLabelConfusionMatrix.oneVsRest "A") "all-vs-all binary confusion matrices incorrectly created from multi label confusion matrix")
        testCase "oneVsAll2" (fun _ -> Expect.equal (snd expectedAllVsAll[1]) (multiLabelCM |> MultiLabelConfusionMatrix.oneVsRest "B") "all-vs-all binary confusion matrices incorrectly created from multi label confusion matrix")
        testCase "oneVsAll3" (fun _ -> Expect.equal (snd expectedAllVsAll[2]) (multiLabelCM |> MultiLabelConfusionMatrix.oneVsRest "C") "all-vs-all binary confusion matrices incorrectly created from multi label confusion matrix")
        testCase "allVsAll" (fun _ -> Expect.sequenceEqual expectedAllVsAll allVsAll "all-vs-all binary confusion matrices incorrectly created from multi label confusion matrix")
        
    ]

[<Tests>]
let comparisonMetricsTests =

    testList "Testing.ComparisonMetrics" [

        // values calculated by formulas at https://en.wikipedia.org/wiki/Confusion_matrix
        let sensitivity = 0.75
        let specificity = 0.6666666667
        let precision = 0.75
        let negativePredictiveValue = 0.6666666667
        let missrate = 0.25
        let fallOut = 0.3333333333
        let falseDiscoveryRate = 0.25
        let falseOmissionRate = 0.3333333333
        let positiveLikelihoodRatio = sensitivity / fallOut
        let negativeLikelihoodRatio = missrate / specificity
        let prevalenceThreshold = sqrt(fallOut) / (sqrt(sensitivity) + sqrt(fallOut))
        let threatScore = 0.6
        let prevalence = 0.5714285714
        let accuracy = 0.7142857143
        let balancedAccuracy = (sensitivity + specificity) / 2.
        let f1 = 0.75
        let phiCoefficient = 0.4166666667
        let fowlkesMallowsIndex = 0.75
        let informedness = 0.4166666667
        let markedness = 0.4166666667
        let diagnosticOddsRatio = positiveLikelihoodRatio / negativeLikelihoodRatio

        let tp = 3.
        let tn = 2.
        let fp = 1.
        let fn = 1.
        let p = 4.
        let n = 3.
        let samplesize = 7.
        let binaryCM = BinaryConfusionMatrix.create(3,2,1,1)
        let cm = ComparisonMetrics.create(binaryCM)

        testList "Metric calculation" [
            createMetricTestFloat Accuracy.veryHigh "Calculate Sensitivity" (ComparisonMetrics.calculateSensitivity tp p) sensitivity
            createMetricTestFloat Accuracy.veryHigh "Calculate Specificity" (ComparisonMetrics.calculateSpecificity tn n) specificity
            createMetricTestFloat Accuracy.veryHigh "Calculate Precision" (ComparisonMetrics.calculatePrecision tp fp) precision
            createMetricTestFloat Accuracy.veryHigh "Calculate NegativePredictiveValue" (ComparisonMetrics.calculateNegativePredictiveValue tn fn) negativePredictiveValue
            createMetricTestFloat Accuracy.veryHigh "Calculate Missrate" (ComparisonMetrics.calculateMissrate fn p) missrate 
            createMetricTestFloat Accuracy.veryHigh "Calculate FallOut" (ComparisonMetrics.calculateFallOut fp n) fallOut
            createMetricTestFloat Accuracy.veryHigh "Calculate FalseDiscoveryRate" (ComparisonMetrics.calculateFalseDiscoveryRate fp tp) falseDiscoveryRate
            createMetricTestFloat Accuracy.veryHigh "Calculate FalseOmissionRate" (ComparisonMetrics.calculateFalseOmissionRate fn tn) falseOmissionRate
            createMetricTestFloat Accuracy.veryHigh "Calculate PositiveLikelihoodRatio" (ComparisonMetrics.calculatePositiveLikelihoodRatio tp p fp n) positiveLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "Calculate NegativeLikelihoodRatio" (ComparisonMetrics.calculateNegativeLikelihoodRatio fn p tn n) negativeLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "Calculate PrevalenceThreshold" (ComparisonMetrics.calculatePrevalenceThreshold fp n tp p) prevalenceThreshold
            createMetricTestFloat Accuracy.veryHigh "Calculate ThreatScore" (ComparisonMetrics.calculateThreatScore tp fn fp) threatScore
            createMetricTestFloat Accuracy.veryHigh "Calculate Prevalence" (ComparisonMetrics.calculatePrevalence p samplesize) prevalence
            createMetricTestFloat Accuracy.veryHigh "Calculate Accuracy" (ComparisonMetrics.calculateAccuracy tp tn samplesize) accuracy
            createMetricTestFloat Accuracy.veryHigh "Calculate BalancedAccuracy" (ComparisonMetrics.calculateBalancedAccuracy tp p tn n) balancedAccuracy
            createMetricTestFloat Accuracy.veryHigh "Calculate F1" (ComparisonMetrics.calculateF1 tp fp fn) f1
            createMetricTestFloat Accuracy.veryHigh "Calculate PhiCoefficient" (ComparisonMetrics.calculatePhiCoefficient tp tn fp fn) phiCoefficient
            createMetricTestFloat Accuracy.veryHigh "Calculate FowlkesMallowsIndex" (ComparisonMetrics.calculateFowlkesMallowsIndex tp fp p) fowlkesMallowsIndex
            createMetricTestFloat Accuracy.veryHigh "Calculate Informedness" (ComparisonMetrics.calculateInformedness tp p tn n) informedness
            createMetricTestFloat Accuracy.veryHigh "Calculate Markedness" (ComparisonMetrics.calculateMarkedness tp fp tn fn) markedness
            createMetricTestFloat Accuracy.veryHigh "Calculate DiagnosticOddsRatio" (ComparisonMetrics.calculateDiagnosticOddsRatio tp tn fp fn p n) diagnosticOddsRatio
        ]
        testList "Binary predictions" [

            createMetricTestInt "TruePositives" cm.TP 3
            createMetricTestInt "TrueNegatives" cm.TN 2
            createMetricTestInt "FalsePositives" cm.FP 1
            createMetricTestInt "FalseNegatives" cm.FN 1
            createMetricTestInt "Positves" cm.P 4
            createMetricTestInt "Negatives" cm.N 3
            createMetricTestInt "Total" cm.SampleSize 7

            createMetricTestFloat Accuracy.veryHigh "Sensitivity" cm.Sensitivity sensitivity
            createMetricTestFloat Accuracy.veryHigh "Specificity" cm.Specificity specificity
            createMetricTestFloat Accuracy.veryHigh "Precision" cm.Precision precision
            createMetricTestFloat Accuracy.veryHigh "NegativePredictiveValue" cm.NegativePredictiveValue negativePredictiveValue
            createMetricTestFloat Accuracy.veryHigh "Missrate" cm.Missrate missrate 
            createMetricTestFloat Accuracy.veryHigh "FallOut" cm.FallOut fallOut
            createMetricTestFloat Accuracy.veryHigh "FalseDiscoveryRate" cm.FalseDiscoveryRate falseDiscoveryRate
            createMetricTestFloat Accuracy.veryHigh "FalseOmissionRate" cm.FalseOmissionRate falseOmissionRate
            createMetricTestFloat Accuracy.veryHigh "PositiveLikelihoodRatio" cm.PositiveLikelihoodRatio positiveLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "NegativeLikelihoodRatio" cm.NegativeLikelihoodRatio negativeLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "PrevalenceThreshold" cm.PrevalenceThreshold prevalenceThreshold
            createMetricTestFloat Accuracy.veryHigh "ThreatScore" cm.ThreatScore threatScore
            createMetricTestFloat Accuracy.veryHigh "Prevalence" cm.Prevalence prevalence
            createMetricTestFloat Accuracy.veryHigh "Accuracy" cm.Accuracy accuracy
            createMetricTestFloat Accuracy.veryHigh "BalancedAccuracy" cm.BalancedAccuracy balancedAccuracy
            createMetricTestFloat Accuracy.veryHigh "F1" cm.F1 f1
            createMetricTestFloat Accuracy.veryHigh "PhiCoefficient" cm.PhiCoefficient phiCoefficient
            createMetricTestFloat Accuracy.veryHigh "FowlkesMallowsIndex" cm.FowlkesMallowsIndex fowlkesMallowsIndex
            createMetricTestFloat Accuracy.veryHigh "Informedness" cm.Informedness informedness
            createMetricTestFloat Accuracy.veryHigh "Markedness" cm.Markedness markedness
            createMetricTestFloat Accuracy.veryHigh "DiagnosticOddsRatio" cm.DiagnosticOddsRatio diagnosticOddsRatio

        ]
        testList "Multi-label predictions" [
            let c: Matrix<int> = 
                [
                    [3; 1; 1]
                    [1; 2; 0]
                    [2; 0; 4]
                ]
                |> array2D
                |> Matrix.Generic.ofArray2D

            let multiLabelCM  = MultiLabelConfusionMatrix.create([|"A";"B";"C"|], c)

            let expectedAvsRest = BinaryConfusionMatrix.create(3,6,3,2)
            let expectedBvsRest = BinaryConfusionMatrix.create(2,10,1,1)
            let expectedCvsRest = BinaryConfusionMatrix.create(4,7,1,2)

            let expectedMicroAverage = ComparisonMetrics.create(BinaryConfusionMatrix.create(9,23,5,5))

            let expectedMacroAverage =
                [
                    expectedAvsRest
                    expectedBvsRest
                    expectedCvsRest
                ]
                |> List.map (fun x -> ComparisonMetrics.create(x))
                |> fun metrics ->
                    ComparisonMetrics.create(
                        ((metrics[0].P                       + metrics[1].P                      + metrics[2].P                      ) / 3.),
                        ((metrics[0].N                       + metrics[1].N                      + metrics[2].N                      ) / 3.),
                        ((metrics[0].SampleSize              + metrics[1].SampleSize             + metrics[2].SampleSize             ) / 3.),
                        ((metrics[0].TP                      + metrics[1].TP                     + metrics[2].TP                     ) / 3.),
                        ((metrics[0].TN                      + metrics[1].TN                     + metrics[2].TN                     ) / 3.),
                        ((metrics[0].FP                      + metrics[1].FP                     + metrics[2].FP                     ) / 3.),
                        ((metrics[0].FN                      + metrics[1].FN                     + metrics[2].FN                     ) / 3.),
                        ((metrics[0].Sensitivity             + metrics[1].Sensitivity            + metrics[2].Sensitivity            ) / 3.),
                        ((metrics[0].Specificity             + metrics[1].Specificity            + metrics[2].Specificity            ) / 3.),
                        ((metrics[0].Precision               + metrics[1].Precision              + metrics[2].Precision              ) / 3.),
                        ((metrics[0].NegativePredictiveValue + metrics[1].NegativePredictiveValue+ metrics[2].NegativePredictiveValue) / 3.),
                        ((metrics[0].Missrate                + metrics[1].Missrate               + metrics[2].Missrate               ) / 3.),
                        ((metrics[0].FallOut                 + metrics[1].FallOut                + metrics[2].FallOut                ) / 3.),
                        ((metrics[0].FalseDiscoveryRate      + metrics[1].FalseDiscoveryRate     + metrics[2].FalseDiscoveryRate     ) / 3.),
                        ((metrics[0].FalseOmissionRate       + metrics[1].FalseOmissionRate      + metrics[2].FalseOmissionRate      ) / 3.),
                        ((metrics[0].PositiveLikelihoodRatio + metrics[1].PositiveLikelihoodRatio+ metrics[2].PositiveLikelihoodRatio) / 3.),
                        ((metrics[0].NegativeLikelihoodRatio + metrics[1].NegativeLikelihoodRatio+ metrics[2].NegativeLikelihoodRatio) / 3.),
                        ((metrics[0].PrevalenceThreshold     + metrics[1].PrevalenceThreshold    + metrics[2].PrevalenceThreshold    ) / 3.),
                        ((metrics[0].ThreatScore             + metrics[1].ThreatScore            + metrics[2].ThreatScore            ) / 3.),
                        ((metrics[0].Prevalence              + metrics[1].Prevalence             + metrics[2].Prevalence             ) / 3.),
                        ((metrics[0].Accuracy                + metrics[1].Accuracy               + metrics[2].Accuracy               ) / 3.),
                        ((metrics[0].BalancedAccuracy        + metrics[1].BalancedAccuracy       + metrics[2].BalancedAccuracy       ) / 3.),
                        ((metrics[0].F1                      + metrics[1].F1                     + metrics[2].F1                     ) / 3.),
                        ((metrics[0].PhiCoefficient          + metrics[1].PhiCoefficient         + metrics[2].PhiCoefficient         ) / 3.),
                        ((metrics[0].FowlkesMallowsIndex     + metrics[1].FowlkesMallowsIndex    + metrics[2].FowlkesMallowsIndex    ) / 3.),
                        ((metrics[0].Informedness            + metrics[1].Informedness           + metrics[2].Informedness           ) / 3.),
                        ((metrics[0].Markedness              + metrics[1].Markedness             + metrics[2].Markedness             ) / 3.),
                        ((metrics[0].DiagnosticOddsRatio     + metrics[1].DiagnosticOddsRatio    + metrics[2].DiagnosticOddsRatio    ) / 3.)
                    )


            let cmMicroAverage1 = ComparisonMetrics.microAverage multiLabelCM
            let cmMacroAverage1 = ComparisonMetrics.macroAverage multiLabelCM
            let cmMicroAverage2 = multiLabelCM |> MultiLabelConfusionMatrix.allVsAll |> Array.map snd |> ComparisonMetrics.microAverage
            let cmMacroAverage2 = multiLabelCM |> MultiLabelConfusionMatrix.allVsAll |> Array.map (snd >> ComparisonMetrics.create) |> ComparisonMetrics.macroAverage
            
            createMetricTestFloat Accuracy.veryHigh "microAverage: Sensitivity 1" cmMicroAverage1.Sensitivity expectedMicroAverage.Sensitivity
            createMetricTestFloat Accuracy.veryHigh "microAverage: Specificity 1" cmMicroAverage1.Specificity expectedMicroAverage.Specificity
            createMetricTestFloat Accuracy.veryHigh "microAverage: Precision 1" cmMicroAverage1.Precision expectedMicroAverage.Precision
            createMetricTestFloat Accuracy.veryHigh "microAverage: NegativePredictiveValue 1" cmMicroAverage1.NegativePredictiveValue expectedMicroAverage.NegativePredictiveValue
            createMetricTestFloat Accuracy.veryHigh "microAverage: Missrate 1" cmMicroAverage1.Missrate expectedMicroAverage.Missrate 
            createMetricTestFloat Accuracy.veryHigh "microAverage: FallOut 1" cmMicroAverage1.FallOut expectedMicroAverage.FallOut
            createMetricTestFloat Accuracy.veryHigh "microAverage: FalseDiscoveryRate 1" cmMicroAverage1.FalseDiscoveryRate expectedMicroAverage.FalseDiscoveryRate
            createMetricTestFloat Accuracy.veryHigh "microAverage: FalseOmissionRate 1" cmMicroAverage1.FalseOmissionRate expectedMicroAverage.FalseOmissionRate
            createMetricTestFloat Accuracy.veryHigh "microAverage: PositiveLikelihoodRatio 1" cmMicroAverage1.PositiveLikelihoodRatio expectedMicroAverage.PositiveLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "microAverage: NegativeLikelihoodRatio 1" cmMicroAverage1.NegativeLikelihoodRatio expectedMicroAverage.NegativeLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "microAverage: PrevalenceThreshold 1" cmMicroAverage1.PrevalenceThreshold expectedMicroAverage.PrevalenceThreshold
            createMetricTestFloat Accuracy.veryHigh "microAverage: ThreatScore 1" cmMicroAverage1.ThreatScore expectedMicroAverage.ThreatScore
            createMetricTestFloat Accuracy.veryHigh "microAverage: Prevalence 1" cmMicroAverage1.Prevalence expectedMicroAverage.Prevalence
            createMetricTestFloat Accuracy.veryHigh "microAverage: Accuracy 1" cmMicroAverage1.Accuracy expectedMicroAverage.Accuracy
            createMetricTestFloat Accuracy.veryHigh "microAverage: BalancedAccuracy 1" cmMicroAverage1.BalancedAccuracy expectedMicroAverage.BalancedAccuracy
            createMetricTestFloat Accuracy.veryHigh "microAverage: F1 1" cmMicroAverage1.F1 expectedMicroAverage.F1
            createMetricTestFloat Accuracy.veryHigh "microAverage: PhiCoefficient 1" cmMicroAverage1.PhiCoefficient expectedMicroAverage.PhiCoefficient
            createMetricTestFloat Accuracy.veryHigh "microAverage: FowlkesMallowsIndex 1" cmMicroAverage1.FowlkesMallowsIndex expectedMicroAverage.FowlkesMallowsIndex
            createMetricTestFloat Accuracy.veryHigh "microAverage: Informedness 1" cmMicroAverage1.Informedness expectedMicroAverage.Informedness
            createMetricTestFloat Accuracy.veryHigh "microAverage: Markedness 1" cmMicroAverage1.Markedness expectedMicroAverage.Markedness
            createMetricTestFloat Accuracy.veryHigh "microAverage: DiagnosticOddsRatio 1" cmMicroAverage1.DiagnosticOddsRatio expectedMicroAverage.DiagnosticOddsRatio

            createMetricTestFloat Accuracy.veryHigh "microAverage: Sensitivity 2" cmMicroAverage2.Sensitivity expectedMicroAverage.Sensitivity
            createMetricTestFloat Accuracy.veryHigh "microAverage: Specificity 2" cmMicroAverage2.Specificity expectedMicroAverage.Specificity
            createMetricTestFloat Accuracy.veryHigh "microAverage: Precision 2" cmMicroAverage2.Precision expectedMicroAverage.Precision
            createMetricTestFloat Accuracy.veryHigh "microAverage: NegativePredictiveValue 2" cmMicroAverage2.NegativePredictiveValue expectedMicroAverage.NegativePredictiveValue
            createMetricTestFloat Accuracy.veryHigh "microAverage: Missrate 2" cmMicroAverage2.Missrate expectedMicroAverage.Missrate 
            createMetricTestFloat Accuracy.veryHigh "microAverage: FallOut 2" cmMicroAverage1.FallOut expectedMicroAverage.FallOut
            createMetricTestFloat Accuracy.veryHigh "microAverage: FalseDiscoveryRate 2" cmMicroAverage2.FalseDiscoveryRate expectedMicroAverage.FalseDiscoveryRate
            createMetricTestFloat Accuracy.veryHigh "microAverage: FalseOmissionRate 2" cmMicroAverage2.FalseOmissionRate expectedMicroAverage.FalseOmissionRate
            createMetricTestFloat Accuracy.veryHigh "microAverage: PositiveLikelihoodRatio 2" cmMicroAverage1.PositiveLikelihoodRatio expectedMicroAverage.PositiveLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "microAverage: NegativeLikelihoodRatio 2" cmMicroAverage1.NegativeLikelihoodRatio expectedMicroAverage.NegativeLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "microAverage: PrevalenceThreshold 2" cmMicroAverage1.PrevalenceThreshold expectedMicroAverage.PrevalenceThreshold
            createMetricTestFloat Accuracy.veryHigh "microAverage: ThreatScore 2" cmMicroAverage1.ThreatScore expectedMicroAverage.ThreatScore
            createMetricTestFloat Accuracy.veryHigh "microAverage: Prevalence 2" cmMicroAverage1.Prevalence expectedMicroAverage.Prevalence
            createMetricTestFloat Accuracy.veryHigh "microAverage: Accuracy 2" cmMicroAverage1.Accuracy expectedMicroAverage.Accuracy
            createMetricTestFloat Accuracy.veryHigh "microAverage: BalancedAccuracy 2" cmMicroAverage1.BalancedAccuracy expectedMicroAverage.BalancedAccuracy
            createMetricTestFloat Accuracy.veryHigh "microAverage: F1 2" cmMicroAverage1.F1 expectedMicroAverage.F1
            createMetricTestFloat Accuracy.veryHigh "microAverage: PhiCoefficient 2" cmMicroAverage1.PhiCoefficient expectedMicroAverage.PhiCoefficient
            createMetricTestFloat Accuracy.veryHigh "microAverage: FowlkesMallowsIndex 2" cmMicroAverage1.FowlkesMallowsIndex expectedMicroAverage.FowlkesMallowsIndex
            createMetricTestFloat Accuracy.veryHigh "microAverage: Informedness 2" cmMicroAverage1.Informedness expectedMicroAverage.Informedness
            createMetricTestFloat Accuracy.veryHigh "microAverage: Markedness 2" cmMicroAverage1.Markedness expectedMicroAverage.Markedness
            createMetricTestFloat Accuracy.veryHigh "microAverage: DiagnosticOddsRatio 2" cmMicroAverage1.DiagnosticOddsRatio expectedMicroAverage.DiagnosticOddsRatio
        
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Sensitivity 1" cmMacroAverage1.Sensitivity expectedMacroAverage.Sensitivity
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Specificity 1" cmMacroAverage1.Specificity expectedMacroAverage.Specificity
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Precision 1" cmMacroAverage1.Precision expectedMacroAverage.Precision
            createMetricTestFloat Accuracy.veryHigh "macroAverage: NegativePredictiveValue 1" cmMacroAverage1.NegativePredictiveValue expectedMacroAverage.NegativePredictiveValue
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Missrate 1" cmMacroAverage1.Missrate expectedMacroAverage.Missrate 
            createMetricTestFloat Accuracy.veryHigh "macroAverage: FallOut 1" cmMacroAverage1.FallOut expectedMacroAverage.FallOut
            createMetricTestFloat Accuracy.veryHigh "macroAverage: FalseDiscoveryRate 1" cmMacroAverage1.FalseDiscoveryRate expectedMacroAverage.FalseDiscoveryRate
            createMetricTestFloat Accuracy.veryHigh "macroAverage: FalseOmissionRate 1" cmMacroAverage1.FalseOmissionRate expectedMacroAverage.FalseOmissionRate
            createMetricTestFloat Accuracy.veryHigh "macroAverage: PositiveLikelihoodRatio 1" cmMacroAverage1.PositiveLikelihoodRatio expectedMacroAverage.PositiveLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "macroAverage: NegativeLikelihoodRatio 1" cmMacroAverage1.NegativeLikelihoodRatio expectedMacroAverage.NegativeLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "macroAverage: PrevalenceThreshold 1" cmMacroAverage1.PrevalenceThreshold expectedMacroAverage.PrevalenceThreshold
            createMetricTestFloat Accuracy.veryHigh "macroAverage: ThreatScore 1" cmMacroAverage1.ThreatScore expectedMacroAverage.ThreatScore
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Prevalence 1" cmMacroAverage1.Prevalence expectedMacroAverage.Prevalence
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Accuracy 1" cmMacroAverage1.Accuracy expectedMacroAverage.Accuracy
            createMetricTestFloat Accuracy.veryHigh "macroAverage: BalancedAccuracy 1" cmMacroAverage1.BalancedAccuracy expectedMacroAverage.BalancedAccuracy
            createMetricTestFloat Accuracy.veryHigh "macroAverage: F1 1" cmMacroAverage1.F1 expectedMacroAverage.F1
            createMetricTestFloat Accuracy.veryHigh "macroAverage: PhiCoefficient 1" cmMacroAverage1.PhiCoefficient expectedMacroAverage.PhiCoefficient
            createMetricTestFloat Accuracy.veryHigh "macroAverage: FowlkesMallowsIndex 1" cmMacroAverage1.FowlkesMallowsIndex expectedMacroAverage.FowlkesMallowsIndex
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Informedness 1" cmMacroAverage1.Informedness expectedMacroAverage.Informedness
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Markedness 1" cmMacroAverage1.Markedness expectedMacroAverage.Markedness
            createMetricTestFloat Accuracy.veryHigh "macroAverage: DiagnosticOddsRatio 1" cmMacroAverage1.DiagnosticOddsRatio expectedMacroAverage.DiagnosticOddsRatio

            createMetricTestFloat Accuracy.veryHigh "macroAverage: Sensitivity 2" cmMacroAverage2.Sensitivity expectedMacroAverage.Sensitivity
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Specificity 2" cmMacroAverage2.Specificity expectedMacroAverage.Specificity
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Precision 2" cmMacroAverage2.Precision expectedMacroAverage.Precision
            createMetricTestFloat Accuracy.veryHigh "macroAverage: NegativePredictiveValue 2" cmMacroAverage2.NegativePredictiveValue expectedMacroAverage.NegativePredictiveValue
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Missrate 2" cmMacroAverage2.Missrate expectedMacroAverage.Missrate 
            createMetricTestFloat Accuracy.veryHigh "macroAverage: FallOut 2" cmMacroAverage2.FallOut expectedMacroAverage.FallOut
            createMetricTestFloat Accuracy.veryHigh "macroAverage: FalseDiscoveryRate 2" cmMacroAverage2.FalseDiscoveryRate expectedMacroAverage.FalseDiscoveryRate
            createMetricTestFloat Accuracy.veryHigh "macroAverage: FalseOmissionRate 2" cmMacroAverage2.FalseOmissionRate expectedMacroAverage.FalseOmissionRate
            createMetricTestFloat Accuracy.veryHigh "macroAverage: PositiveLikelihoodRatio 2" cmMacroAverage2.PositiveLikelihoodRatio expectedMacroAverage.PositiveLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "macroAverage: NegativeLikelihoodRatio 2" cmMacroAverage2.NegativeLikelihoodRatio expectedMacroAverage.NegativeLikelihoodRatio
            createMetricTestFloat Accuracy.veryHigh "macroAverage: PrevalenceThreshold 2" cmMacroAverage2.PrevalenceThreshold expectedMacroAverage.PrevalenceThreshold
            createMetricTestFloat Accuracy.veryHigh "macroAverage: ThreatScore 2" cmMacroAverage2.ThreatScore expectedMacroAverage.ThreatScore
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Prevalence 2" cmMacroAverage2.Prevalence expectedMacroAverage.Prevalence
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Accuracy 2" cmMacroAverage2.Accuracy expectedMacroAverage.Accuracy
            createMetricTestFloat Accuracy.veryHigh "macroAverage: BalancedAccuracy 2" cmMacroAverage2.BalancedAccuracy expectedMacroAverage.BalancedAccuracy
            createMetricTestFloat Accuracy.veryHigh "macroAverage: F1 2" cmMacroAverage2.F1 expectedMacroAverage.F1
            createMetricTestFloat Accuracy.veryHigh "macroAverage: PhiCoefficient 2" cmMacroAverage2.PhiCoefficient expectedMacroAverage.PhiCoefficient
            createMetricTestFloat Accuracy.veryHigh "macroAverage: FowlkesMallowsIndex 2" cmMacroAverage2.FowlkesMallowsIndex expectedMacroAverage.FowlkesMallowsIndex
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Informedness 2" cmMacroAverage2.Informedness expectedMacroAverage.Informedness
            createMetricTestFloat Accuracy.veryHigh "macroAverage: Markedness 2" cmMacroAverage2.Markedness expectedMacroAverage.Markedness
            createMetricTestFloat Accuracy.veryHigh "macroAverage: DiagnosticOddsRatio 2" cmMacroAverage2.DiagnosticOddsRatio expectedMacroAverage.DiagnosticOddsRatio
        ]
        testList "binary threshold map" [
            let actual = 
                ComparisonMetrics.binaryThresholdMap(
                    [true;true;true;true;false;false;false],
                    [0.9 ;0.6 ;0.7 ; 0.2 ; 0.7; 0.3 ; 0.1]
                )

            let expected = [|
                1.9, BinaryConfusionMatrix.create(0,3,0,4) |> ComparisonMetrics.create
                0.9, BinaryConfusionMatrix.create(1,3,0,3) |> ComparisonMetrics.create
                0.7, BinaryConfusionMatrix.create(2,2,1,2) |> ComparisonMetrics.create
                0.6, BinaryConfusionMatrix.create(3,2,1,1) |> ComparisonMetrics.create
                0.3, BinaryConfusionMatrix.create(3,1,2,1) |> ComparisonMetrics.create
                0.2, BinaryConfusionMatrix.create(4,1,2,0) |> ComparisonMetrics.create
                0.1, BinaryConfusionMatrix.create(4,0,3,0) |> ComparisonMetrics.create
            |]
            testCase "threshold 1-9" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd actual[0]) (snd expected[0]) "Incorrect metrics for threshold 1.9")
            testCase "threshold 0-9" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd actual[1]) (snd expected[1]) "Incorrect metrics for threshold 0.9")
            testCase "threshold 0-7" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd actual[2]) (snd expected[2]) "Incorrect metrics for threshold 0.7")
            testCase "threshold 0-6" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd actual[3]) (snd expected[3]) "Incorrect metrics for threshold 0.6")
            testCase "threshold 0-3" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd actual[4]) (snd expected[4]) "Incorrect metrics for threshold 0.3")
            testCase "threshold 0-2" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd actual[5]) (snd expected[5]) "Incorrect metrics for threshold 0.2")
            testCase "threshold 0-1" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd actual[6]) (snd expected[6]) "Incorrect metrics for threshold 0.1")
        ]
        testList "multi-label threshold map" [
            let expectedMetricsMap =
                Map.ofList [
                    "A", [|
                        1.9, BinaryConfusionMatrix.create(0,9,0,5) |> ComparisonMetrics.create
                        0.9, BinaryConfusionMatrix.create(1,9,0,4) |> ComparisonMetrics.create
                        0.8, BinaryConfusionMatrix.create(2,9,0,3) |> ComparisonMetrics.create
                        0.7, BinaryConfusionMatrix.create(3,9,0,2) |> ComparisonMetrics.create
                        0.6, BinaryConfusionMatrix.create(3,9,0,2) |> ComparisonMetrics.create
                        0.5, BinaryConfusionMatrix.create(3,7,2,2) |> ComparisonMetrics.create
                        0.4, BinaryConfusionMatrix.create(4,6,3,1) |> ComparisonMetrics.create
                        0.3, BinaryConfusionMatrix.create(5,5,4,0) |> ComparisonMetrics.create
                        0.2, BinaryConfusionMatrix.create(5,4,5,0) |> ComparisonMetrics.create
                        0.1, BinaryConfusionMatrix.create(5,0,9,0) |> ComparisonMetrics.create
                        0.0, BinaryConfusionMatrix.create(5,0,9,0) |> ComparisonMetrics.create
                    |]
                    "B", [|
                        1.9, BinaryConfusionMatrix.create(0,11,0,3) |> ComparisonMetrics.create
                        0.9, BinaryConfusionMatrix.create(0,11,0,3) |> ComparisonMetrics.create
                        0.8, BinaryConfusionMatrix.create(1,11,0,2) |> ComparisonMetrics.create
                        0.7, BinaryConfusionMatrix.create(2,11,0,1) |> ComparisonMetrics.create
                        0.6, BinaryConfusionMatrix.create(2,11,0,1) |> ComparisonMetrics.create
                        0.5, BinaryConfusionMatrix.create(2,10,1,1) |> ComparisonMetrics.create
                        0.4, BinaryConfusionMatrix.create(3,10,1,0) |> ComparisonMetrics.create
                        0.3, BinaryConfusionMatrix.create(3,9,2,0) |> ComparisonMetrics.create
                        0.2, BinaryConfusionMatrix.create(3,9,2,0) |> ComparisonMetrics.create
                        0.1, BinaryConfusionMatrix.create(3,4,7,0) |> ComparisonMetrics.create
                        0.0, BinaryConfusionMatrix.create(3,0,11,0) |> ComparisonMetrics.create
                    |]
                    "C", [|
                        1.9, BinaryConfusionMatrix.create(0,8,0,6) |> ComparisonMetrics.create
                        0.9, BinaryConfusionMatrix.create(1,8,0,5) |> ComparisonMetrics.create
                        0.8, BinaryConfusionMatrix.create(3,8,0,3) |> ComparisonMetrics.create
                        0.7, BinaryConfusionMatrix.create(4,8,0,2) |> ComparisonMetrics.create
                        0.6, BinaryConfusionMatrix.create(4,7,1,2) |> ComparisonMetrics.create
                        0.5, BinaryConfusionMatrix.create(4,7,1,2) |> ComparisonMetrics.create
                        0.4, BinaryConfusionMatrix.create(5,7,1,1) |> ComparisonMetrics.create
                        0.3, BinaryConfusionMatrix.create(6,7,1,0) |> ComparisonMetrics.create
                        0.2, BinaryConfusionMatrix.create(6,5,3,0) |> ComparisonMetrics.create
                        0.1, BinaryConfusionMatrix.create(6,0,8,0) |> ComparisonMetrics.create
                        0.0, BinaryConfusionMatrix.create(6,0,8,0) |> ComparisonMetrics.create
                    |]
                ]   
            
            let actual = 
                ComparisonMetrics.multiLabelThresholdMap(
                    actual = [|"A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "C"; "C"; "C"; "C"; "C"; "C"|],
                    predictions = [|
                        "A", [|0.8; 0.7; 0.9; 0.4; 0.3; 0.1; 0.2; 0.5; 0.1; 0.1; 0.1; 0.3; 0.5; 0.4|]
                        "B", [|0.0; 0.1; 0.0; 0.5; 0.1; 0.8; 0.7; 0.4; 0.0; 0.1; 0.1; 0.0; 0.1; 0.3|]
                        "C", [|0.2; 0.2; 0.1; 0.1; 0.6; 0.1; 0.1; 0.1; 0.9; 0.8; 0.8; 0.7; 0.4; 0.3|]
                    |]
                )

            testCase "A: threshold 1-9" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][0])) (snd (expectedMetricsMap["A"][0])) "Incorrect metrics for threshold 1.9")
            testCase "A: threshold 0-9" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][1])) (snd (expectedMetricsMap["A"][1])) "Incorrect metrics for threshold 0.9")
            testCase "A: threshold 0-8" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][2])) (snd (expectedMetricsMap["A"][2])) "Incorrect metrics for threshold 0.8")
            testCase "A: threshold 0-7" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][3])) (snd (expectedMetricsMap["A"][3])) "Incorrect metrics for threshold 0.7")
            testCase "A: threshold 0-6" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][4])) (snd (expectedMetricsMap["A"][4])) "Incorrect metrics for threshold 0.6")
            testCase "A: threshold 0-5" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][5])) (snd (expectedMetricsMap["A"][5])) "Incorrect metrics for threshold 0.5")
            testCase "A: threshold 0-4" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][6])) (snd (expectedMetricsMap["A"][6])) "Incorrect metrics for threshold 0.4")
            testCase "A: threshold 0-3" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][7])) (snd (expectedMetricsMap["A"][7])) "Incorrect metrics for threshold 0.3")
            testCase "A: threshold 0-2" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][8])) (snd (expectedMetricsMap["A"][8])) "Incorrect metrics for threshold 0.2")
            testCase "A: threshold 0-1" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][9])) (snd (expectedMetricsMap["A"][9])) "Incorrect metrics for threshold 0.1")
            testCase "A: threshold 0-0" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["A"][10])) (snd (expectedMetricsMap["A"][10])) "Incorrect metrics for threshold 0.0")

            testCase "B: threshold 1-9" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][0])) (snd (expectedMetricsMap["B"][0])) "Incorrect metrics for threshold 1.9")
            testCase "B: threshold 0-9" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][1])) (snd (expectedMetricsMap["B"][1])) "Incorrect metrics for threshold 0.9")
            testCase "B: threshold 0-8" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][2])) (snd (expectedMetricsMap["B"][2])) "Incorrect metrics for threshold 0.8")
            testCase "B: threshold 0-7" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][3])) (snd (expectedMetricsMap["B"][3])) "Incorrect metrics for threshold 0.7")
            testCase "B: threshold 0-6" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][4])) (snd (expectedMetricsMap["B"][4])) "Incorrect metrics for threshold 0.6")
            testCase "B: threshold 0-5" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][5])) (snd (expectedMetricsMap["B"][5])) "Incorrect metrics for threshold 0.5")
            testCase "B: threshold 0-4" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][6])) (snd (expectedMetricsMap["B"][6])) "Incorrect metrics for threshold 0.4")
            testCase "B: threshold 0-3" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][7])) (snd (expectedMetricsMap["B"][7])) "Incorrect metrics for threshold 0.3")
            testCase "B: threshold 0-2" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][8])) (snd (expectedMetricsMap["B"][8])) "Incorrect metrics for threshold 0.2")
            testCase "B: threshold 0-1" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][9])) (snd (expectedMetricsMap["B"][9])) "Incorrect metrics for threshold 0.1")
            testCase "B: threshold 0-0" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["B"][10])) (snd (expectedMetricsMap["B"][10])) "Incorrect metrics for threshold 0.0")

            testCase "C: threshold 1-9" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][0])) (snd (expectedMetricsMap["C"][0])) "Incorrect metrics for threshold 1.9")
            testCase "C: threshold 0-9" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][1])) (snd (expectedMetricsMap["C"][1])) "Incorrect metrics for threshold 0.9")
            testCase "C: threshold 0-8" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][2])) (snd (expectedMetricsMap["C"][2])) "Incorrect metrics for threshold 0.8")
            testCase "C: threshold 0-7" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][3])) (snd (expectedMetricsMap["C"][3])) "Incorrect metrics for threshold 0.7")
            testCase "C: threshold 0-6" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][4])) (snd (expectedMetricsMap["C"][4])) "Incorrect metrics for threshold 0.6")
            testCase "C: threshold 0-5" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][5])) (snd (expectedMetricsMap["C"][5])) "Incorrect metrics for threshold 0.5")
            testCase "C: threshold 0-4" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][6])) (snd (expectedMetricsMap["C"][6])) "Incorrect metrics for threshold 0.4")
            testCase "C: threshold 0-3" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][7])) (snd (expectedMetricsMap["C"][7])) "Incorrect metrics for threshold 0.3")
            testCase "C: threshold 0-2" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][8])) (snd (expectedMetricsMap["C"][8])) "Incorrect metrics for threshold 0.2")
            testCase "C: threshold 0-1" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][9])) (snd (expectedMetricsMap["C"][9])) "Incorrect metrics for threshold 0.1")
            testCase "C: threshold 0-0" (fun _ -> TestExtensions.comparisonMetricsEqualRounded 3 (snd (actual["C"][10])) (snd (expectedMetricsMap["C"][10])) "Incorrect metrics for threshold 0.0")
        ]
    ]

