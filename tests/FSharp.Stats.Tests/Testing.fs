module TestingTests
open Expecto
open System
open FSharp.Stats.Testing
open FSharp.Stats

open System.IO
open System.Reflection

let assembly = Assembly.GetExecutingAssembly()
let resnames = assembly.GetManifestResourceNames();

let readEmbeddedRessource (name:string) = 
    match Array.tryFind (fun (r:string) -> r.Contains(name)) resnames with
    | Some path -> 
        use stream = assembly.GetManifestResourceStream(path)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    | _ -> failwithf "could not embedded ressources, check package integrity"


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
        testCase "oneSample" <| fun () -> 
            Expect.floatClose Accuracy.low tTest4.PValue 0.634 "pValue should be equal."
            Expect.equal tTest4.DegreesOfFreedom 4. "df should be equal."
            Expect.floatClose Accuracy.low tTest4.Statistic 0.514 "t statistic should be equal."
    ]

[<Tests>]
let fTestTests = 
    // F-Test validated against res.ftest <- var.test(samplea, sampleb, alternative = "two.sided") RStudio 2022.02.3+492 "Prairie Trillium" Release (1db809b8323ba0a87c148d16eb84efe39a8e7785, 2022-05-20) for Windows

    let sampleFA = vector [|5.0; 6.0; 5.8; 5.7|] 
    let sampleFB = vector [|3.5; 3.7; 4.0; 3.3; 3.6|]
    let sampleNaN = vector [|5.0; 6.0; 5.8; nan|]
    let sampleInf = vector [|5.0; 6.0; 5.8; infinity|]
    let sampleNegInf = vector [|5.0; 6.0; 5.8; -infinity|]
    let sampleties = vector [|5.0; 5.0; 5.8; 5.3|]

    // calculation of the F test 
    let fResult = FTest.testVariances sampleFA sampleFB
    let fResultNaN = FTest.testVariances sampleNaN sampleFB
    let fResultInf = FTest.testVariances sampleInf sampleFB
    let fResultNegInf = FTest.testVariances sampleNegInf sampleFB
    let fResultTies = FTest.testVariances sampleFA sampleties 



    testList "Testing.FTest" [
        testCase "createFTest" <| fun () -> 
            Expect.floatClose Accuracy.low fResult.Statistic 2.82338 "statistics should be equal."
            Expect.floatClose Accuracy.low fResult.PValueTwoTailed 0.34172 "pValue should be equal."
        testCase "FTest NaN" <| fun () -> 
            Expect.isTrue (nan.Equals (fResultNaN.Statistic)) "statistic should be nan"
        testCase "FTest infinities" <| fun () -> 
            Expect.isTrue (nan.Equals (fResultInf.Statistic)) "statistic should be nan"
            Expect.isTrue (nan.Equals (fResultNegInf.Statistic)) "statistic should be nan"
        testCase "FTest 2 ties" <| fun () -> 
            Expect.floatClose Accuracy.low fResultTies.Statistic 1.32748538 "statistics should be equal."
            Expect.floatClose Accuracy.low fResultTies.PValueTwoTailed 0.8214 "pValue should be equal."
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
    
    let readCsv path =
        readEmbeddedRessource path
        |> fun s -> s.Split("\r\n")
        |> Array.skip 1
        |> Array.map (fun x -> 
            printfn "%s" x
            x.Split(", ") |> fun ([|a;b|]) -> a, float b
        )

    let largeSetWithIds = readCsv @"benjaminiHochberg_Input.csv"
    let largeSet        = largeSetWithIds |> Array.map snd

    let largeSetWithIds_Expected = readCsv @"benjaminiHochberg_AdjustedWithR.csv"
    let largeSet_Expected        = largeSetWithIds_Expected |> Array.map snd

    testList "Testing.PValueAdjust.BenjaminiHochberg" [
        
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