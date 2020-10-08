namespace FSharp.Stats.Tests
open Expecto

module TestingTests =
    open System
    open FSharp.Stats.Testing
    
    [<Tests>]
    let testPostHoc =
        //Tests taken from:
        //https://www.icalcu.com/stat/anova-tukey-hsd-calculator.html
        testList "Testing.PostHoc" [
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
        ]


    let hTest = 
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

    let chiSquared = 
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
