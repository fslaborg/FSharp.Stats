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




