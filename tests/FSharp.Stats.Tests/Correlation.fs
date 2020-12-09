namespace FSharp.Stats.Tests
open Expecto

module CorrelationTests =
    open FSharp.Stats.Correlation
    let testKendallCorrelation =
        // tested with R Kendall(x,y) function
        testList "Correlation.Seq" [
            testCase "kendall" <| fun () ->
                let xs = [|-0.5;-0.4 ;0.  ;0.7;0.65;0.9649|]
                let ys = [|-0.3;-0.25;-0.1;-0.46;0.103;0.409|]
                let tau = Seq.kendall xs ys
                Expect.floatClose Accuracy.high tau 0.4666666667 "Should be equal (double precision)"
            
        //ToDo ties tau_a,tau_b,tau_c
        ]