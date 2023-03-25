module ConfidenceIntervalTests
open System
open FSharp.Stats
open Expecto


[<Tests>]
let ci =
    testList "ConfidenceInterval" [
        //tested against R: 
        //options(digits=13)
        //xs <- c(-0.5,-0.4 ,0.  ,0.7,0.65,0.9649)
        //df<- data.frame(xs)
        //f <- lm(xs ~ 1,df)
        //confint(f,level=0.95)
            
        //sample the ci should be calculated on
        let xs = [|-0.5; -0.4; 0.0; 0.7; 0.65; 0.9649|]

        testCase "ci_0.95" <| fun () ->
            let ex_min = -0.414473646404
            let ex_max =  0.8861069797373
            let ac_interval = ConfidenceInterval.ci 0.95 xs
            let ac_min = Intervals.getStart ac_interval
            let ac_max = Intervals.getEnd ac_interval
            Expect.floatClose Accuracy.medium ac_min ex_min "Should be equal (double precision)"
            Expect.floatClose Accuracy.medium ac_max ex_max "Should be equal (double precision)"

        testCase "ci_0.05" <| fun () ->
            let ex_min = 0.21914192348
            let ex_max = 0.2524914098533
            let ac_interval = ConfidenceInterval.ci 0.05 xs
            let ac_min = Intervals.getStart ac_interval
            let ac_max = Intervals.getEnd ac_interval
            Expect.floatClose Accuracy.high ac_min ex_min "Should be equal (double precision)"
            Expect.floatClose Accuracy.high ac_max ex_max "Should be equal (double precision)"
    ]


