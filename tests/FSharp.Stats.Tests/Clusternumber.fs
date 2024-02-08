module ClusternumberTests
open System
open FSharp.Stats.ML.Unsupervised.ClusterNumber
open Expecto

[<Tests>]
let nmi =

    testList "ML.Unsupervised.Clusternumber" [
        testCase "calcNMI" <| fun () ->
            let trueLabels = [|"blue";"blue";"yellow";"red";"yellow"|]
            let trueLabelsAsInt = [|1; 1; 3; 2; 3|]
            let clusteredLabels = [|6; 6; 5; 5; 5|]
            let nmi0 = calcNMI trueLabelsAsInt clusteredLabels    
            Expect.floatClose Accuracy.high nmi0 0.77897941733453601731 "Should be equal (double precision)"
       
            let trueLabelsAsInt1 = [|1; 1; 3; 2|]
            let clusteredLabels1 = [|6; 6; 5; 5; 5|]
            let nmi1() = calcNMI trueLabelsAsInt1 clusteredLabels1   
            Expect.throws (fun () -> nmi1 () |> ignore ) "Should be equal (double precision)"

            let trueLabelsAsInt2 = [|0; 0; 1; 1|]
            let clusteredLabels2 = [|1; 1; 0; 0|]
            let nmi2 = calcNMI trueLabelsAsInt2 clusteredLabels2  
            Expect.floatClose Accuracy.high nmi2 1 "Should be equal (double precision)"

            let trueLabelsAsInt0 = [|0; 1; 2; 3|]
            let clusteredLabels0 = [|0; 0; 0; 0|]
            let nmi3 = calcNMI trueLabelsAsInt0 clusteredLabels0   
            Expect.floatClose Accuracy.high nmi3 0 "Should be equal (double precision)"


            let trueLabelsAsIntPYTest = [|0; 0; 0; 1; 1; 1; 1; 2; 2; 2|]
            let clusteredLabelsPYTest = [|0; 0; 1; 2; 2; 2; 2; 2; 2; 2|]
            let nmi4 = calcNMI trueLabelsAsIntPYTest clusteredLabelsPYTest    
            Expect.floatClose Accuracy.high nmi4 0.64617159354446 "Should be equal (double precision)"


            let trueLabelsAsIntPYTest2 = [|3;4;3|]
            let clusteredLabelsPYTest2 = [|2;1;7|]
            let nmi5 = calcNMI trueLabelsAsIntPYTest2 clusteredLabelsPYTest2    
            Expect.floatClose Accuracy.high nmi5 0.7336804366512112 "Should be equal (double precision)"
    ]
