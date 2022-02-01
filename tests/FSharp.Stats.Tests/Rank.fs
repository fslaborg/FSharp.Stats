module RankTests
open Expecto
open System
open FSharp.Stats
    

[<Tests>]
let rankTests = 
    let arrayA = [|6.;6.;6.;1.;-2.;3.;3.;3.;3.;2.5;4.;-2.;0.;5.;1.|]

    let exAvg = [|14.0;14.0;14.0;4.5;1.5;8.5;8.5;8.5;8.5;6.0;11.0;1.5;3.0;12.0;4.5|]

    let exFst = [|13.0;14.0;15.0;4.0;1.0;7.0;8.0;9.0;10.0;6.0;11.0;2.0;3.0;12.0;5.0|]

    let exMax = [|15.0;15.0;15.0;5.0;2.0;10.0;10.0;10.0;10.0;6.0;11.0;2.0;3.0;12.0;5.0|]

    let exMin = [|13.0;13.0;13.0;4.0;1.0;7.0;7.0;7.0;7.0;6.0;11.0;1.0;3.0;12.0;4.0|]

    testList "Rank" [
        testCase "rankAverage" <| fun () -> 
            Expect.sequenceEqual (Rank.rankAverage arrayA) exAvg "ranks should be equal"
        testCase "rankFirst" <| fun () -> 
            Expect.sequenceEqual (Rank.rankFirst arrayA) exFst "ranks should be equal"
        testCase "rankMax" <| fun () -> 
            Expect.sequenceEqual (Rank.rankMax arrayA) exMax "ranks should be equal"
        testCase "rankMin" <| fun () -> 
            Expect.sequenceEqual (Rank.rankMin arrayA) exMin "ranks should be equal"
    ]

