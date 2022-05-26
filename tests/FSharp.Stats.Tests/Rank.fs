module RankTests
open Expecto
open System
open FSharp.Stats
    

[<Tests>]
let rankTests = 
    let arrayA = [|6.;6.;6.;1.;-2.;3.;3.;3.;3.;2.5;4.;-2.;0.;5.;1.|]
    let arrayB = [|6;6;6;1;-2;3;3;3;3;2;4;-2;0;5;1|]
    let arrayC = [|-infinity;nan;nan;1.;-2.;3.;3.;3.;infinity;2.5;infinity;-2.;0.;5.;1.|]
    
    let exFstA = [|13.;14.;15.;4.;1.;7.;8.;9.;10.;6.;11.;2.;3.;12.;5.|]
    let exAvgA = [|14.;14.;14.;4.5;1.5;8.5;8.5;8.5;8.5;6.;11.;1.5;3.;12.;4.5|] 
    let exMaxA = [|15.;15.;15.;5.;2.;10.;10.;10.;10.;6.;11.;2.;3.;12.;5.;|]
    let exMinA = [|13.;13.;13.;4.;1.;7.;7.;7.;7.;6.;11.;1.;3.;12.;4.;|]
    
    //C default sorting
    let exAvgCNanFirst = [|3.;1.;2.;7.5;4.5;11.;11.;11.;14.5;9.;14.5;4.5;6.;13.;7.5;|]
    let exFstCNanFirst = [|3.;1.;2.;7.;4.;10.;11.;12.;14.;9.;15.;5.;6.;13.;8.;|]
    let exMaxCNanFirst = [|3.;1.;2.;8.;5.;12.;12.;12.;15.;9.;15.;5.;6.;13.;8.;|]
    let exMinCNanFirst = [|3.;1.;2.;7.;4.;10.;10.;10.;14.;9.;14.;4.;6.;13.;7.;|]
    
    //C nan last
    let exAvgCNanLast = [|1.;14.;15.;5.5;2.5;9.;9.;9.;12.5;7.;12.5;2.5;4.;11.;5.5;|]
    let exFstCNanLast = [|1.;14.;15.;5.;2.;8.;9.;10.;12.;7.;13.;3.;4.;11.;6.;|]
    let exMaxCNanLast = [|1.;14.;15.;6.;3.;10.;10.;10.;13.;7.;13.;3.;4.;11.;6.;|]
    let exMinCNanLast = [|1.;14.;15.;5.;2.;8.;8.;8.;12.;7.;12.;2.;4.;11.;5.;|]

    //C assign nan to nan ranks
    let exAvgNanNan = [|1.;nan;nan;5.5;2.5;9.;9.;9.;12.5;7.;12.5;2.5;4.;11.;5.5;|]
    let exFstNanNan = [|1.;nan;nan;5.;2.;8.;9.;10.;12.;7.;13.;3.;4.;11.;6.;|]
    let exMaxNanNan = [|1.;nan;nan;6.;3.;10.;10.;10.;13.;7.;13.;3.;4.;11.;6.;|]
    let exMinNanNan = [|1.;nan;nan;5.;2.;8.;8.;8.;12.;7.;12.;2.;4.;11.;5.;|]

    testList "Rank" [
        testCase "RankAverage" <| fun () -> 
            Expect.sequenceEqual (Rank.RankAverage(false,false) arrayA) exAvgA "ranks should be equal"
        testCase "RankFirst" <| fun () -> 
            Expect.sequenceEqual (Rank.RankFirst(false,false) arrayA) exFstA "ranks should be equal"
        testCase "RankMax" <| fun () -> 
            Expect.sequenceEqual (Rank.RankMax(false,false) arrayA) exMaxA "ranks should be equal"
        testCase "RankMin" <| fun () -> 
            Expect.sequenceEqual (Rank.RankMin(false,false) arrayA) exMinA "ranks should be equal"
        testCase "RankFirstInt" <| fun () -> 
            Expect.sequenceEqual (Rank.RankFirst(false,false) arrayB) exFstA "ranks for ints should be equal"

            
        testCase "RankAverageNaNFirst" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1 (Rank.RankAverage(false,false) arrayC) exAvgCNanFirst "ranks should be equal"
        testCase "RankFirstNaNFirst" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankFirst(false,false) arrayC) exFstCNanFirst "ranks should be equal"
        testCase "RankMaxNaNFirst" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankMax(false,false) arrayC) exMaxCNanFirst "ranks should be equal"
        testCase "RankMinNaNFirst" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankMin(false,false) arrayC) exMinCNanFirst "ranks should be equal"

        testCase "RankAverageNaNLast" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankAverage(true,false) arrayC) exAvgCNanLast "ranks should be equal"
        testCase "RankFirstNaNLast" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankFirst(true,false) arrayC) exFstCNanLast "ranks should be equal"
        testCase "RankMaxNaNLast" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankMax(true,false) arrayC) exMaxCNanLast "ranks should be equal"
        testCase "RankMinNaNLast" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankMin(true,false) arrayC) exMinCNanLast "ranks should be equal"

        
        testCase "RankAverageSetNanToNan" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankAverage(true,true) arrayC) exAvgNanNan "ranks should be equal"
        testCase "RankFirstSetNanToNan" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankFirst(true,true) arrayC) exFstNanNan "ranks should be equal"
        testCase "RankMaxSetNanToNan" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankMax(true,true) arrayC) exMaxNanNan "ranks should be equal"
        testCase "RankMinSetNanToNan" <| fun () -> 
            TestExtensions.TestExtensions.sequenceEqualRoundedNaN 1  (Rank.RankMin(true,true) arrayC) exMinNanNan "ranks should be equal"

    ]

