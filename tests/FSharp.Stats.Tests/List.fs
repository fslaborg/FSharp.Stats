module ListTests

open Expecto
open System
open FSharp.Stats

let testListEvenCounts = [10000.;-0.1;14.;-10.]
let testListOddCounts = [10000.;-0.1;14.;-10.;5.]
let testListNan = [10000.;-0.1;14.;-10.;5.;Double.NaN]
let testListInfinity = [10000.;-0.1;14.;-10.;Double.PositiveInfinity]
let testListNegInfinity = [10000.;-0.1;14.;-10.;5.;Double.NegativeInfinity]

let testListEvenCountsInt = [10000;-50;14;-9]
let testListOddCountsInt = [10000;-50;14;-10;5]

[<Tests>]
let medianTests =
    testList "List" [
        testCase "medianEvenCounts" <| fun () ->
            let median = List.median testListEvenCounts
            Expect.floatClose Accuracy.high median 6.95 "Median should be 6.95"
        testCase "medianOddCounts" <| fun () ->
            let median = List.median testListOddCounts
            Expect.floatClose Accuracy.high median 5. "Median should be 5.0"
        testCase "medianNan" <| fun () ->
            let median = List.median testListNan
            Expect.isTrue (Double.IsNaN median) "Median should be nan"
        testCase "medianInf" <| fun () ->
            let median = List.median testListInfinity
            Expect.floatClose Accuracy.high median 14. "Median should be 14.0"
        testCase "medianNegInf" <| fun () ->
            let median = List.median testListNegInfinity
            Expect.floatClose Accuracy.high median 2.45 "Median should be 2.45"
        
        testCase "testListEvenCountsInt" <| fun () ->
            let median = List.median testListEvenCountsInt
            Expect.equal median 2 "Median should be 2"
        testCase "testListOddCountsInt" <| fun () ->
            let median = List.median testListOddCountsInt
            Expect.equal median 5 "Median should be 5"
    ]

[<Tests>]
let meanTests =
    testList "List" [
        testCase "mean" <| fun () ->
            let mean = List.mean testListEvenCounts
            Expect.floatClose Accuracy.high mean 2500.975 "Mean should be 2500.975"
        testCase "meanNan" <| fun () ->
            let mean = List.mean testListNan
            Expect.isTrue (Double.IsNaN mean) "Mean should be nan"
        testCase "meanInf" <| fun () ->
            let mean = List.mean testListInfinity
            Expect.isTrue (Double.IsInfinity mean) "Mean should be inf"
        testCase "meanNegInf" <| fun () ->
            let mean = List.mean testListNegInfinity
            Expect.isTrue (Double.IsNegativeInfinity mean) "Mean should be nan"
    ]
