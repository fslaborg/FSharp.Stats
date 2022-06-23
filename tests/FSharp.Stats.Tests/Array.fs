module ArrayTests

open Expecto
open System
open FSharp.Stats

let testArrayEvenCounts = [|10000.;-0.1;14.;-10.|]
let testArrayOddCounts = [|10000.;-0.1;14.;-10.;5.|]
let testArrayNan = [|10000.;-0.1;14.;-10.;5.;Double.NaN|]
let testArrayInfinity = [|10000.;-0.1;14.;-10.;Double.PositiveInfinity|]
let testArrayNegInfinity = [|10000.;-0.1;14.;-10.;5.;Double.NegativeInfinity|]

let testArrayEvenCountsInt = [|10000;-50;14;-9|]
let testArrayOddCountsInt = [|10000;-50;14;-10;5|]

[<Tests>]
let medianTests =
    testList "Array" [
        testCase "medianEvenCounts" <| fun () ->
            let median = Array.median testArrayEvenCounts
            Expect.floatClose Accuracy.high median 6.95 "Median should be 6.95"
        testCase "medianOddCounts" <| fun () ->
            let median = Array.median testArrayOddCounts
            Expect.floatClose Accuracy.high median 5. "Median should be 5.0"
        testCase "medianNan" <| fun () ->
            let median = Array.median testArrayNan
            Expect.isTrue (nan.Equals(median)) "Median should be nan"
        testCase "medianInf" <| fun () ->
            let median = Array.median testArrayInfinity
            Expect.floatClose Accuracy.high median 14. "Median should be 14.0"
        testCase "medianNegInf" <| fun () ->
            let median = Array.median testArrayNegInfinity
            Expect.floatClose Accuracy.high median 2.45 "Median should be 2.45"
        
        testCase "testListEvenCountsInt" <| fun () ->
            let median = Array.median testArrayEvenCountsInt
            Expect.equal median 2 "Median should be 2"
        testCase "testListOddCountsInt" <| fun () ->
            let median = Array.median testArrayOddCountsInt
            Expect.equal median 5 "Median should be 5"
    ]
