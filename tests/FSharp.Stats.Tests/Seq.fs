module SeqTests

open Expecto
open System
open FSharp.Stats

let testSeqEvenCounts = seq [10000.;-0.1;14.;-10.]
let testSeqOddCounts = seq [10000.;-0.1;14.;-10.;5.]
let testSeqNan = seq [10000.;-0.1;14.;-10.;5.;Double.NaN]
let testSeqInfinity = seq [10000.;-0.1;14.;-10.;Double.PositiveInfinity]
let testSeqNegInfinity = seq [10000.;-0.1;14.;-10.;5.;Double.NegativeInfinity]

let testSeqEvenCountsInt = seq [10000;-50;14;-9]
let testSeqOddCountsInt = seq [10000;-50;14;-10;5]

[<Tests>]
let medianTests =
    testList "Seq" [
        testCase "medianEvenCounts" <| fun () ->
            let median = Seq.median testSeqEvenCounts
            Expect.floatClose Accuracy.high median 6.95 "Median should be 6.95"
        testCase "medianOddCounts" <| fun () ->
            let median = Seq.median testSeqOddCounts
            Expect.floatClose Accuracy.high median 5. "Median should be 5.0"
        testCase "medianNan" <| fun () ->
            let median = Seq.median testSeqNan
            Expect.isTrue (nan.Equals(median)) "Median should be nan"
        testCase "medianInf" <| fun () ->
            let median = Seq.median testSeqInfinity
            Expect.floatClose Accuracy.high median 14. "Median should be 14.0"
        testCase "medianNegInf" <| fun () ->
            let median = Seq.median testSeqNegInfinity
            Expect.floatClose Accuracy.high median 2.45 "Median should be 2.45"
        
        testCase "testListEvenCountsInt" <| fun () ->
            let median = Seq.median testSeqEvenCountsInt
            Expect.equal median 2 "Median should be 2"
        testCase "testListOddCountsInt" <| fun () ->
            let median = Seq.median testSeqOddCountsInt
            Expect.equal median 5 "Median should be 5"
    ]

[<Tests>]
let meanTests =
    testList "Seq" [
        testCase "mean" <| fun () ->
            let mean = Seq.mean testSeqEvenCounts
            Expect.floatClose Accuracy.high mean 2500.975 "Mean should be 2500.975"
        testCase "meanNan" <| fun () ->
            let mean = Seq.mean testSeqNan
            Expect.isTrue (Double.IsNaN mean) "Mean should be nan"
        testCase "meanInf" <| fun () ->
            let mean = Seq.mean testSeqInfinity
            Expect.isTrue (Double.IsInfinity mean) "Mean should be inf"
        testCase "meanNegInf" <| fun () ->
            let mean = Seq.mean testSeqNegInfinity
            Expect.isTrue (Double.IsNegativeInfinity mean) "Mean should be nan"
    ]

[<Tests>]
let meanQuadraticTests =
    testList "Seq" [
        testCase "meanQuadratic" <| fun () ->
            let mean = Seq.meanQuadratic testSeqEvenCounts
            Expect.floatClose Accuracy.high mean 5000.0074 "Mean should be 5000.0074"
        testCase "meanQuadraticNan" <| fun () ->
            let mean = Seq.meanQuadratic testSeqNan
            Expect.isTrue (Double.IsNaN mean) "Mean should be nan"
    ]


