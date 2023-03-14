module SeqTests

open Expecto
open System
open FSharp.Stats
open TestExtensions

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


[<Tests>]
let geomspaceTests =
    testList "Seq" [
        testCase "geomspace_0" <| fun () ->
            let expected = Seq.geomspace(10, 1000, 3)
            let actual = seq {10.0; 100.0; 1000.0}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_1" <| fun () ->
            let expected = Seq.geomspace(10, 1000, 2, IncludeEndpoint = false)
            let actual = seq {10.0; 100.0}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_2" <| fun () ->
            let expected = Seq.geomspace(8, 2, 3)
            let actual = seq {8.0; 4.0; 2.0}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_3" <| fun () ->
            let expected = Seq.geomspace(0.1, 10, 3)
            let actual = seq {0.1; 1.0; 10.0}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_4" <| fun () ->
            let expected = Seq.geomspace(2., 2. ** 50.)
            let actual =
                seq {2.; 4.; 8.; 16.; 32.; 64.; 128.; 256.; 512.; 1024.;
                2048.; 4096.; 8192.; 16384.; 32768.; 65536.; 131072.;
                262144.; 524288.; 1048576.; 2097152.; 4194304.; 8388608.;
                16777216.; 33554432.; 67108864.; 134217728.; 268435456.;
                536870912.; 1073741824.; 2147483648.; 4294967296.; 8589934592.;
                17179869184.; 34359738368.; 68719476736.; 137438953472.;
                274877906944.; 549755813888.; 1099511627776.; 2199023255552.;
                4398046511104.; 8796093022208.; 17592186044416.; 35184372088832.;
                70368744177664.; 140737488355328.; 281474976710656.;
                562949953421312.; 1125899906842624.}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_5" <| fun () ->
            let expected() = Seq.geomspace(-2., 2., 3) |> ignore
            Expect.throws expected "geomspace cannot be initialized with negative values."

        testCase "geomspace_6" <| fun () ->
            let expected() = Seq.geomspace(2., -2.) |> ignore
            Expect.throws expected "geomspace cannot be initialized with negative values."

        testCase "geomspace_7" <| fun () ->
            let expected() = Seq.geomspace(-2., -20.) |> ignore
            Expect.throws expected "geomspace cannot be initialized with negative values."
    ]

