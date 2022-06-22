module DistanceMetricsTests
open Expecto
open System
open FSharp.Stats
open FSharp.Stats.DistanceMetrics
open FSharp.Stats.DistanceMetrics.Vector
open FSharp.Stats.DistanceMetrics.Array

[<Tests>]
let euclidianseqfunctiontests =
    testList "DistanceMetrics.euclidiansequence" [
        testCase "euclidian" <| fun () ->
            let seq1 = seq {0.001; -2.0; 0.0; 10000.0}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclidean seq1 seq2 
            Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
        testCase "euclidianinf" <| fun () ->
            let seq1 = seq {0.001; -2.0; -infinity; infinity}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclidean seq1 seq2 
            Expect.equal distance infinity "Should be equal"
        testCase "euclidian0" <| fun () ->
            let seq1 = seq {0.0; 0.0; 0.0; 0.0}
            let seq2 = seq {0.0;0.0;0.0;0.0}
            let distance = FSharp.Stats.DistanceMetrics.euclidean seq1 seq2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidiannan" <| fun () ->
            let seq1 = seq {00.001; -2.0; 0.0; nan}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclidean seq1 seq2 
            Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

        testCase "euclidianNaN" <| fun () ->
            let seq1 = seq {0.001; -2.0; 0.0; 10000.0}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclideanNaN seq1 seq2 
            Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
        testCase "euclidianNaNinf" <| fun () ->
            let seq1 = seq {0.001; -2.0; -infinity; infinity}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclideanNaN seq1 seq2 
            Expect.equal distance infinity "Should be equal"
        testCase "euclidianNaN0" <| fun () ->
            let seq1 = seq {0.0; 0.0; 0.0; 0.0}
            let seq2 = seq {0.0;0.0;0.0;0.0}
            let distance = FSharp.Stats.DistanceMetrics.euclideanNaN seq1 seq2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidianNaNnan" <| fun () ->
            let seq1 = seq {00.001; -2.0; 0.0; nan}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclideanNaN seq1 seq2 
            Expect.floatClose Accuracy.high distance 8.245968773 "Should be equal"

        testCase "euclidianNaNsqrt" <| fun () ->
            let seq1 = seq {0.001; -2.0; 0.0; 10000.0}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclideanNaNSquared seq1 seq2 
            Expect.floatClose Accuracy.high distance 99980069 "Should be equal"
        testCase "euclidianNaNsqrtinf" <| fun () ->
            let seq1 = seq {0.001; -2.0; -infinity; infinity}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclideanNaNSquared seq1 seq2 
            Expect.equal distance infinity "Should be equal"
        testCase "euclidianNaNsqrt0" <| fun () ->
            let seq1 = seq {0.0; 0.0; 0.0; 0.0}
            let seq2 = seq {0.0;0.0;0.0;0.0}
            let distance = FSharp.Stats.DistanceMetrics.euclideanNaNSquared seq1 seq2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidianNaNsqrtnan" <| fun () ->
            let seq1 = seq {00.001; -2.0; 0.0; nan}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.euclideanNaNSquared seq1 seq2 
            Expect.floatClose Accuracy.high distance 67.996001 "Should be equal"
    ]



[<Tests>]
let euclidianvecfunctiontests =
    testList "DistanceMetrics.euclidianvector" [
        testCase "euclidian" <| fun () -> 
            let v1 = vector [0.001; -2.0; 0.0; 10000.0]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.euclidean v1 v2
            Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
        testCase "euclidianinf" <| fun () ->
            let v1 = vector [0.001; -2.0; -infinity; infinity]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.euclidean v1 v2 
            Expect.equal distance infinity "Should be equal"
        testCase "euclidian0" <| fun () ->
            let v1 = vector [0.0; 0.0; 0.0; 0.0]
            let v2 = vector [0.0;0.0;0.0;0.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.euclidean v1 v2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidiannan" <| fun () ->
            let v1 = vector [00.001; -2.0; 0.0; nan]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.euclidean v1 v2 
            Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

        testCase "euclidiansqrt" <| fun () -> 
            let v1 = vector [0.001; -2.0; 0.0; 10000.0]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = euclideanSquared v1 v2
            Expect.floatClose Accuracy.high distance 99980069 "Should be equal"
        testCase "euclidiansqrtinf" <| fun () ->
            let v1 = vector [0.001; -2.0; -infinity; infinity]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = euclideanSquared v1 v2
            Expect.equal distance infinity "Should be equal"
        testCase "euclidiansqrt0" <| fun () ->
            let v1 = vector [0.0; 0.0; 0.0; 0.0]
            let v2 = vector [0.0;0.0;0.0;0.0]
            let distance = euclideanSquared v1 v2
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidiansqrtnan" <| fun () ->
            let v1 = vector [00.001; -2.0; 0.0; nan]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = euclideanSquared v1 v2
            Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

        testCase "euclidianNaN" <| fun () -> 
            let v1 = vector [0.001; -2.0; 0.0; 10000.0]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.euclideanNaN v1 v2
            Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
        testCase "euclidianNaNinf" <| fun () ->
            let v1 = vector [0.001; -2.0; -infinity; infinity]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.euclideanNaN v1 v2
            Expect.equal distance infinity "Should be equal"
        testCase "euclidianNaN0" <| fun () ->
            let v1 = vector [0.0; 0.0; 0.0; 0.0]
            let v2 = vector [0.0;0.0;0.0;0.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.euclideanNaN v1 v2
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidianNaNnan" <| fun () ->
            let v1 = vector [00.001; -2.0; 0.0; nan]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.euclideanNaN v1 v2
            Expect.floatClose Accuracy.high distance 8.245968773 "Should be equal"

    ]

[<Tests>]
let euclidianarrayfunctiontests =
    testList "DistanceMetrics.euclidianarray" [
        testCase "euclidian" <| fun () -> 
            let a1 = [|0.001; -2.0; 0.0; 10000.0|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclidean a1 a2
            Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
        testCase "euclidianinf" <| fun () ->
            let a1 = [|0.001; -2.0; -infinity; infinity|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclidean a1 a2 
            Expect.equal distance infinity "Should be equal"
        testCase "euclidian0" <| fun () ->
            let a1 = [|0.0; 0.0; 0.0; 0.0|]
            let a2 = [|0.0;0.0;0.0;0.0|]
            let distance = euclidean a1 a2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidiannan" <| fun () ->
            let a1 = [|00.001; -2.0; 0.0; nan|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclidean a1 a2 
            Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

        testCase "euclidianNaNsqrt" <| fun () -> 
            let a1 = [|0.001; -2.0; 0.0; 10000.0|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclideanNaNSquared a1 a2
            Expect.floatClose Accuracy.high distance 99980069 "Should be equal"
        testCase "euclidianNaNsqrtinf" <| fun () ->
            let a1 = [|0.001; -2.0; -infinity; infinity|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclideanNaNSquared a1 a2
            Expect.equal distance infinity "Should be equal"
        testCase "euclidianNaNsqrt0" <| fun () ->
            let a1 = [|0.0; 0.0; 0.0; 0.0|]
            let a2 = [|0.0;0.0;0.0;0.0|]
            let distance = euclideanNaNSquared a1 a2
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidianNaNsqrtnan" <| fun () ->
            let a1 = [|00.001; -2.0; 0.0; nan|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclideanNaNSquared a1 a2
            Expect.floatClose Accuracy.high distance 67.996001 "Should be equal"

        testCase "euclidianNaN" <| fun () -> 
            let a1 = [|0.001; -2.0; 0.0; 10000.0|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclideanNaN a1 a2
            Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
        testCase "euclidianNaNinf" <| fun () ->
            let a1 = [|0.001; -2.0; -infinity; infinity|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclideanNaN a1 a2
            Expect.equal distance infinity "Should be equal"
        testCase "euclidianNaN0" <| fun () ->
            let a1 = [|0.0; 0.0; 0.0; 0.0|]
            let a2 = [|0.0;0.0;0.0;0.0|]
            let distance = euclideanNaN a1 a2
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "euclidianNaNnan" <| fun () ->
            let a1 = [|00.001; -2.0; 0.0; nan|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = euclideanNaN a1 a2
            Expect.floatClose Accuracy.high distance 8.245968773 "Should be equal"

    ]

[<Tests>]
let cityblockseqfunctiontests =
    testList "DistanceMetrics.cityblockseq" [
        testCase "cityblock" <| fun () -> 
            let seq1 = seq {0.001; -2.0; 0.0; 10000.0}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.cityblock seq1 seq2
            Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
        testCase "cityblockinf" <| fun () ->
            let seq1 = seq {0.001; -2.0; -infinity; infinity}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.cityblock seq1 seq2
            Expect.equal distance infinity "Should be equal"
        testCase "cityblock0" <| fun () ->
            let seq1 = seq {0.0; 0.0; 0.0; 0.0}
            let seq2 = seq {0.0;0.0;0.0;0.0}
            let distance = FSharp.Stats.DistanceMetrics.cityblock seq1 seq2
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "cityblocknan" <| fun () ->
            let seq1 = seq {0.001; -2.0; 0.0; nan}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.cityblock seq1 seq2
            Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

        testCase "cityblockNaN" <| fun () -> 
            let seq1 = seq {0.001; -2.0; 0.0; 10000.0}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.cityblockNaN seq1 seq2
            Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
        testCase "cityblockNaNinf" <| fun () ->
            let seq1 = seq {0.001; -2.0; -infinity; infinity}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.cityblockNaN seq1 seq2
            Expect.equal distance infinity "Should be equal"
        testCase "cityblockNaN0" <| fun () ->
            let seq1 = seq {0.0; 0.0; 0.0; 0.0}
            let seq2 = seq {0.0;0.0;0.0;0.0}
            let distance = FSharp.Stats.DistanceMetrics.cityblockNaN seq1 seq2
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "cityblockNaNnan" <| fun () ->
            let seq1 = seq {0.001; -2.0; 0.0; nan}
            let seq2 = seq {2.0;-10.0;0.0;1.0}
            let distance = FSharp.Stats.DistanceMetrics.cityblockNaN seq1 seq2
            Expect.floatClose Accuracy.high distance 9.999 "Should be equal"
    ]

[<Tests>]
let cityblockvectorfunctiontests =
    testList "DistanceMetrics.cityblockvector" [
        testCase "cityblock" <| fun () -> 
            let v1 = vector [0.001; -2.0; 0.0; 10000.0]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.cityblock v1 v2
            Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
        testCase "cityblockinf" <| fun () ->
            let v1 = vector [0.001; -2.0; -infinity; infinity]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.cityblock v1 v2 
            Expect.equal distance infinity "Should be equal"
        testCase "cityblock0" <| fun () ->
            let v1 = vector [0.0; 0.0; 0.0; 0.0]
            let v2 = vector [0.0;0.0;0.0;0.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.cityblock v1 v2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "cityblocknan" <| fun () ->
            let v1 = vector [00.001; -2.0; 0.0; nan]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.cityblock v1 v2 
            Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

        testCase "cityblockNaN" <| fun () -> 
            let v1 = vector [0.001; -2.0; 0.0; 10000.0]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.cityblockNaN v1 v2
            Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
        testCase "cityblockNaNinf" <| fun () ->
            let v1 = vector [0.001; -2.0; -infinity; infinity]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.cityblockNaN  v1 v2 
            Expect.equal distance infinity "Should be equal"
        testCase "cityblockNaN0" <| fun () ->
            let v1 = vector [0.0; 0.0; 0.0; 0.0]
            let v2 = vector [0.0;0.0;0.0;0.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.cityblockNaN  v1 v2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "cityblockNaNnan" <| fun () ->
            let v1 = vector [00.001; -2.0; 0.0; nan]
            let v2 = vector [2.0;-10.0;0.0;1.0]
            let distance = FSharp.Stats.DistanceMetrics.Vector.cityblockNaN  v1 v2 
            Expect.floatClose Accuracy.high distance 9.999 "Should be equal"
    ]

[<Tests>]
let cityblockarrayfunctiontests =
    testList "DistanceMetrics.cityblockarray" [
        testCase "cityblock" <| fun () -> 
            let a1 = [|0.001; -2.0; 0.0; 10000.0|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = cityblock a1 a2
            Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
        testCase "cityblockinf" <| fun () ->
            let a1 = [|0.001; -2.0; -infinity; infinity|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = cityblock a1 a2 
            Expect.equal distance infinity "Should be equal"
        testCase "cityblock0" <| fun () ->
            let a1 = [|0.0; 0.0; 0.0; 0.0|]
            let a2 = [|0.0;0.0;0.0;0.0|]
            let distance = cityblock a1 a2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "cityblocknan" <| fun () ->
            let a1 = [|00.001; -2.0; 0.0; nan|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = cityblock a1 a2 
            Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

        testCase "cityblockNaN" <| fun () -> 
            let a1 = [|0.001; -2.0; 0.0; 10000.0|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = cityblockNaN a1 a2
            Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
        testCase "cityblockNaNinf" <| fun () ->
            let a1 = [|0.001; -2.0; -infinity; infinity|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = cityblockNaN a1 a2 
            Expect.equal distance infinity "Should be equal"
        testCase "cityblockNaN0" <| fun () ->
            let a1 = [|0.0; 0.0; 0.0; 0.0|]
            let a2 = [|0.0;0.0;0.0;0.0|]
            let distance = cityblockNaN a1 a2 
            Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
        testCase "cityblockNaNnan" <| fun () ->
            let a1 = [|00.001; -2.0; 0.0; nan|]
            let a2 = [|2.0;-10.0;0.0;1.0|]
            let distance = cityblockNaN a1 a2 
            Expect.floatClose Accuracy.high distance 9.999 "Should be equal"
    ]

[<Tests>]
let Levenshteindistancetest =
    testList "DistanceMetrics.levenshteindistance" [
        // normal test case from Aung, K. M. M. (2019). Comparison of levenshtein distance algorithm and needleman-wunsch distance algorithm for string matching (Doctoral dissertation, MERAL Portal).
        testCase "Levenstein" <|fun () ->
            let string1 = "hello"
            let string2 = "helo"
            let distnace = wagnerFischerLazy string1 string2
            Expect.equal distnace 1 "should be equal"
        //TestCases from R stringdist('hello', '', method = 'lv') "R version 4.0.3 (2020-10-10)"
        testCase "Levensteinoneempty" <|fun () ->
            let string1 = "hello"
            let string2 = ""
            let distance = wagnerFischerLazy string1 string2
            Expect.equal distance 5 "should be equal"
        //TestCases from R stringdist('', '', method = 'lv') "R version 4.0.3 (2020-10-10)"
        testCase "Levensteinbothempty" <|fun ()->
            let string1 = ""
            let string2 = ""
            let distance = wagnerFischerLazy string1 string2
            Expect.equal distance 0 "should be equal"
    
    ]