module SeqTests

open Expecto
open System
open FSharp.Stats
open TestExtensions

/// Linear congruential generator
/// Used for generating longer consistent sequences
let lcg (seed: int) =
    let m = 2147483647I
    let a = 16807I     
    let c = 0I
    (bigint seed) 
    |> Seq.unfold 
        (fun state ->
            let next = (a * state + c) % m
            Some (next, next)
        ) 

let seqGenDbl mul len = lcg len |> Seq.map (fun i -> float i / float System.Int32.MaxValue) |> Seq.map (fun x -> x*mul - mul/2.0) |> Seq.take len 
let seqGenInts len = lcg len |> Seq.map (fun i -> i) |> Seq.take len
let seqGen len = seqGenDbl 1000.0 len

let testSeqEvenCounts = seq [10000.;-0.1;14.;-10.]
let testSeqOddCounts = seq [10000.;-0.1;14.;-10.;5.]
let testSeqNan = seq [10000.;-0.1;14.;-10.;5.;Double.NaN]
let testSeqInfinity = seq [10000.;-0.1;14.;-10.;Double.PositiveInfinity]
let testSeqNegInfinity = seq [10000.;-0.1;14.;-10.;5.;Double.NegativeInfinity]
let testSeqEvenCountsInt = seq [10000;-50;14;-9]
let testSeqOddCountsInt = seq [10000;-50;14;-10;5]

[<Tests>]
let medianTests =
    testList "Seq.median" [
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
let rangeTests =
    testList "Seq.range" [
        testCase "Empty sequence" <| fun () ->
            Expect.equal (Seq.range Seq.empty) Interval.Empty "Range of empty sequence should be Empty"

        testCase "One element sequence" <| fun () ->
            Expect.equal (Seq.range [42]) (Interval.Closed(42, 42)) "Range of one element sequence should be Closed(x, x)"

        testCase "Two element sequence" <| fun () ->
            Expect.equal (Seq.range [1; 2]) (Interval.Closed(1, 2)) "Range of two element sequence should be Closed(min, max)"

        testCase "All same element sequence" <| fun () ->
            Expect.equal (Seq.range [5; 5; 5; 5]) (Interval.Closed(5, 5)) "Range of all same element sequence should be Closed(x, x)"

        testCase "All different element sequence" <| fun () ->
            Expect.equal (Seq.range [1; 4; 2; 8; 3]) (Interval.Closed(1, 8)) "Range of all different element sequence should be Closed(min, max)"

        // Currently this is undefined and will depend on the order of the sequence
        //testCase "Sequence with NaN" <| fun () ->
        //    Expect.equal (Seq.range [1.0; 2.0; nan; 3.0]) (Interval.Closed(nan, nan)) "Range of sequence with NaN should be Closed(nan, nan)"

        testCase "Sequence with Infinity" <| fun () ->
            Expect.equal (Seq.range [1.0; 2.0; infinity]) (Interval.Closed(1.0, infinity)) "Range of sequence with Infinity should be Closed(min, infinity)"

        testCase "Sequence with Negative Infinity" <| fun () ->
            Expect.equal (Seq.range [1.0; 2.0; -infinity]) (Interval.Closed(-infinity, 2.0)) "Range of sequence with Negative Infinity should be Closed(-infinity, max)"

        testCase "Sequence with negative values" <| fun () ->
            Expect.equal (Seq.range [-1; -4; -2]) (Interval.Closed(-4, -1)) "Range of sequence with negative values should be Closed(min, max)"

        testCase "Sequence with positive values" <| fun () ->
            Expect.equal (Seq.range [1; 4; 2]) (Interval.Closed(1, 4)) "Range of sequence with positive values should be Closed(min, max)"

        testCase "Sequence with mixed values" <| fun () ->
            Expect.equal (Seq.range [-1; 4; -2; 8]) (Interval.Closed(-2, 8)) "Range of sequence with mixed values should be Closed(min, max)"

        testCase "Sequence with Int32 values" <| fun () ->
            Expect.equal (Seq.range [1; 4; 2]) (Interval.Closed(1, 4)) "Range of sequence with Int32 values should be Closed(min, max)"

        testCase "Sequence with Int64 values" <| fun () ->
            Expect.equal (Seq.range [1L; 4L; 2L]) (Interval.Closed(1L, 4L)) "Range of sequence with Int64 values should be Closed(min, max)"

        testCase "Sequence with string values" <| fun () ->
            Expect.equal (Seq.range ["a"; "c"; "b"]) (Interval.Closed("a", "c")) "Range of sequence with string values should be Closed(min, max)"

        testCase "Sequence with null string values" <| fun () ->
            Expect.equal (Seq.range ["a"; null; "b"]) (Interval.Closed(null, "b")) "Range of sequence with null string value should be Closed(null, max)"
    ]

[<Tests>]
let meanTests =
    testList "Seq.mean" [
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
let meanByTests =
    testList "Seq.meanBy" [
        testCase "Empty seq" <| fun () ->
            Expect.isTrue (Seq.meanBy sin Seq.empty<float> |> Double.IsNaN) "Expected NaN"
            
        testCase "One element seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy sin (seq [42.0])) -0.916522 "Expected -0.916522"
            
        testCase "Two element seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy sin (seq [1.0; 2.0])) 0.875384 "Expected 0.875384"
            
        testCase "All same seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy sin (seq [5.0; 5.0; 5.0])) -0.958924 "Expected -0.958924"
            
        testCase "All different seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy sin (seq [1.0; 2.0; 3.0])) 0.630629 "Expected 0.630629"
            
        testCase "Seq with NaN" <| fun () ->
            Expect.isTrue (Seq.meanBy sin (seq [1.0; nan; 2.0]) |> Double.IsNaN) "Expected NaN"
            
        testCase "Seq with Infinity" <| fun () ->
            Expect.isTrue (Seq.meanBy sin (seq [1.0; infinity; 2.0]) |> Double.IsNaN) "Expected NaN"
            
        testCase "Seq with -Infinity" <| fun () ->
            Expect.isTrue (Seq.meanBy sin (seq [1.0; -infinity; 2.0]) |> Double.IsNaN) "Expected NaN"
            
        testCase "Negative seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy sin (seq [-1.0; -2.0; -3.0])) -0.630629 "Expected -0.630629"
            
        testCase "Positive seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy sin (seq [1.0; 2.0; 3.0])) 0.630629 "Expected 0.630629"
            
        testCase "Mixed seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy sin (seq [-1.0; 2.0; -3.0])) -0.024431 "Expected -0.024431"
            
        testCase "Int32 seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy (float >> sin) (seq [1; 2; 3])) 0.630629 "Expected 0.630629"
            
        testCase "Int64 seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy (float >> sin) (seq [1L; 2L; 3L])) 0.630629 "Expected 0.630629"
            
        testCase "String seq" <| fun () ->
            Expect.floatClose Accuracy.medium (Seq.meanBy (float << String.length) (seq ["hello"; "world"; "!"])) 3.666667 "Expected 3.666667"
    ]

[<Tests>]
let weightedMeanTests =
    testList "Seq.weightedMean" [
        testCase "basic" <| fun () ->
            let weights = [0.1; 0.2; 0.3; 0.2; 0.2] 
            let values = [1.0; 2.0; 3.0; 4.0; 5.0]
            let wMean = Seq.weightedMean weights values
            Expect.floatClose Accuracy.high wMean 3.2 "Weighted mean should be 3.2"

        testCase "emptySeq" <| fun () ->
            let emptyWeights = Seq.empty<float>
            let emptyValues = Seq.empty<float>
            let wMean = Seq.weightedMean emptyWeights emptyValues 
            Expect.isTrue (Double.IsNaN(wMean)) "Weighted mean of empty seq should be NaN"
        
        testCase "oneElement" <| fun () ->
            let oneElemWeights = seq [1.0]
            let oneElemValues = seq [5.0]
            let wMean = Seq.weightedMean oneElemWeights oneElemValues
            Expect.floatClose Accuracy.high wMean 5.0 "Weighted mean of one element seq should be the element value"
        
        testCase "twoElements" <| fun () -> 
            let twoElemWeights = seq [0.4;0.6]
            let twoElemValues = seq [2.0;4.0]
            let wMean = Seq.weightedMean twoElemWeights twoElemValues
            Expect.floatClose Accuracy.high wMean 3.2 "Weighted mean of [2.0,4.0] with weights [0.4,0.6] should be 3.2"
        
        testCase "allSameElements" <| fun () ->
            let allSameWeights = seq [0.2;0.2;0.2;0.2;0.2] 
            let allSameValues = seq [3.0;3.0;3.0;3.0;3.0]
            let wMean = Seq.weightedMean allSameWeights allSameValues
            Expect.floatClose Accuracy.high wMean 3.0 "Weighted mean of all same elements should be the element value"
        
        testCase "nanValue" <| fun () ->
            let nanWeights = seq [0.5;0.5]
            let nanValues = seq [1.0;nan]
            let wMean = Seq.weightedMean nanWeights nanValues
            Expect.isTrue (Double.IsNaN(wMean)) "Weighted mean of seq containing NaN should be NaN"
        
        testCase "infValue" <| fun () ->
            let infWeights = seq [0.5;0.5]
            let infValues = seq [1.0;infinity] 
            let wMean = Seq.weightedMean infWeights infValues
            Expect.equal wMean infinity "Weighted mean of seq containing infinity should be infinity"
        
        testCase "negInfValue" <| fun () ->
            let negInfWeights = seq [0.5;0.5]
            let negInfValues = seq [1.0;-infinity]
            let wMean = Seq.weightedMean negInfWeights negInfValues
            Expect.equal wMean -infinity "Weighted mean of seq containing -infinity should be -infinity"
        
        testCase "negativeValues" <| fun () ->
            let negativeWeights = seq [-0.1;-0.2;-0.3;-0.2;-0.2] 
            let negativeValues = seq [-1.0;-2.0;-3.0;-4.0;-5.0]
            let wMean = Seq.weightedMean negativeWeights negativeValues
            Expect.floatClose Accuracy.high wMean -3.2 "Weighted mean of negative values should be -3.2"
        
        testCase "positiveValues" <| fun () ->
            let positiveWeights = seq [0.1;0.2;0.3;0.2;0.2]
            let positiveValues = seq [1.0;2.0;3.0;4.0;5.0]
            let wMean = Seq.weightedMean positiveWeights positiveValues  
            Expect.floatClose Accuracy.high wMean 3.2 "Weighted mean of positive values should be 3.2"
        
        testCase "mixedValues" <| fun () ->
            let mixedWeights = seq [-0.1;0.2;-0.3;0.2;-0.2]
            let mixedValues = seq [-1.0;2.0;-3.0;4.0;-5.0] 
            let wMean = Seq.weightedMean mixedWeights mixedValues
            Expect.floatClose Accuracy.high wMean -16.0 "Weighted mean of mixed values should be -16.0"
        
        testCase "int32Values" <| fun () ->
            let int32Weights = seq [1;2;3;2;2]
            let int32Values = seq [1;2;3;4;5]
            let wMean = Seq.weightedMean int32Weights int32Values
            Expect.equal wMean 3 "Weighted mean of int32 values should be 3"
        
        testCase "int64Values" <| fun () ->
            let int64Weights = seq [1L;2L;3L;2L;2L]
            let int64Values = seq [1L;2L;3L;4L;5L]
            let wMean = Seq.weightedMean int64Weights int64Values
            Expect.equal wMean 3L "Weighted mean of int64 values should be 3L"
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
            let expected = Seq.geomspace(start= 10, stop= 1000, num= 3)
            let actual = seq {10.0; 100.0; 1000.0}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_1" <| fun () ->
            let expected = Seq.geomspace(start= 10, stop= 1000, num= 2, IncludeEndpoint = false)
            let actual = seq {10.0; 100.0}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_2" <| fun () ->
            let expected = Seq.geomspace(start= 8, stop= 2, num= 3)
            let actual = seq {8.0; 4.0; 2.0}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_3" <| fun () ->
            let expected = Seq.geomspace(start= 0.1, stop= 10, num= 3)
            let actual = seq {0.1; 1.0; 10.0}
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "geomspace results in wrong seq"

        testCase "geomspace_4" <| fun () ->
            let expected = Seq.geomspace(start= 2., stop= 2. ** 50., num= 50)
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
            let expected() = Seq.geomspace(start= -2., stop= 2., num= 3) |> ignore
            Expect.throws expected "geomspace cannot be initialized with negative values."

        testCase "geomspace_6" <| fun () ->
            let expected() = Seq.geomspace(start= 2., stop= -2., num= 3) |> ignore
            Expect.throws expected "geomspace cannot be initialized with negative values."

        testCase "geomspace_7" <| fun () ->
            let expected() = Seq.geomspace(start= -2., stop= -20., num= 3) |> ignore
            Expect.throws expected "geomspace cannot be initialized with negative values."
    ]


[<Tests>]
let meanHarmonicTests =
    testList "Seq.meanHarmonic" [
        testCase "Empty sequence" <| fun () ->
            Expect.isTrue (Seq.meanHarmonic Seq.empty<float> |> System.Double.IsNaN) "Expected NaN for empty sequence"

        testCase "One element sequence" <| fun () ->
            Expect.equal (Seq.meanHarmonic [42.0]) 42.0 "Expected 42.0 for one element sequence"

        testCase "Two element sequence" <| fun () ->
            Expect.floatClose Accuracy.high (Seq.meanHarmonic [3.0; 6.0]) 4.0 "Expected 4.0 for two element sequence"

        testCase "All same elements sequence" <| fun () ->
            Expect.floatClose Accuracy.high (Seq.meanHarmonic [2.5; 2.5; 2.5]) 2.5 "Expected 2.5 for all same elements sequence"

        testCase "All different elements sequence" <| fun () ->
            Expect.floatClose Accuracy.high (Seq.meanHarmonic [1.0; 2.0; 3.0; 4.0; 5.0]) 2.18978102189781 "Expected approximately 2.18978 for all different elements sequence"

        testCase "Sequence with NaN" <| fun () ->
            Expect.isTrue (Seq.meanHarmonic [1.0; 2.0; nan] |> System.Double.IsNaN) "Expected NaN for sequence with NaN"

        testCase "Sequence with Infinity" <| fun () ->
            Expect.equal (Seq.meanHarmonic [1.0; 2.0; infinity]) 2.0 "Expected 2.0 for sequence with Infinity"

        testCase "Sequence with -Infinity" <| fun () ->
            Expect.equal (Seq.meanHarmonic [1.0; 2.0; -infinity]) 2.0 "Expected 2.0 for sequence with -Infinity"

        testCase "Sequence with negative values" <| fun () ->
            Expect.floatClose Accuracy.high (Seq.meanHarmonic [-1.0; -2.0; -3.0]) -1.6363636363636365 "Expected approximately -1.63636 for sequence with negative values"

        testCase "Sequence with positive values" <| fun () ->
            Expect.floatClose Accuracy.high (Seq.meanHarmonic [1.0; 2.0; 3.0]) 1.6363636363636365 "Expected approximately 1.63636 for sequence with positive values"

        testCase "Sequence with mixed values" <| fun () ->
            Expect.floatClose Accuracy.high (Seq.meanHarmonic [-1.0; 2.0; -3.0; 4.0]) -6.857142857142857 "Expected approximately -6.85714 for sequence with mixed values"

        testCase "Sequence with Int32 values" <| fun () ->
            Expect.equal (Seq.meanHarmonic [1; 2; 3; 4; 5]) 5 "Expected 5 for sequence with Int32 values"

        testCase "Sequence with Int64 values" <| fun () ->
            Expect.equal (Seq.meanHarmonic [1L; 2L; 3L; 4L; 5L]) 5L "Expected 5L for sequence with Int64 values"
    ]


[<Tests>]
let seqGenTests = 
    testList "Seq.meanTruncated" [
        testCase "Empty sequence" <| fun () ->
            let xs = Seq.empty<float>
            let result = Seq.meanTruncated 0.1 xs
            Expect.isTrue (Double.IsNaN result) "Expected NaN for empty sequence"

        testCase "Single element" <| fun () ->
            let xs = seqGen 1
            Expect.floatClose Accuracy.high (Seq.meanTruncated 0.1 xs) (Seq.head xs) "Expected mean to equal single element"
            
        testCase "All same value" <| fun () ->
            let xs = Seq.replicate 100 5.0
            Expect.floatClose Accuracy.high 5.0 (Seq.meanTruncated 0.1 xs) "Expected 5.0 for all same value"
            
        testCase "Random floats length 10 trunc 0.1" <| fun () ->
            let xs = seqGen 10 
            Expect.floatClose Accuracy.high (Seq.meanTruncated 0.1 xs) -52.347631218073715331 "Expected mean of -52.347631218073715331"

        testCase "Random floats length 100 trunc 0.2" <| fun () ->
            let xs = seqGen 100
            Expect.floatClose Accuracy.high (Seq.meanTruncated 0.2 xs) 9.5124633561411808813 "Expected mean of 9.5124633561411808813"

        testCase "Random floats length 1000 trunc 0.05" <| fun () ->
            let xs = seqGen 1000
            Expect.floatClose Accuracy.high (Seq.meanTruncated 0.05 xs) -6.0286203235934587852 "Expected mean of -6.0286203235934587852"

        testCase "Sequence with NaN" <| fun () ->
            let xs = seq [1.0; 2.0; Double.NaN; 3.0; 4.0]
            let result = Seq.meanTruncated 0.1 xs
            Expect.isTrue (Double.IsNaN result) "Expected NaN when sequence contains NaN"
    ]


[<Tests>]
let varTests =
    testList "Seq.var" [
        testCase "varEmpty" <| fun () ->
            let variance = Seq.var Seq.empty
            Expect.isTrue (nan.Equals(variance)) "Variance of empty seq should be NaN"
            
        testCase "varSingleValue" <| fun () ->
            let variance = Seq.var [5.]
            Expect.isTrue (nan.Equals(variance)) "Variance of single value should be NaN"
            
        testCase "varSameValues" <| fun () ->
            let variance = Seq.var [2.;2.;2.;2.]
            Expect.floatClose Accuracy.high variance 0. "Variance of same values should be 0.0"
            
        testCase "varShortSeq" <| fun () ->
            let variance = Seq.var [1.;2.;3.;4.;5.]
            Expect.floatClose Accuracy.high variance 2.5 "Variance of short seq [1.;2.;3.;4.;5.] should be 2.5"
            
        testCase "varNaN" <| fun () ->
            let variance = Seq.var [1.;2.;3.;nan]
            Expect.isTrue (nan.Equals(variance)) "Variance of seq containing NaN should be NaN"
            
        testCase "varInfinity" <| fun () ->
            let variance = Seq.var [1.;2.;infinity]
            Expect.isTrue (nan.Equals(variance)) "Variance of seq containing infinity should be NaN"
            
        testCase "varNegInfinity" <| fun () ->
            let variance = Seq.var [1.;2.;-infinity]  
            Expect.isTrue (nan.Equals(variance)) "Variance of seq containing -infinity should be NaN"
            
        testCase "varSeqGen10" <| fun () ->
            let variance = Seq.var (seqGen 10)
            Expect.floatClose Accuracy.high variance 63886.22 "Variance of seqGen 10 should be around 63886.22"
            
        testCase "varSeqGen100" <| fun () ->
            let variance = Seq.var (seqGen 100)
            Expect.floatClose Accuracy.high variance 84091.74 "Variance of seqGen 100 should be around 84091.74"
            
        testCase "varSeqGen1000" <| fun () ->
            let variance = Seq.var (seqGen 1000)
            Expect.floatClose Accuracy.high variance 82020.82 "Variance of seqGen 1000 should be around 82020.82"
    ]



[<Tests>]
let varPopulationTests =
    testList "Seq.varPopulation" [
        testCase "varPopulationEmpty" <| fun () ->
            let variance = Seq.varPopulation Seq.empty
            Expect.isTrue (Double.IsNaN variance) "Variance of empty sequence should be NaN"
        
        testCase "varPopulationAllSame" <| fun () ->
            let variance = Seq.varPopulation (List.replicate 100 5.0)
            Expect.floatClose Accuracy.high variance 0.0 "Variance of sequence with all same values should be 0.0"
        
        testCase "varPopulationWithNaN" <| fun () ->
            let variance = Seq.varPopulation [1.0; 2.0; 3.0; nan; 4.0; 5.0]
            Expect.isTrue (Double.IsNaN variance) "Variance of sequence containing NaN should be NaN"
        
        testCase "varPopulationWithInfinity" <| fun () ->
            let variance = Seq.varPopulation [1.0; 2.0; 3.0; infinity; 4.0; 5.0]
            Expect.isTrue (Double.IsNaN variance) "Variance of sequence containing infinity should be NaN"
        
        testCase "varPopulationWithNegInfinity" <| fun () ->
            let variance = Seq.varPopulation [1.0; 2.0; 3.0; -infinity; 4.0; 5.0]
            Expect.isTrue (Double.IsNaN variance) "Variance of sequence containing negative infinity should be NaN"
        
        testCase "varPopulationSeq5" <| fun () ->
            let variance = Seq.varPopulation (seqGen 5)
            Expect.floatClose Accuracy.high variance 83883.29 "Variance of seqGen 5"
        
        testCase "varPopulationSeq10" <| fun () ->
            let variance = Seq.varPopulation (seqGen 10)
            Expect.floatClose Accuracy.high variance 57497.59 "Variance of seqGen 10"
        
        testCase "varPopulationSeq100" <| fun () ->
            let variance = Seq.varPopulation (seqGen 100)
            Expect.floatClose Accuracy.high variance 83250.82 "Variance of seqGen 100"
    ]

[<Tests>]
let stDevTests =
    testList "Seq.stDev" [
        testCase "stDevEmpty" <| fun () ->
            let stDev = Seq.stDev (Seq.empty:seq<float>)
            Expect.isTrue (Double.IsNaN(stDev)) "stDev of empty seq should be NaN"

        testCase "stDevSingleValue" <| fun () ->
            let stDev = Seq.stDev ([5.0]:seq<float>)
            Expect.isTrue (Double.IsNaN(stDev)) "stDev of single value should be NaN"
            
        testCase "stDevAllSameValue" <| fun () ->
            let stDev = Seq.stDev ([10.0; 10.0; 10.0; 10.0; 10.0]:seq<float>)  
            Expect.floatClose Accuracy.high stDev 0.0 "stDev of all same values should be 0.0"
            
        testCase "stDevShortSeq" <| fun () ->
            let stDev = Seq.stDev ([1.0; 2.0; 3.0; 4.0; 5.0]:seq<float>)
            Expect.floatClose Accuracy.high stDev 1.58113883 "stDev of [1.0; 2.0; 3.0; 4.0; 5.0] should be about 1.58113883"

        testCase "stDevWithNegatives" <| fun () ->
            let stDev = Seq.stDev ([1.0; -2.0; 3.0; -4.0; 5.0]:seq<float>) 
            Expect.floatClose Accuracy.high stDev 3.64691651 "stDev of [1.0; -2.0; 3.0; -4.0; 5.0] should be about 3.64691651"

        testCase "stDevLargeSeq" <| fun () ->
            let stDev = Seq.stDev (seqGen 1000)
            Expect.floatClose Accuracy.veryHigh stDev 286.39276524 "stDev of seqGen 1000 should be about 286.39276524"
    ]


[<Tests>]
let stDevPopulationTests =
    testList "Seq.stDevPopulation" [
        testCase "stDevPopulationEmpty" <| fun () ->
            let stdev = Seq.stDevPopulation Seq.empty<double>
            Expect.isTrue (Double.IsNaN(stdev)) "stdev of empty seq should be NaN"

        testCase "stDevPopulationAllSame" <| fun () ->
            let stdev = Seq.stDevPopulation (Seq.replicate 100 42.0)
            Expect.floatClose Accuracy.high stdev 0.0 "stdev of all same values should be 0.0"
            
        testCase "stDevPopulationSeqGen5" <| fun () ->
            let stdev = Seq.stDevPopulation (seqGen 5)
            Expect.floatClose Accuracy.medium stdev 289.62612676671483314 "stdev of seqGen 5 should be around 289.62612676671483314"
            
        testCase "stDevPopulationWithNaN" <| fun () ->
            let stdev = Seq.stDevPopulation [1.0; 2.0; 3.0; nan]
            Expect.isTrue (Double.IsNaN(stdev)) "stdev of seq with NaN should be NaN"
            
        testCase "stDevPopulationWithInfinity" <| fun () ->
            let stdev = Seq.stDevPopulation [1.0; 2.0; 3.0; infinity]
            Expect.isTrue (Double.IsNaN(stdev)) "stdev of seq with infinity should be NaN"
            
        testCase "stDevPopulationWithNegativeInfinity" <| fun () ->
            let stdev = Seq.stDevPopulation [1.0; 2.0; 3.0; -infinity]
            Expect.isTrue (Double.IsNaN(stdev)) "stdev of seq with negative infinity should be NaN"
    ]


[<Tests>]
let semTests =
    testList "Seq.sem" [
        testCase "semEmpty" <| fun () ->
            let sem = Seq.sem ([]:double list)
            Expect.isTrue (nan.Equals(sem)) "SEM of empty sequence should be NaN"
        testCase "semSingleValue" <| fun () ->
            let sem = Seq.sem [42.0]
            Expect.isTrue (nan.Equals(sem)) "SEM of single value should be NaN"
        testCase "semAllSameValue" <| fun () ->
            let sem = Seq.sem [42.0; 42.0; 42.0; 42.0; 42.0]
            Expect.floatClose Accuracy.high sem 0.0 "SEM of all same values should be 0.0"
        testCase "semShortSeq" <| fun () ->
            let sem = Seq.sem [1.0; 2.0; 3.0; 4.0; 5.0]
            Expect.floatClose Accuracy.high sem 0.70710678118654757274 "SEM of short sequence"
        testCase "semLongSeq" <| fun () ->
            let sem = Seq.sem (seqGen 1000)
            Expect.floatClose Accuracy.high sem 9.0565344355779000551 "SEM of long sequence"
        testCase "semNaN" <| fun () ->
            let sem = Seq.sem [1.0; 2.0; 3.0; nan; 5.0]
            Expect.isTrue (nan.Equals(sem)) "SEM of sequence with NaN should be NaN"
        testCase "semInfinity" <| fun () ->
            let sem = Seq.sem [1.0; 2.0; 3.0; infinity; 5.0]
            Expect.isTrue (nan.Equals(sem)) "SEM of sequence with infinity should be NaN"
        testCase "semNegInfinity" <| fun () ->
            let sem = Seq.sem [1.0; 2.0; 3.0; -infinity; 5.0]
            Expect.isTrue (nan.Equals(sem)) "SEM of sequence with negative infinity should be NaN"
    ]


[<Tests>]
let cvTests =
    testList "Seq.cv" [
        testCase "cvEmpty" <| fun () ->
            let cv = Seq.cv Seq.empty
            Expect.isTrue (Double.IsNaN cv) "CV of empty sequence should be NaN"
        
        testCase "cvAllSame" <| fun () ->
            let cv = Seq.cv (List.replicate 100 5.0)
            Expect.floatClose Accuracy.high cv 0.0 "CV of all same values should be 0.0"
        
        testCase "cvShortSeq" <| fun () ->
            let cv = Seq.cv [1.0; 2.0; 3.0; 4.0; 5.0]
            Expect.floatClose Accuracy.medium cv 0.52705 "CV of short sequence"
        
        testCase "cvLongSeq" <| fun () ->
            let cv = Seq.cv (seqGen 1000)
            Expect.floatClose Accuracy.medium cv -50.953708636964790912 "CV of long sequence"
        
        testCase "cvNaN" <| fun () ->
            let cv = Seq.cv [1.0; 2.0; 3.0; nan; 5.0]
            Expect.isTrue (Double.IsNaN cv) "CV of sequence with NaN should be NaN"
        
        testCase "cvInf" <| fun () ->
            let cv = Seq.cv [1.0; 2.0; 3.0; infinity; 5.0]
            Expect.isTrue (Double.IsNaN cv) "CV of sequence with Infinity should be NaN"
        
        testCase "cvNegInf" <| fun () ->
            let cv = Seq.cv [1.0; 2.0; 3.0; -infinity; 5.0]
            Expect.isTrue (Double.IsNaN cv) "CV of sequence with -Infinity should be NaN"
    ]


[<Tests>]
let cvPopulationTests =
    testList "Seq.cvPopulation" [
        testCase "cvPopulationTypical" <| fun () ->
            let cv = Seq.cvPopulation (seqGen 100)
            Expect.floatClose Accuracy.medium cv 116.8527 "CV should be approximately 116.8527"
        
        testCase "cvPopulationEmpty" <| fun () ->
            let cv = Seq.cvPopulation Seq.empty
            Expect.isTrue (Double.IsNaN cv) "CV of empty sequence should be NaN"
        
        testCase "cvPopulationSingleValue" <| fun () ->
            let cv = Seq.cvPopulation (Seq.replicate 10 5.0)
            Expect.floatClose Accuracy.high cv 0.0 "CV of sequence with all same values should be 0.0"
        
        testCase "cvPopulationWithNaN" <| fun () ->
            let cv = Seq.cvPopulation [1.0; 2.0; 3.0; nan]
            Expect.isTrue (Double.IsNaN cv) "CV of sequence containing NaN should be NaN"
        
        testCase "cvPopulationWithInfinity" <| fun () ->
            let cv = Seq.cvPopulation [1.0; 2.0; 3.0; infinity]
            Expect.isTrue (Double.IsNaN cv) "CV of sequence containing infinity should be NaN"
        
        testCase "cvPopulationWithNegativeInfinity" <| fun () ->
            let cv = Seq.cvPopulation [1.0; 2.0; 3.0; -infinity]
            Expect.isTrue (Double.IsNaN cv) "CV of sequence containing negative infinity should be NaN"
    ]



[<Tests>]
let covPopulationTests =
    testList "Seq.covPopulation" [
        testCase "covPopulationBasic" <| fun () ->
            let x = seqGen 5 |> Seq.take 5
            let y = seqGen 10 |> Seq.take 5
            let cov = Seq.covPopulation x y
            Expect.floatClose Accuracy.high cov 34997.222487256090972 "Covariance should be 34997.222487256090972"

        testCase "covPopulationEmpty" <| fun () ->
            let x = Seq.empty<float>
            let y = Seq.empty<float>
            let cov = Seq.covPopulation x y
            Expect.isTrue (Double.IsNaN(cov)) "Covariance of empty sequences should be NaN"

        testCase "covPopulationNaN" <| fun () ->
            let x = [1.0; 2.0; Double.NaN]
            let y = [4.0; 5.0; 6.0]
            let cov = Seq.covPopulation x y
            Expect.isTrue (Double.IsNaN(cov)) "Covariance should be NaN if any element is NaN"

        testCase "covPopulationSameValue" <| fun () ->
            let x = [2.5; 2.5; 2.5]
            let y = [8.0; 8.0; 8.0]
            let cov = Seq.covPopulation x y
            Expect.floatClose Accuracy.high cov 0.0 "Covariance of sequences with same values should be 0"

        testCase "covPopulationInfinity" <| fun () ->
            let x = [1.0; 2.0; Double.PositiveInfinity]
            let y = [4.0; 5.0; 6.0]
            let cov = Seq.covPopulation x y
            Expect.isTrue (Double.IsNaN(cov)) "Covariance should be NaN if any element is infinity"

        testCase "covPopulationNegativeInfinity" <| fun () ->
            let x = [1.0; 2.0; Double.NegativeInfinity]
            let y = [4.0; 5.0; 6.0]
            let cov = Seq.covPopulation x y
            Expect.isTrue (Double.IsNaN(cov)) "Covariance should be NaN if any element is negative infinity"

        testCase "covPopulationDifferentLengths" <| fun () ->
            let x = [1.0; 2.0; 3.0]
            let y = [4.0; 5.0]
            Expect.throws (fun () -> Seq.covPopulation x y |> ignore) "Sequences of different lengths should throw an exception"
    ]




[<Tests>]
let covPopulationOfPairsTests =
    testList "Seq.covPopulationOfPairs" [
        testCase "covPopulationOfPairsEmpty" <| fun () ->
            let cov = Seq.covPopulationOfPairs Seq.empty
            Expect.isTrue (Double.IsNaN(cov)) "Covariance of empty sequence should be NaN"
        
        testCase "covPopulationOfPairsNaN" <| fun () ->
            let cov = Seq.covPopulationOfPairs [(1.0, 2.0); (2.0, nan); (3.0, 6.0)]
            Expect.isTrue (Double.IsNaN(cov)) "Covariance of sequence with NaN should be NaN"
        
        testCase "covPopulationOfPairsAllSame" <| fun () ->
            let cov = Seq.covPopulationOfPairs (seqGen 100 |> Seq.map (fun x -> (x, x)))
            Expect.floatClose Accuracy.high cov 83250.82204 "Covariance of sequence with all same values should be close to 83250.82204"
        
        testCase "covPopulationOfPairsInfinity" <| fun () ->
            let cov = Seq.covPopulationOfPairs [(1.0, 2.0); (infinity, 4.0); (3.0, 6.0)]
            Expect.isTrue (Double.IsNaN(cov)) "Covariance of sequence with infinity should be NaN"

        testCase "covPopulationOfPairsNegInfinity" <| fun () ->
            let cov = Seq.covPopulationOfPairs [(1.0, 2.0); (-infinity, 4.0); (3.0, 6.0)]
            Expect.isTrue (Double.IsNaN(cov)) "Covariance of sequence with negative infinity should be NaN"

        testCase "covPopulationOfPairsLargeSeq" <| fun () ->
            let cov = Seq.covPopulationOfPairs (seqGen 100000 |> Seq.map (fun x -> (x, x+1.0)))
            Expect.floatClose Accuracy.high cov 83366.21512 "Covariance of large sequence should be close to 83366.21512"
        
        testCase "covPopulationOfPairsSeq" <| fun () ->
            let s1 = seqGen 100
            let s2 = seqGen 200 |> Seq.take 100
            let cov = Seq.covPopulationOfPairs (Seq.zip s1 s2)
            Expect.floatClose Accuracy.high cov 40559.822281678054424 "Covariance of large sequence should be close to 40559.822281678054424"
    ]



[<Tests>]
let covTests =
    testList "Seq.cov" [
        testCase "covPositiveCorrelation" <| fun () ->
            let x = seqGen 100 |> Seq.take 10
            let y = x |> Seq.map (fun x -> 2.0 * x)
            let cov = Seq.cov x y
            Expect.floatClose Accuracy.high cov 229809.2 "Covariance should be around 229809.2"
        
        testCase "covNegativeCorrelation" <| fun () ->
            let x = seqGen 100 |> Seq.take 10
            let y = x |> Seq.map (fun x -> -2.0 * x)
            let cov = Seq.cov x y
            Expect.floatClose Accuracy.high cov -229809.2 "Covariance should be around -229809.2"
        
        testCase "covEmpty" <| fun () ->
            let x = Seq.empty<float>
            let y = Seq.empty<float>
            let cov = Seq.cov x y
            Expect.isTrue (nan.Equals(cov)) "Covariance of empty sequences should be NaN"

        testCase "covNaN" <| fun () ->
            let x = [1.0; 2.0; 3.0; nan]
            let y = [2.0; 4.0; 6.0; 8.0]
            let cov = Seq.cov x y
            Expect.isTrue (nan.Equals(cov)) "Covariance should be NaN if any element is NaN"

        testCase "covInfinity" <| fun () ->
            let x = [1.0; 2.0; 3.0; infinity]
            let y = [2.0; 4.0; 6.0; 8.0]
            let cov = Seq.cov x y
            Expect.isTrue (nan.Equals(cov)) "Covariance should be NaN if any element is infinity"

        testCase "covNegInfinity" <| fun () ->
            let x = [1.0; 2.0; 3.0; -infinity]
            let y = [2.0; 4.0; 6.0; 8.0]
            let cov = Seq.cov x y
            Expect.isTrue (nan.Equals(cov)) "Covariance should be NaN if any element is -infinity"
    ]



[<Tests>]
let covOfPairsTests =
    testList "Seq.covOfPairs" [
        testCase "covOfPairsEmpty" <| fun () ->
            let cov = Seq.covOfPairs Seq.empty
            Expect.isTrue (nan.Equals(cov)) "Covariance of empty sequence should be NaN"
            
        testCase "covOfPairsNaN" <| fun () ->
            let cov = Seq.covOfPairs [(1.0,1.0); (2.0,nan); (3.0,3.0)]
            Expect.isTrue (nan.Equals(cov)) "Covariance of sequence containing NaN should be NaN"
            
        testCase "covOfPairsAllSame" <| fun () ->
            let cov = Seq.covOfPairs (Seq.init 100 (fun _ -> (5.0, 5.0)))
            Expect.equal cov 0.0 "Covariance of sequence with all same values should be 0.0"
            
        testCase "covOfPairsSeqGen" <| fun () ->
            let cov = Seq.covOfPairs (Seq.zip (seqGen 100) (seqGen 100))
            Expect.floatClose Accuracy.high cov 84091.74 "Covariance of seqGen 100 with itself"
            
        testCase "covOfPairsSeqGenOffset" <| fun () ->
            let cov = Seq.covOfPairs (Seq.zip (seqGen 100) (seqGen 100 |> Seq.skip 50))
            Expect.floatClose Accuracy.medium cov 5709.76 "Covariance of offset seqGen sequences"
            
        testCase "covOfPairsInfinity" <| fun () ->
            let cov = Seq.covOfPairs [(1.0,1.0); (2.0,infinity); (3.0,Double.NegativeInfinity)]
            Expect.isTrue (nan.Equals(cov)) "Covariance of sequence with infinities should be NaN"
    ]


[<Tests>]
let medianAbsoluteDevTests =
    testList "Seq.medianAbsoluteDev" [
        testCase "emptySeq" <| fun () ->
            let mad = Seq.medianAbsoluteDev Seq.empty
            Expect.isTrue (Double.IsNaN mad) "MAD of empty sequence should be NaN"
            
        testCase "singleValue" <| fun () ->
            let mad = Seq.medianAbsoluteDev [42.0]
            Expect.equal mad 0.0 "MAD of single value should be 0.0"
            
        testCase "allSameValue" <| fun () ->
            let mad = Seq.medianAbsoluteDev (List.replicate 100 42.0)
            Expect.equal mad 0.0 "MAD of all same values should be 0.0"
            
        testCase "seqWithNaN" <| fun () ->
            let mad = Seq.medianAbsoluteDev (seqGen 100 |> Seq.map (fun x -> if x < 0.5 then nan else x))
            Expect.isTrue (Double.IsNaN mad) "MAD of sequence containing NaN should be NaN"
            
        testCase "seqWithInfinity" <| fun () ->
            let mad = Seq.medianAbsoluteDev (seqGen 100 |> Seq.map (fun x -> if x < 0.5 then infinity else x))
            Expect.floatClose Accuracy.high mad 424.181 "MAD of sequence containing infinity"
            
        testCase "seqWithNegInfinity" <| fun () ->
            let mad = Seq.medianAbsoluteDev (seqGen 100 |> Seq.map (fun x -> if x < 0.5 then -infinity else x))
            Expect.floatClose Accuracy.high mad 424.181 "MAD of sequence containing negative infinity"
            
        testCase "seqWithPosAndNeg" <| fun () ->
            let mad = Seq.medianAbsoluteDev (seqGen 100 |> Seq.map (fun x -> if x < 0.5 then -x else x))
            Expect.floatClose Accuracy.medium mad 125.358 "MAD of sequence with pos and neg values"
            
        testCase "largeSeq" <| fun () ->
            let mad = Seq.medianAbsoluteDev (seqGen 10000)
            Expect.floatClose Accuracy.medium mad 246.563 "MAD of large sequence"
    ]



[<Tests>]
let statsTests =
    testList "Seq.stats" [
        testCase "statsEmpty" <| fun () ->
            let stats = Seq.stats (Seq.empty<float>)
            Expect.equal stats.N 0 "N should be 0"
            Expect.isTrue (Double.IsNaN stats.Mean) "Mean should be NaN"
            Expect.isTrue (Double.IsNaN stats.SumOfSquares) "SumOfSquares should be NaN"
            Expect.isTrue (Double.IsNaN stats.Min) "Min should be NaN"
            Expect.isTrue (Double.IsNaN stats.Max) "Max should be NaN"

        testCase "statsSeqGen10" <| fun () ->
            let stats = seqGen 10 |> Seq.stats
            Expect.equal stats.N 10 "N should be 9"
            Expect.floatClose Accuracy.high stats.Mean -13.979665708718687 "Mean should be -13.979665708718687"
            Expect.floatClose Accuracy.high stats.SumOfSquares 362450.2113702808 "SumOfSquares should be 362450.2113702808"
            Expect.floatClose Accuracy.high stats.Min -499.92173630740575163 "Min should be -499.92173630740575163"
            Expect.floatClose Accuracy.high stats.Max 292.9640583661216624 "Max should be 10.644420177367894"

        testCase "statsSeqGen1000" <| fun () ->
            let stats = seqGen 1000 |> Seq.stats
            Expect.equal stats.N 1000 "N should be 999"
            Expect.floatClose Accuracy.medium stats.Mean -5.133606105015737 "Mean should be -5.133606105015737"
            Expect.floatClose Accuracy.medium stats.SumOfSquares 81701824.38921407 "SumOfSquares should be 81701824.38921407"
            Expect.floatClose Accuracy.medium stats.Min -498.70270583718212265 "Min should be -498.70270583718212265"
            Expect.floatClose Accuracy.medium stats.Max 499.80056798076293489 "Max should be 10.644420177367894"

        testCase "statsAllSame" <| fun () ->
            let stats = Seq.init 100 (fun _ -> 42.0) |> Seq.stats
            Expect.equal stats.N 100 "N should be 100"
            Expect.equal stats.Mean 42.0 "Mean should be 42.0"
            Expect.equal stats.SumOfSquares 0.0 "SumOfSquares should be 0.0"
            Expect.equal stats.Min 42.0 "Min should be 42.0"
            Expect.equal stats.Max 42.0 "Max should be 42.0"

        testCase "statsNaN" <| fun () ->
            let stats = Seq.stats [1.0; 2.0; Double.NaN; 3.0]
            Expect.equal stats.N 4 "N should be 4"
            Expect.isTrue (Double.IsNaN stats.Mean) "Mean should be NaN"
            Expect.isTrue (Double.IsNaN stats.SumOfSquares) "SumOfSquares should be NaN"

        testCase "statsInfinity" <| fun () ->
            let stats = Seq.stats [1.0; Double.PositiveInfinity; 2.0; Double.NegativeInfinity]
            Expect.equal stats.N 4 "N should be 4"
            Expect.isTrue (Double.IsNaN stats.Mean) "Mean should be NaN"
            Expect.isTrue (Double.IsNaN stats.SumOfSquares) "SumOfSquares should be NaN"
            Expect.equal stats.Min Double.NegativeInfinity "Min should be negative infinity"
            Expect.equal stats.Max Double.PositiveInfinity "Max should be positive infinity"
    ]


[<Tests>]
let getMeanOfReplicatesTests =
    testList "Seq.getMeanOfReplicates" [
        testCase "emptySeq" <| fun () ->
            let means = Seq.getMeanOfReplicates 2 Seq.empty<float>
            Expect.isEmpty means "Means of empty seq should be empty"
        
        testCase "singleValue" <| fun () ->
            let means = Seq.getMeanOfReplicates 2 (Seq.replicate 6 42.0)
            Expect.sequenceEqual means [42.0; 42.0; 42.0] "Means should all be 42.0"
        
        testCase "seqWithNaN" <| fun () ->
            let values = [1.0; 2.0; nan; 4.0; 5.0; nan]
            let means = Seq.getMeanOfReplicates 2 values |> Seq.toList
            Expect.floatClose Accuracy.high means.[0] 1.5 "First mean should be 1.5"
            Expect.isTrue (means.[1] |> System.Double.IsNaN) "Second mean should be NaN"
            Expect.isTrue (means.[2] |> System.Double.IsNaN) "Third mean should be NaN"
        
        testCase "seqWithInfinity" <| fun () ->
            let values = [1.0; 2.0; infinity; 4.0; -infinity; 6.0]
            let means = Seq.getMeanOfReplicates 2 values |> Seq.toList
            Expect.floatClose Accuracy.high means.[0] 1.5 "First mean should be 1.5"
            Expect.isTrue (means.[1] |> System.Double.IsInfinity) "Second mean should be Infinity"
            Expect.isTrue (means.[2] |> System.Double.IsNegativeInfinity) "Third mean should be -Infinity"
        
        testCase "generatedSeq" <| fun () ->
            let values = seqGen 100 |> Seq.truncate 60
            let means = Seq.getMeanOfReplicates 3 values |> Seq.toList
            Expect.equal means.Length 20 "Should have 20 means"
            Expect.floatClose Accuracy.high means.[0] -261.63544 "First mean"
            Expect.floatClose Accuracy.high means.[9] -129.70288 "10th mean"
            Expect.floatClose Accuracy.high means.[19] 80.95719 "Last mean"
    ]



[<Tests>]
let getStDevOfReplicatesTests =
    testList "Seq.getStDevOfReplicates" [
        testCase "emptySeq" <| fun () ->
            let stdevs = Seq.getStDevOfReplicates 2 (Seq.empty : float seq)
            Expect.isEmpty stdevs "Empty sequence should return empty"
            
        testCase "nanSeq" <| fun () ->
            let data = Seq.init 10 (fun _ -> nan)
            let stdevs = Seq.getStDevOfReplicates 2 data
            stdevs |> Seq.iter (fun sd -> Expect.isTrue (nan.Equals(sd)) "Stdev should be NaN")
            
        testCase "allSameValue" <| fun () ->
            let data = Seq.init 10 (fun _ -> 42.0)
            let stdevs = Seq.getStDevOfReplicates 2 data
            stdevs |> Seq.iter (fun sd -> Expect.floatClose Accuracy.high sd 0.0 "Stdev should be 0.0")
            
        testCase "seqWithInfinity" <| fun () ->
            let data = seqGen 10 |> Seq.map (fun i -> if int i % 2 = 0 then infinity else 1.0)
            let stdevs = Seq.getStDevOfReplicates 2 data
            stdevs |> Seq.iter (fun sd -> Expect.isTrue (nan.Equals(sd)) "Stdev should be NaN")
            
        testCase "seqWithNegInfinity" <| fun () ->
            let data = seqGen 10 |> Seq.map (fun i -> if int i % 2 = 0 then -infinity else 1.0)
            let stdevs = Seq.getStDevOfReplicates 2 data
            stdevs |> Seq.iter (fun sd -> Expect.isTrue (nan.Equals(sd)) "Stdev should be NaN")
            
        testCase "seqLengthNotMultipleOfRep" <| fun () ->
            let data = seqGen 10
            Expect.throws (fun () -> Seq.getStDevOfReplicates 3 data |> ignore) "Should throw for length not multiple of rep"
            
        testCase "typicalValues" <| fun () ->
            let data = [1.0; 2.0; 3.0; 4.0; 5.0; 6.0]
            let stdevs = Seq.getStDevOfReplicates 2 data
            let expected = [0.7071067811865476; 0.7071067811865476; 0.7071067811865476]
            Expect.sequenceEqual stdevs expected "Stdevs should match expected"
            
        testCase "largeSequence" <| fun () ->
            let data = seqGen 10000
            let stdevs = Seq.getStDevOfReplicates 100 data
            Expect.equal (Seq.length stdevs) 100 "Should have 100 stdev values"
    ]



[<Tests>]
let cvOrReplicatesTests =
    testList "Seq.getCvOfReplicates" [
        testCase "emptySeq" <| fun () ->
            let cvs = Seq.getCvOfReplicates 2 Seq.empty
            Expect.isEmpty cvs "CV of empty seq should be empty"

        testCase "nanSeq" <| fun () ->
            let data = [nan; nan; nan; nan]
            let cvs = Seq.getCvOfReplicates 2 data |> Seq.take 2 |> List.ofSeq
            Expect.isTrue (cvs |> List.forall (fun cv -> Double.IsNaN cv)) "All CVs should be NaN"
            
        testCase "sameValueSeq" <| fun () -> 
            let data = [42.0; 42.0; 42.0; 42.0; 42.0; 42.0]
            let cvs = Seq.getCvOfReplicates 3 data |> List.ofSeq
            Expect.equal cvs [0.0; 0.0] "CVs of same values should be 0.0"

        testCase "randomSeq" <| fun () ->
            let data = seqGen 100
            let cvs = Seq.getCvOfReplicates 5 data |> Seq.take 20 |> List.ofSeq
            Expect.isTrue (cvs.Length = 20) "Should return 20 CVs"
            Expect.floatClose Accuracy.high cvs.[0] -2.6735807467216741173 "CV at index 0"
            Expect.floatClose Accuracy.high cvs.[4] 3.3738276249721761424 "CV at index 4"
            Expect.floatClose Accuracy.high cvs.[11] 3.0084709580126212103 "CV at index 11"

    ]