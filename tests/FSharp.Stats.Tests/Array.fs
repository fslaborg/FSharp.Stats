module ArrayTests

open Expecto
open System
open FSharp.Stats
open TestExtensions

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

[<Tests>]
let dropNanTests =
    testList "Array" [
        testCase "dropNaN" <| fun () ->
            let testArray = [|-infinity; 0.5; 1.5; 1000.; nan; 5.0; nan|]
            let expected = [|-infinity; 0.5; 1.5; 1000.; 5.0|]
            let actual = Array.dropNaN testArray
            Expect.equal expected actual "Filtered array is incorrect"
    ]

   
[<Tests>]
let linspaceTests =
    testList "Array" [

        testCase "linspace_1" <| fun () ->
            let expected = Array.linspace(start= -3.5,stop= 30.1,num=7)
            let actual = [|-3.5; 2.1; 7.7; 13.3; 18.9; 24.5; 30.1|]
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "linspace results in wrong array"
        
        testCase "linspace_2" <| fun () ->
            let expected = Array.linspace(start= -3.5,stop= 2.9,num=17)
            let actual = [|-3.5; -3.1; -2.7; -2.3; -1.9; -1.5; -1.1; -0.7; -0.3;  0.1;  0.5; 0.9;  1.3;  1.7;  2.1;  2.5;  2.9|]
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "linspace results in wrong array"
        
        testCase "linspace_3" <| fun () ->
            let expected = Array.linspace(start= -3.5,stop= 30.1,num=6,IncludeEndpoint=false)
            let actual = [|-3.5;  2.1;  7.7; 13.3; 18.9; 24.5|]
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "linspace results in wrong array"
    ]


[<Tests>]
let varianceTests =
    testList "Array" [
        testCase "variance float" <| fun () ->
            let testArray = [|1.; 2.; 3.; 4.; 5.|]
            let expected = 2.5
            let actual = Array.varOf (Array.average testArray) testArray
            Expect.floatClose Accuracy.high actual expected "Variance is incorrect"
        testCase "variance float32" <| fun () ->
            let testArray = [|1.f; 2.f; 3.f; 4.f; 5.f|]
            let expected = 2.5f
            let actual : float32 = Array.varOf (Array.average testArray) testArray
            Expect.floatClose Accuracy.high (float actual) (float expected) "Variance is incorrect"
    ]
