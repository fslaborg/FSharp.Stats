module CovarianceTests
open Expecto

open FSharp.Stats



[<Tests>]
let sequenceTests =
    let x = [5.;12.;18.;-23.;45.]
    let y = [2.;8.;18.;-20.;28.]

    let xd = [5m;12m;18m;-23m;45m] 
    let yd = [2m;8m;18m;-20m;28m] 

    testList "Seq" [
        testCase "cov of floats" <| fun () ->
            let cov = Seq.cov x y
            Expect.floatClose Accuracy.high cov 434.90 "Should be equal (double precision)"
        testCase "covPopulation of floats" <| fun () ->
            let covPop = Seq.covPopulation x y
            Expect.floatClose Accuracy.high covPop 347.92 "Should be equal (double precision)"
        testCase "cov of decimals" <| fun () ->
            let cov = Seq.cov xd yd
            Expect.equal cov 434.90m "Should be equal (decimal)"
        testCase "covPopulation of decimals" <| fun () ->
            let covPop = Seq.covPopulation xd yd
            Expect.equal covPop 347.92m "Should be equal (decimal)"

    ]

[<Tests>]
let listTests =
    let x = [5.;12.;18.;-23.;45.]
    let y = [2.;8.;18.;-20.;28.]
    testList "List" [
        testCase "cov" <| fun () ->
            let cov = List.cov x y
            Expect.floatClose Accuracy.high cov 434.90 "Should be equal (double precision)"
        testCase "covPopulation" <| fun () ->
            let covPop = List.covPopulation x y
            Expect.floatClose Accuracy.high covPop 347.92 "Should be equal (double precision)"
    ]

[<Tests>]
let arrayTests =
    let x = [| 5.;12.;18.;-23.;45. |]
    let y = [| 2.;8.;18.;-20.;28. |]
    testList "Array" [
        testCase "cov" <| fun () ->
            let cov = Array.cov x y
            Expect.floatClose Accuracy.high cov 434.90 "Should be equal (double precision)"
        testCase "covPopulation" <| fun () ->
            let covPop = Array.covPopulation x y
            Expect.floatClose Accuracy.high covPop 347.92 "Should be equal (double precision)"
    ]