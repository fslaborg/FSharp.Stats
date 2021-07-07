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
//tested in comparison with
//https://www.itl.nist.gov/div898/handbook/pmc/section5/pmc541.htm

[<Tests>]
let CovariancematrixTest =
    let x = [|[|4.0;4.2;3.9;4.3;4.1|];
              [|2.0;2.1;2.0;2.1;2.2|];
              [|0.60;0.59;0.58;0.62;0.63|]|] |> matrix

    let actual = Matrix.columnSampleCovarianceMatrixOf(x)
    let expected = [|[|0.025;0.0075;0.00175|];
                    [|0.0075;0.00701;0.00135|];
                    [|0.00175;0.00135;0.00043|]|] |> matrix

    Expect.equal actual expected
            
        
