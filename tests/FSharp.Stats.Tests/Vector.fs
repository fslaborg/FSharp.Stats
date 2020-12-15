module VectorTests 
open Expecto

open FSharp.Stats
open FSharp.Stats.Vector

let private testVectorA =
    let values = [|0.;3.;6.|]
    Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

[<Tests>]
let covarianceTests =
    let x = vector [5.;12.;18.;-23.;45.]
    let y = vector [2.;8.;18.;-20.;28.]

    testList "Vector" [
        testCase "cov" <| fun () ->
            let cov = Vector.cov x y
            Expect.floatClose Accuracy.high cov 434.90 "Should be equal (double precision)"
        testCase "covPopulation" <| fun () ->
            let covPop = Vector.covPopulation x y
            Expect.floatClose Accuracy.high covPop 347.92 "Should be equal (double precision)"
    ]
