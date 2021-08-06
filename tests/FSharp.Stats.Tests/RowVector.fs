module RowVectorTests 
open Expecto

open FSharp.Stats
open FSharp.Stats.RowVector

let private testRowVectorA =
    let values = [|5.;12.;18.;-23.;45.|]
    RowVector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testArrayA =
    [|5.;12.;18.;-23.;45.|]
    
[<Tests>]
let floatImplementationTests =
    let testRowVectorASquare = rowvec [25.0; 144.0; 324.0; 529.0; 2025.]

    testList "RowVector" [
        testCase "map" <| fun () -> 
            let actual = testRowVectorA |> RowVector.map (fun x -> x**2.)
            Expect.equal actual testRowVectorASquare "RowVectors should be equal"

        testCase "init" <| fun () -> 
            let actual = RowVector.init testArrayA.Length (fun i -> testArrayA.[i])
            Expect.equal actual testRowVectorA "RowVectors should be equal"
    ]


