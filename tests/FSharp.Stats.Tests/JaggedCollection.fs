module JaggedCollectionTests

open Expecto
open System
open FSharp.Stats


let a = 
    [|
        [| 1.; 2.; 3;|]
        [| 4.; 5.; 6;|]
        [| 7.; 8.; 9;|]
        [|10.;11.;12;|]
    |]

let aL = 
    [
        [ 1.; 2.; 3;]
        [ 4.; 5.; 6;]
        [ 7.; 8.; 9;]
        [10.;11.;12;]
    ]

let aT = 
    [|
        [| 1.; 4.; 7.; 10.|]
        [| 2.; 5.; 8.; 11.|]
        [| 3.; 6.; 9.; 12.|]
    |]

let aFirstLong = 
    [|
        [|1.;2.;3.|]
        [|1.;2.   |]
        [|1.;2.   |]
        [|1.;2.   |]
    |]

let aFirstShort = 
    [|
        [|1.;2.;3.;4.   |]
        [|1.;2.;3.;4.;5.;6.|]
        [|1.;2.;3.;4.;5.;6.|]
        [|1.;2.;3.;4.;5.;6.|]
    |]

[<Tests>]
let arrayTests =
    testList "JaggedArray" [
        testCase "transpose" <| fun () ->
            let a_transposed = JaggedArray.transpose a
            Expect.equal a_transposed aT "transpose didn't work"
            
            let aFirstLong_transpose() = JaggedArray.transpose aFirstLong
            Expect.throws (fun _ -> aFirstLong_transpose () |> ignore ) "All rows must be of equal length!"
            
            let aFirstShort_transpose() = JaggedArray.transpose aFirstShort
            Expect.throws (fun _ -> aFirstShort_transpose () |> ignore ) "All rows must be of equal length!"
            
            //The unchecked version works even if the inner collections are of varying length and the first collection is the longest
            let aFirstShort_transposeUnchecked = JaggedArray.transpose_ aFirstShort
            Expect.equal aFirstShort_transposeUnchecked [|[|1.;1.;1.;1.|];[|2.;2.;2.;2.|];[|3.;3.;3.;3.|];[|4.;4.;4.;4.|]|] "Unchecked transpose gives unintuitive results"
            
            //The unchecked version does not work if the inner collections are of varying length and the first collection isn't the longest
            let aFirstLong_transposeUnchecked() = JaggedArray.transpose_ aFirstLong
            Expect.throws (fun _ -> aFirstLong_transposeUnchecked () |> ignore ) "All rows must be of equal length!"
    ]


[<Tests>]
let listTests =
    testList "JaggedList" [
        testCase "ofJaggedArray" <| fun () ->
            let a' = JaggedList.ofJaggedArray a
            Expect.equal aL a' "JaggedList.ofJaggedArray didn't work"

        testCase "transpose" <| fun () ->
            let a' = JaggedList.ofJaggedArray a
            let aT' = JaggedList.ofJaggedArray aT
            let aFirstLong' = JaggedList.ofJaggedArray aFirstLong
            let aFirstShort' = JaggedList.ofJaggedArray aFirstShort

            let a_transposed = JaggedList.transpose a'
            Expect.equal a_transposed aT' "transpose didn't work"
            
            let aFirstLong_transpose() = JaggedList.transpose aFirstLong'
            Expect.throws (fun _ -> aFirstLong_transpose () |> ignore ) "All rows must be of equal length!"
            
            let aFirstShort_transpose() = JaggedList.transpose aFirstShort'
            Expect.throws (fun _ -> aFirstShort_transpose () |> ignore ) "All rows must be of equal length!"
            
            //The unchecked version works even if the inner collections are of varying length and the first collection is the longest
            let aFirstShort_transposeUnchecked = JaggedList.transpose_ aFirstShort'
            Expect.equal aFirstShort_transposeUnchecked [[1.;1.;1.;1.];[2.;2.;2.;2.];[3.;3.;3.;3.];[4.;4.;4.;4.]] "Unchecked transpose gives unintuitive results"
            
            //The unchecked version does not work if the inner collections are of varying length and the first collection isn't the longest
            let aFirstLong_transposeUnchecked() = JaggedList.transpose_ aFirstLong'
            Expect.throws (fun _ -> aFirstLong_transposeUnchecked () |> ignore ) "All rows must be of equal length!"
    ]
