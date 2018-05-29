(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/"
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"
#r "netstandard.dll"
open FSharp.Plotly
(**
Basic stats
=========================

**)
#r "FSharp.Stats.dll"
open FSharp.Stats

(**
Read iris data set via Csv-reader
----------------------------------
*)
open System.IO

let irisData =
    File.ReadLines (__SOURCE_DIRECTORY__  + "./data/irisData.csv")
    |> Seq.skip 1
    |> Seq.map (fun line -> line.Split([|','|]) )
    |> Seq.toList
let irisFeatures = 
    irisData 
    |> List.map (fun ii -> ii.[0..3] |> Array.map float |> Array.toList)    

let irisLables = irisData |> List.map (fun ii -> ii.[4])
let irisFeaturesMatrix = Matrix.ofColList irisFeatures



(**
BioFSharp ML module 
--------------------
Principal component analysis of the iris data set and visual inspection of the result.
*)
open FSharp.Stats.ML
open FSharp.Stats.ML.Unsupervised


let adjCenter = PCA.toAdjustCenter irisFeaturesMatrix
let irisPCA = PCA.compute adjCenter irisFeaturesMatrix
let irisDataPCA = PCA.transform adjCenter irisPCA irisFeaturesMatrix
let irisrev = PCA.revert adjCenter irisPCA irisDataPCA



