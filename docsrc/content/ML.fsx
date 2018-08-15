(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/netstandard2.0"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/Fsharp.Plotly.dll"
#r "netstandard"
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
let irisFeaturesMatrix = Matrix.ofList irisFeatures


(**
BioFSharp ML module 
--------------------
Principal component analysis of the iris data set and visual inspection of the result.
*)
open FSharp.Stats.ML
open FSharp.Stats.ML.Unsupervised


let adjCenter = PCA.toAdjustCorrelation irisFeaturesMatrix
let irisPCA = PCA.compute adjCenter irisFeaturesMatrix
let irisDataPCA = PCA.transform adjCenter irisPCA irisFeaturesMatrix
let irisrev = PCA.revert adjCenter irisPCA irisDataPCA


// Plot loadings colored grouped by grouping function
let plotLoadingsColoredByGrouping (pcaComponents : PCA.Component []) 
    (labels : seq<string>) (grouping : string -> string) pcIndex1 pcIndex2 = 
    let pComponent1 = pcaComponents.[pcIndex1 - 1]
    let pComponent2 = pcaComponents.[pcIndex2 - 1]
    (Seq.zip3 (pComponent1.EigenVector) (pComponent2.EigenVector) labels)
    |> Seq.groupBy (fun (x, y, label) -> grouping label)
    |> Seq.map 
           (fun (key, values) -> 
           let nVal = values |> Seq.map (fun (x, y, l) -> (x, y))
           let nLab = values |> Seq.map (fun (x, y, l) -> l)
           Chart.Point(nVal, Name = key, Labels = nLab) 
           |> Chart.withMarkerStyle (Size = 15))
    |> Chart.Combine
    |> Chart.withTitle
           (sprintf "PC %i (%.2f) versus PC %i (%.2f)" pComponent1.Index 
                (pComponent1.Proportion * 100.) pComponent2.Index 
                (pComponent2.Proportion * 100.))


// Plot loadings colored grouped by grouping function
let plotScoresColoredByGrouping (transformedData : Matrix<float>) 
    (labels : seq<string>) (grouping : string -> string) pcIndex1 pcIndex2 = 
    (Seq.zip3 (transformedData.Column(pcIndex1 - 1)) 
         (transformedData.Column(pcIndex2 - 1)) labels)
    |> Seq.groupBy (fun (x, y, label) -> grouping label)
    |> Seq.map 
           (fun (key, values) -> 
               let nVal = values |> Seq.map (fun (x, y, l) -> (x, y))
               let nLab = values |> Seq.map (fun (x, y, l) -> l)
               Chart.Point(nVal, Name = key, Labels = nLab)
           )
    //       |> Chart.withMarkerStyle(Size = 15))
    |> Chart.Combine    

plotLoadingsColoredByGrouping irisPCA 
    [ "Sepal length"; "Sepal width"; "Petal length"; "Petal width" ] 
    (fun x -> x) 1 2
|> Chart.Show


plotScoresColoredByGrouping irisDataPCA irisLables (fun x -> x) 1 3
|> Chart.Show   



// ---------------
// Kmeans clustering
// For random cluster inititalization use randomInitFactory:
let rng = new System.Random()
let randomInitFactory : IterativeClustering.CentroidsFactory<float []> = 
    IterativeClustering.randomCentroids<float []> rng
let cvmaxFactory : IterativeClustering.CentroidsFactory<float []> = 
    IterativeClustering.intitCVMAX
let kmeansResult = 
    IterativeClustering.kmeans <| DistanceMetrics.euclidean <| cvmaxFactory 
    <| (Matrix.toJaggedArrayRowWise irisFeaturesMatrix) <| 3

let chartsOfClassifiedData = 
    Matrix.toJaggedArrayRowWise irisFeaturesMatrix
    |> Seq.groupBy (fun dataPoint -> fst (kmeansResult.Classifier dataPoint))
    |> Seq.sortBy fst
    |> Seq.map (fun (key, values) -> 
           values
           |> Seq.map 
                  (fun v -> 
                  Chart.Line v 
                  |> Chart.WithStyling
                         (Color = System.Drawing.Color.Silver, BorderWidth = 1))
           |> Chart.Combine
           |> Chart.WithTitle(key.ToString()))
    |> Chart.Rows
    |> Chart.ShowChart
