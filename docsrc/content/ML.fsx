(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/netstandard2.0"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/Fsharp.Plotly.dll"
#r "netstandard"
open FSharp.Plotly
open FSharp.Plotly.Axis
open FSharp.Plotly.StyleParam
let myAxis title = LinearAxis.init(Title=title,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)


(**
ML
=========================

*)
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
let irisFeaturesMatrix = Matrix.ofJaggedList irisFeatures


// https://intoli.com/blog/pca-and-svd/
//https://medium.com/@jonathan_hui/machine-learning-singular-value-decomposition-svd-principal-component-analysis-pca-1d45e885e491

//# we now perform singular value decomposition of X
//# "economy size" (or "thin") SVD
//U, s, Vt = la.svd(X, full_matrices=False)
//V = Vt.T
//S = np.diag(s)

//# 1) then columns of V are principal directions/axes.
//assert np.allclose(*flip_signs(V, principal_axes))

//# 2) columns of US are principal components
//assert np.allclose(*flip_signs(U.dot(S), principal_components))

//# 3) singular values are related to the eigenvalues of covariance matrix
//assert np.allclose((s ** 2) / (n - 1), l)


let meanColumnWise (a:matrix) = 
    a
    |> Matrix.sumColumns
    |> Vector.map (fun sum -> sum / (a.NumRows |> float))


let toAdjustStandardize (data:Matrix<float>) =                
    let colMeans =
        data |> meanColumnWise                    
    // Atttention: not entierly sure if space of data before
    let colStDev =
        data |> Matrix.Generic.enumerateColumnWise Seq.stDevPopulation |> Seq.toArray
     
    data
    |> Matrix.mapi (fun ri ci value ->
        if colStDev.[ci] = 0. then
            raise (System.ArgumentException(sprintf "Standard deviation cannot be zero (cannot standardize the constant variable at column index %i" ci))
        (value - colMeans.[ci]) / colStDev.[ci] )
    
    
toAdjustStandardize irisFeaturesMatrix

let s,u,vt = FSharp.Stats.Algebra.LinearAlgebra.SVD  ( toAdjustStandardize irisFeaturesMatrix )
let v = vt.Transpose |> Matrix.map (fun v -> v)

let principal_components = u * s 
(*** hide ***)
let vChart = Chart.Point(Seq.zip (Matrix.getCol v 0) (Matrix.getCol v 1))


(*** include-value:vChart ***)

(**
BioFSharp ML module 
--------------------
Principal component analysis of the iris data set and visual inspection of the result.
*)
open FSharp.Stats.ML
open FSharp.Stats.ML.Unsupervised


let adjCenter = PCA.toAdjustCenter irisFeaturesMatrix
(*** do-not-eval ***)
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

let loadingPlot =
    plotLoadingsColoredByGrouping irisPCA 
        [ "Sepal length"; "Sepal width"; "Petal length"; "Petal width" ] 
        (fun x -> x) 1 2


let scorePlot =
    plotScoresColoredByGrouping irisDataPCA irisLables (fun x -> x) 1 3
 
(*** include-value:loadingPlot ***)
(*** include-value:scorePlot ***)

(*** hide ***)
// ---------------
//// Kmeans clustering
//// For random cluster inititalization use randomInitFactory:
//let rng = new System.Random()
//let randomInitFactory : IterativeClustering.CentroidsFactory<float []> = 
//    IterativeClustering.randomCentroids<float []> rng
//let cvmaxFactory : IterativeClustering.CentroidsFactory<float []> = 
//    IterativeClustering.intitCVMAX
//let kmeansResult = 
//    IterativeClustering.kmeans <| DistanceMetrics.euclidean <| cvmaxFactory 
//    <| (Matrix.toJaggedArrayRowWise irisFeaturesMatrix) <| 3

//let chartsOfClassifiedData = 
//    Matrix.toJaggedArrayRowWise irisFeaturesMatrix
//    |> Seq.groupBy (fun dataPoint -> fst (kmeansResult.Classifier dataPoint))
//    |> Seq.sortBy fst
//    |> Seq.map (fun (key, values) -> 
//           values
//           |> Seq.map 
//                  (fun v -> 
//                  Chart.Line v 
//                  |> Chart.WithStyling
//                         (Color = System.Drawing.Color.Silver, BorderWidth = 1))
//           |> Chart.Combine
//           |> Chart.WithTitle(key.ToString()))
//    |> Chart.Rows
//    |> Chart.ShowChart
