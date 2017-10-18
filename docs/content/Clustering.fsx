(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"

open FSharp.Plotly

#r "FSharp.Stats.dll"
open FSharp.Stats


let fromFileWithSep (separator:char) (filePath) =     
    // The function is implemented using a sequence expression
    seq {   let sr = System.IO.File.OpenText(filePath)
            while not sr.EndOfStream do 
                let line = sr.ReadLine() 
                let words = line.Split separator//[|',';' ';'\t'|] 
                yield words }


let lable,data =
    fromFileWithSep ',' (__SOURCE_DIRECTORY__ + "/data/irisData.csv")
    |> Seq.skip 1
    |> Seq.map (fun arr -> arr.[4], [| float arr.[0]; float arr.[1]; float arr.[2]; float arr.[3]; |])
    |> Seq.toArray
    |> Array.shuffleFisherYates
    |> Array.unzip

let colnames = ["Sepal length";"Sepal width";"Petal length";"Petal width"]


let colorscaleValue = 
    //StyleParam.ColorScale.Electric
    StyleParam.Colorscale.Electric //Custom [(0.0,"#3D9970");(1.0,"#001f3f")]

Chart.Heatmap(data,ColNames=colnames,RowNames=(lable |> Seq.mapi (fun i s -> sprintf "%s%i" s i )),Colorscale=colorscaleValue,Showscale=true)
|> Chart.withMarginSize(Left=250.)
|> Chart.withSize(500.,1100.)
(*** include-it:heat1 ***)
|> Chart.Show


open FSharp.Stats.ML
open FSharp.Stats.ML.Unsupervised
open FSharp.Stats.ML.Unsupervised.HierarchicalClustering

// ---------------
// Kmeans clustering
// For random cluster inititalization use randomInitFactory:
let rng = new System.Random()
let randomInitFactory : IterativeClustering.CentroidsFactory<float []> = 
    IterativeClustering.randomCentroids<float []> rng
//let cvmaxFactory : IterativeClustering.CentroidsFactory<float []> = 
//    IterativeClustering.intitCVMAX
  

let kmeansResult = 
    IterativeClustering.kmeans <| DistanceMetrics.euclidean <| randomInitFactory 
    <| data <| 4



Array.zip lable data
|> Array.sortBy (fun (l,dataPoint) -> l,fst (kmeansResult.Classifier dataPoint)) 
|> Array.unzip
|> fun (l,d) -> Chart.Heatmap(d,ColNames=colnames,RowNames=(l |> Seq.mapi (fun i s -> sprintf "%s%i" s i )),Colorscale=colorscaleValue,Showscale=true)
|> Chart.withSize(500.,1100.)
|> Chart.Show


// ---------------
// DBSACN clustering
// For random cluster inititalization use randomInitFactory:

let t = DbScan.compute DistanceMetrics.euclidean 5 1.0 data


// ---------------
// Hierarchical clustering
//// fast #time
//let htmp = HierarchicalClustering.generate' DistanceMetrics.euclidean Linker.wardLwLinker data
// slower
let htmp = 
    HierarchicalClustering.generate DistanceMetrics.euclidean Linker.wardLwLinker data
    |> HierarchicalClustering.flattenHClust
    
let hlable =    
    htmp
    |> Seq.map (fun c -> lable.[HierarchicalClustering.getClusterId c])

let hdata =    
    htmp
    |> Seq.choose (fun c -> HierarchicalClustering.tryGetLeafValue c)



Chart.Heatmap(hdata,ColNames=colnames,RowNames=(hlable |> Seq.mapi (fun i s -> sprintf "%s%i" s i )),Colorscale=colorscaleValue,Showscale=true)
|> Chart.withMarginSize(Left=250.)
|> Chart.withSize(500.,2100.)
(*** include-it:heat1 ***)
|> Chart.Show




