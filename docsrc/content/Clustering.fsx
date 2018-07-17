(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/netstandard2.0"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/Fsharp.Plotly.dll"
#r "netstandard"
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

(*** define:heat1 ***)
Chart.Heatmap(data,ColNames=colnames,RowNames=(lable |> Seq.mapi (fun i s -> sprintf "%s%i" s i )),Colorscale=colorscaleValue,Showscale=true)
|> Chart.withMarginSize(Left=250.)
|> Chart.withSize(500.,1100.)
(*** include:heat1 ***)



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


// ---------------
// DBSACN clustering

//four dimensional clustering with sepal length, petal length, sepal width and petal width
let t = DbScan.compute DistanceMetrics.Array.euclideanNaN 5 1.0 data

//extract petal length and petal width
let petL_petW      = data |> Array.map (fun x -> [x.[2];x.[3]])

//extract petal width, petal length and sepal length  
let petW_petL_sepL = data |> Array.map (fun x -> [x.[3];x.[2];x.[0]])

//to create a chart with two dimensional data use the following function
let create2dChart (dfu:array<'a> -> array<'a> -> float) (minPts:int) (eps:float) (input:seq<#seq<'a>>) =  
    if (input |> Seq.head |> Seq.length) = 2 then  
        let result = DbScan.compute (dfu:array<'a> -> array<'a> -> float) (minPts:int) (eps:float) (input:seq<#seq<'a>>)
        let chartCluster = 
            if result.Clusterlist |> Seq.length > 0 then      
                result.Clusterlist
                |> Seq.mapi (fun i l ->
                                    l
                                    |> Seq.map (fun x -> x.[0],x.[1])
                                    |> Seq.distinct //more efficient visualization; no difference in plot but in point numbers
                                    |> Chart.Point
                                    |> Chart.withTraceName (sprintf "Cluster %i" i)
                            )
                |> Chart.Combine
            else Chart.Point []

        let chartNoise = 
            if result.Noisepoints |> Seq.length > 0 then 
                result.Noisepoints
                |> Seq.map (fun x -> x.[0],x.[1])  
                |> Seq.distinct //more efficient visualization; no difference in plot but in point numbers
                |> Chart.Point
                |> Chart.withTraceName "Noise"
            else Chart.Point []

        let chartname = 
            let noiseCount              = result.Noisepoints |> Seq.length
            let clusterCount            = result.Clusterlist |> Seq.length
            let clusteredPointsCount    = result.Clusterlist |> Seq.fold (fun acc x -> acc + (x |> Seq.length)) 0
            sprintf "eps:%A  minPts:%i  Cluster:%i  NoisePts:%i  AllPts:%i" eps minPts clusterCount noiseCount (noiseCount + clusteredPointsCount)

        [chartNoise;chartCluster]
        |> Chart.Combine
        |> Chart.withTitle chartname
    else failwith "create2dChart only can handle 2 coordinates"

//applied function for two dimensional 'sepal length, petal width' data set
create2dChart DistanceMetrics.Array.euclidean 20 0.5 petL_petW
|> Chart.withX_AxisStyle "Petal width"
|> Chart.withY_AxisStyle "Petal length"


//to create a chart with three dimensional data use the following function
let create3dChart (dfu:array<'a> -> array<'a> -> float) (minPts:int) (eps:float) (input:seq<#seq<'a>>) =   
    if (input |> Seq.head |> Seq.length) = 3 then  
        let result = DbScan.compute (dfu:array<'a> -> array<'a> -> float) (minPts:int) (eps:float) (input:seq<#seq<'a>>)
        let chartCluster = 
            if result.Clusterlist |> Seq.length > 0 then 
                result.Clusterlist
                |> Seq.mapi (fun i l ->
                                    l
                                    |> Seq.map (fun x -> x.[0],x.[1],x.[2])
                                    |> Seq.distinct //faster visualization; no difference in plot but in point number
                                    |> fun x -> Chart.Scatter3d (x,StyleParam.Mode.Markers)
                                    |> Chart.withTraceName (sprintf "Cluster %i" i)
                            )
                |> Chart.Combine
            else  Chart.Scatter3d ([],StyleParam.Mode.Markers)

        let chartNoise =
            if result.Noisepoints |> Seq.length > 0 then 
                result.Noisepoints
                |> Seq.map (fun x -> x.[0],x.[1],x.[2])  
                |> Seq.distinct //faster visualization; no difference in plot but in point number
                |> fun x -> Chart.Scatter3d (x,StyleParam.Mode.Markers)
                |> Chart.withTraceName "Noise"
            else Chart.Scatter3d ([],StyleParam.Mode.Markers)

        let chartname = 
            let noiseCount              = result.Noisepoints |> Seq.length
            let clusterCount            = result.Clusterlist |> Seq.length
            let clusteredPointsCount    = result.Clusterlist |> Seq.fold (fun acc x -> acc + (x |> Seq.length)) 0
            sprintf "eps:%A  minPts:%i  Cluster:%i  NoisePts:%i  AllPts:%i" eps minPts clusterCount noiseCount (noiseCount + clusteredPointsCount)

        [chartNoise;chartCluster]
        |> Chart.Combine
        |> Chart.withTitle chartname
    else failwith "create3dChart only can handle 3 coordinates"


//applied function for three dimensional 'petal width, petal length, sepal length' data set
//for faster computation you can use the squaredEuclidean distance and set your eps to its square
create3dChart DistanceMetrics.Array.euclideanNaNSquared 20 (0.7**2.) petW_petL_sepL 
|> Chart.withX_AxisStyle "Petal length"
|> Chart.withY_AxisStyle "Petal width"
|> Chart.withZ_AxisStyle "Sepal length"



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
(*** include:heat1 ***)





