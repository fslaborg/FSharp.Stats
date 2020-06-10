(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/netstandard2.0"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/Fsharp.Plotly.dll"
#r "../../packages/FSharpAux/lib/netstandard2.0/FSharpAux.dll"
#r "netstandard"
open FSharp.Plotly

#r "FSharp.Stats.dll"
open FSharp.Stats
open FSharpAux

let axis title= Axis.LinearAxis.init(Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showline=true,Showgrid=true)
let axisRange title range= Axis.LinearAxis.init(Title=title,Range=StyleParam.Range.MinMax(range),Mirror=StyleParam.Mirror.All,Showgrid=false,Ticks=StyleParam.TickOptions.Inside,Showline=true)

(**
#Clustering

##Iterative Clustering

###k-means clustering
*)

open FSharp.Stats
open FSharp.Plotly

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
   
(*** hide ***)
let colnames = ["Sepal length";"Sepal width";"Petal length";"Petal width"]

let colorscaleValue = 
    StyleParam.Colorscale.Electric //Custom [(0.0,"#3D9970");(1.0,"#001f3f")]
    
let dataChart = 
    Chart.Heatmap(data,ColNames=colnames,RowNames=(lable |> Seq.mapi (fun i s -> sprintf "%s%i" s i )),Colorscale=colorscaleValue,Showscale=true)
    |> Chart.withMarginSize(Left=250.)
    |> Chart.withTitle "raw iris data"
(*** include-value:dataChart ***)

open FSharp.Stats.ML
open FSharp.Stats.ML.Unsupervised
open FSharp.Stats.ML.Unsupervised.HierarchicalClustering


// Kmeans clustering

// For random cluster inititalization use randomInitFactory:
let rnd = new System.Random()
let randomInitFactory : IterativeClustering.CentroidsFactory<float []> = 
    IterativeClustering.randomCentroids<float []> rnd

//let cvmaxFactory : IterativeClustering.CentroidsFactory<float []> = 
//    IterativeClustering.intitCVMAX
  
let kmeansResult = 
    IterativeClustering.kmeans <| DistanceMetrics.euclidean <| randomInitFactory 
    <| data <| 4

let clusteredIrisData =
    Array.zip lable data
    |> Array.sortBy (fun (l,dataPoint) -> l,fst (kmeansResult.Classifier dataPoint)) 
    |> Array.unzip
    |> fun (l,d) -> 
        let labels = l |> Seq.mapi (fun i x -> x + string i)
        Chart.Heatmap(d,ColNames=colnames,RowNames=labels,Colorscale=colorscaleValue,Showscale=true)
        |> Chart.withMarginSize(Left=250.)
        |> Chart.withTitle "clustered iris data (k-means clustering)"

(*** include-value:clusteredIrisData ***)

// To get the best kMeans clustering result in terms of the average squared distance of each point
// to its centroid, perform the clustering b times and minimize the dispersion.
let getBestkMeansClustering data k bootstraps =
    [1..bootstraps]
    |> List.mapi (fun i x -> 
        IterativeClustering.kmeans <| DistanceMetrics.euclidean <| randomInitFactory <| data <| k
        )
    |> List.minBy (fun clusteringResult -> IterativeClustering.DispersionOfClusterResult clusteringResult)

(**
##Density based clustering

###DBSCAN

*)
//four dimensional clustering with sepal length, petal length, sepal width and petal width
let t = DbScan.compute DistanceMetrics.Array.euclideanNaN 5 1.0 data

//extract petal length and petal width
let petL_petW      = data |> Array.map (fun x -> [|x.[2];x.[3]|])

//extract petal width, petal length and sepal length  
let petW_petL_sepL = data |> Array.map (fun x -> [|x.[3];x.[2];x.[0]|])

//to create a chart with two dimensional data use the following function
let create2dChart (dfu:array<'a> -> array<'a> -> float) (minPts:int) (eps:float) (input:seq<#seq<'a>>) =  
    if (input |> Seq.head |> Seq.length) <> 2 then failwithf "create2dChart only can handle 2 coordinates"
    
    let result = DbScan.compute dfu minPts eps input
    
    let chartCluster = 
        if result.Clusterlist |> Seq.length > 0 then      
            result.Clusterlist
            |> Seq.mapi (fun i l ->
                l
                |> Seq.map (fun x -> x.[0],x.[1])
                |> Seq.distinct //more efficient visualization; no difference in plot but in point numbers
                |> Chart.Point
                |> Chart.withTraceName (sprintf "Cluster %i" i))
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
        let noiseCount    = result.Noisepoints |> Seq.length
        let clusterCount  = result.Clusterlist |> Seq.length
        let clPtsCount    = result.Clusterlist |> Seq.sumBy Seq.length
        sprintf "eps:%.1f minPts:%i pts:%i cluster:%i noisePts:%i" 
            eps minPts (noiseCount + clPtsCount) clusterCount noiseCount 

    [chartNoise;chartCluster]
    |> Chart.Combine
    |> Chart.withTitle chartname
    |> Chart.withX_Axis (axis "Petal width") 
    |> Chart.withY_Axis (axis "Petal length")
    
let clusteredChart = create2dChart DistanceMetrics.Array.euclidean 20 0.5 petL_petW

(*** include-value:clusteredChart ***)

//to create a chart with three dimensional data use the following function
let create3dChart (dfu:array<'a> -> array<'a> -> float) (minPts:int) (eps:float) (input:seq<#seq<'a>>) =   
    if (input |> Seq.head |> Seq.length) <> 3 then failwithf "create3dChart only can handle 3 coordinates"
    
    let result = DbScan.compute dfu minPts eps input
    
    let chartCluster = 
        if result.Clusterlist |> Seq.length > 0 then 
            result.Clusterlist
            |> Seq.mapi (fun i l ->
                l
                |> Seq.map (fun x -> x.[0],x.[1],x.[2])
                |> Seq.distinct //faster visualization; no difference in plot but in point number
                |> fun x -> Chart.Scatter3d (x,StyleParam.Mode.Markers)
                |> Chart.withTraceName (sprintf "Cluster_%i" i))
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
        let noiseCount    = result.Noisepoints |> Seq.length
        let clusterCount  = result.Clusterlist |> Seq.length
        let clPtsCount    = result.Clusterlist |> Seq.sumBy Seq.length
        sprintf "eps:%.1f minPts:%i n:%i Cluster:%i NoisePts:%i" 
            eps minPts (noiseCount + clPtsCount) clusterCount noiseCount 

    [chartNoise;chartCluster]
    |> Chart.Combine
    |> Chart.withTitle chartname
    |> Chart.withX_Axis (axis "Petal width")
    |> Chart.withY_Axis (axis "Petal length")
    |> Chart.withZ_Axis (axis "Sepal length")
        


//for faster computation you can use the squaredEuclidean distance and set your eps to its square
let clusteredChart3D = create3dChart DistanceMetrics.Array.euclideanNaNSquared 20 (0.7**2.) petW_petL_sepL 

(*** include-value:clusteredChart3D ***)

(**
##Hierarchical clustering



*)

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


let hierClusteredData = 
    let labels = hlable |> Seq.mapi (fun i s -> sprintf "%s%i" s i)
    Chart.Heatmap(hdata,ColNames=colnames,RowNames=labels,Colorscale=colorscaleValue,Showscale=true)
    |> Chart.withMarginSize(Left=250.)
    |> Chart.withTitle "clustered iris data (hierarchical Clustering)"
(*** include-value:hierClusteredData ***)



(**

#Determining the optimal number of clusters

##Rule of thumb

The rule of thumb is a very crude cluster number estimation only based on the number of data points.

Reference: 'Review on Determining of Cluster in K-means Clustering'; Kodinariya et al; January 2013

*)

//optimal k for iris data set by using rule-of-thumb
let ruleOfThumb = ClusterNumber.k_ruleOfThumb data

(*** include-value:ruleOfThumb ***)

(**


##Elbow criterion

The elbow criterion is a visual method to determine the optimal cluster number. The cluster dispersion is measured as the sum of all average (squared) euclidean distance of each point to its associated centroid.
The point at wich the dispersion drops drastically and further increase in k does not lead to a strong decrease in dispersion is the optimal k.

Reference: 'Review on Determining of Cluster in K-means Clustering'; Kodinariya et al; January 2013

*)
open IterativeClustering
open DistanceMetrics

let kElbow = 10

let iterations = 10 

let dispersionOfK = 
    [|1..kElbow|]
    |> Array.map (fun k -> 
        let (dispersion,std) = 
            [|1..iterations|]
            |> Array.map (fun i -> 
                kmeans euclideanNaNSquared (randomCentroids rnd) data k
                |> DispersionOfClusterResult)
            |> fun dispersions -> 
                Seq.mean dispersions, Seq.stDev dispersions
        k,dispersion,std
        )

let elbowChart = 
    Chart.Line (dispersionOfK |> Array.map (fun (k,dispersion,std) -> k,dispersion))
    |> Chart.withYErrorStyle (dispersionOfK |> Array.map (fun (k,dispersion,std) -> std))
    |> Chart.withX_Axis (axis "k")
    |> Chart.withY_Axis (axis "dispersion")
    |> Chart.withTitle "Iris data set dispersion"


(*** include-value:elbowChart ***)

(**
#AIC

[Reference](https://nlp.stanford.edu/IR-book/html/htmledition/cluster-cardinality-in-k-means-1.html)

The Akaike information criterion (AIC) balances the information gain (with raising k) against parameter necessarity (number of k).
The k that minimizes the AIC is assumed to be the optimal one. 

*)


let aicBootstraps = 10

//optimal k for iris data set by using aic
let (aicK,aicMeans,aicStd) =
    //perform 10 iterations and take the mean and standard deviation of the aic
    let aic = 
        [|1..aicBootstraps|]
        |> Array.map (fun b -> ClusterNumber.calcAIC 10 (kmeans euclideanNaNSquared (randomCentroids rnd) data) 15)
    aic
    |> Array.map (fun iteration -> Array.map snd iteration)
    |> JaggedArray.transpose
    |> Array.mapi (fun i aics -> 
        i+1,Seq.mean aics,Seq.stDev aics)
    |> Array.unzip3

let aicChart = 
    Chart.Line (aicK,aicMeans)
    |> Chart.withX_Axis (axis "k")
    |> Chart.withY_Axis (axis "AIC")
    |> Chart.withYErrorStyle aicStd

(*** include-value:aicChart ***)

(**
##Silhouette coefficient

The silhouette index ranges from -1 to 1, where -1 indicates a missclassified point, and 1 indicates a perfect fit.
It can be calculated for every point by comparing the mean intra cluster distance with the nearest mean inter cluster distance.
The mean of all indices can be visualized, where a maximal value indicates the optimal k.

Reference: 'Review on Determining of Cluster in K-means Clustering'; Kodinariya et al; January 2013

*)


// The following example expects the raw data to be clustered by k means clustering.
// If you already have clustered data use the 'silhouetteIndex' function instead.

let silhouetteData = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/data/silhouetteIndexData.txt")
    |> Array.map (fun x -> 
        let tmp = x.Split '\t'
        [|float tmp.[0]; float tmp.[1]|])

let sI = 
    ML.Unsupervised.ClusterNumber.silhouetteIndexKMeans 
        50              // number of bootstraps 
        (kmeans euclideanNaNSquared (randomCentroids rnd) silhouetteData) 
        silhouetteData  // input data
        15              // maximal number of allowed k

let rawDataChart =
    silhouetteData 
    |> Array.map (fun x -> x.[0],x.[1])
    |> Chart.Point

let silhouetteIndicesChart =
    Chart.Line (sI |> Array.map (fun x -> x.ClusterNumber,x.SilhouetteIndex))
    |> Chart.withYErrorStyle (sI |> Array.map (fun x -> x.SilhouetteIndexStDev))

(*** hide ***)

let combinedSilhouette =
    [
    rawDataChart |> Chart.withX_Axis (axis "") |> Chart.withY_Axis (axis "") |> Chart.withTraceName "raw data"
    silhouetteIndicesChart |> Chart.withX_Axis (axis "k") |> Chart.withY_Axis (axis "silhouette index") |> Chart.withTraceName "silhouette"
    ]
    |> Chart.Stack (2,0.1)

(*** include-value:combinedSilhouette ***)

(**

##GapStatistics

Reference: 'Estimating the number of clusters in a data set via the gap statistic'; J. R. Statist. Soc. B (2001); Tibshirani, Walther, and Hastie

Gap statistics allows to determine the optimal cluster number by comparing the cluster dispersion (intra-cluster variation) of a reference dataset to the original data cluster dispersion.
For each k both dispersions are calculated, while for the reference dataset multiple iterations are performed for each k. The difference of the log(dispersionOriginal) and the log(dispersionReference) is called 'gap'.
The maximal gap points to the optimal cluster number. 

Two ways to generate a reference data set are implemented.

 - a uniform coverage within the range of the original data set
  
 - a PCA based point coverage, that considers the density/shape of the original data

*)

(*** hide ***)
let gapStatisticsData = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/data/gapStatisticsData.txt")
    |> Array.map (fun x ->
        let tmp = x.Split '\t'
        tmp |> Array.map float)

let gapDataChart = 
    [
    gapStatisticsData|> Array.map (fun x -> x.[0],x.[1]) |> Chart.Point |> Chart.withTraceName "original" |> Chart.withX_Axis (axisRange "" (-4.,10.)) |> Chart.withY_Axis (axisRange "" (-2.5,9.))
    (GapStatistics.PointGenerators.generate_uniform_points rnd gapStatisticsData) |> Array.map (fun x -> x.[0],x.[1]) |> Chart.Point |> Chart.withTraceName "uniform" |> Chart.withX_Axis (axisRange "" (-4.,10.)) |> Chart.withY_Axis (axisRange "" (-2.5,9.))
    (GapStatistics.PointGenerators.generate_uniform_points_PCA rnd gapStatisticsData) |> Array.map (fun x -> x.[0],x.[1]) |> Chart.Point |> Chart.withTraceName "uniform PCA" |> Chart.withX_Axis (axisRange "" (-4.,10.)) |> Chart.withY_Axis (axisRange "" (-2.5,9.))
    ]
    |> Chart.Stack 3
    |> Chart.withSize(800.,400.)
(*** include-value:gapDataChart ***)
(**
The log(dispersionReference) should decrease with rising k, but - if clusters are presend in the data - should be greater than the log(dispersionOriginal). 

*)
open GapStatistics

//create gap statistics
let gaps =
    GapStatistics.calculate
        (PointGenerators.generate_uniform_points_PCA rnd)      //uniform point distribution
        100// no gain above 500                                //number of bootstraps samples 
        ClusterDispersionMetric.logDispersionKMeans_initRandom //dispersion metric of clustering algorithm
        10                                                     //maximal number of allowed clusters
        gapStatisticsData                                      //float [] [] data of coordinates

//number of clusters        
let k        = gaps |> Array.map (fun x -> x.ClusterIndex)
//log(dispersion) of the original data (with rising k)
let disp     = gaps |> Array.map (fun x -> x.Dispersion)
//log(dispersion) of the reference data (with rising k)
let disp_ref = gaps |> Array.map (fun x -> x.ReferenceDispersion)
//log(dispersionRef) - log(dispersionOriginal)
let gap      = gaps |> Array.map (fun x -> x.Gaps)
//standard deviation of reference data set dispersion
let std      = gaps |> Array.map (fun x -> x.RefDispersion_StDev)

let gapStatisticsChart =
    let dispersions =
        [
        Chart.Line (k,disp)    |> Chart.withTraceName "disp"
        Chart.Line (k,disp_ref)|> Chart.withTraceName "disp_ref" |> Chart.withYErrorStyle(std)
        ]
        |> Chart.Combine 
        |> Chart.withX_Axis (axisRange "" (0.,11.)) 
        |> Chart.withY_Axis (axis "log(disp)")
    let gaps = 
        Chart.Line (k,gap)|> Chart.withTraceName "gaps"
        |> Chart.withX_Axis (axisRange "k" (0.,11.)) 
        |> Chart.withY_Axis (axis "gaps")
    [dispersions; gaps]
    |> Chart.Stack 1

(*** include-value:gapStatisticsChart ***)

(**
The maximal gap points to the optimal cluster number with the following condition:

 - kopt = smallest k such that Gap(k)>= Gap(k+1)-sk+1

 - where sk = std * sqrt(1+1/bootstraps)

*)

//calculate s(k) out of std(k) and the number of performed iterations for the refernce data set
let s_k   = std |> Array.map (fun sd -> sd * sqrt(1. + 1./500.)) //bootstraps = 500 

let gapChart =
    Chart.Line (k,gap)
    |> Chart.withYErrorStyle(s_k)
    |> Chart.withX_Axis (axisRange "k" (0.,11.)) 
    |> Chart.withY_Axis (axisRange "gap" (-0.5,2.)) 
    
(*** include-value:gapChart ***)

//choose k_opt = smallest k such that Gap(k)>= Gap(k+1)-sk+1, where sk = sdk * sqrt(1+1/bootstraps)
let k_opt = 
    Array.init (gap.Length - 2) (fun i -> gap.[i] >= gap.[i+1] - s_k.[i+1])
    |> Array.findIndex id
    |> fun x -> sprintf "The optimal cluster number is: %i" (x + 1)
    
(*** include-value:k_opt ***)
