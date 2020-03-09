namespace FSharp.Stats.ML.Unsupervised

open FSharp.Stats
open FSharpAux

module ClusterNumber =

    open FSharp.Stats.ML.Unsupervised.IterativeClustering
    
    /// Simple estimator for number of cluster (k) // can be used as the upper bound for other methods
    let k_ruleOfThumb observations = 
        let ruleOfThumb (n:int) = sqrt (float n / 2.)
        ruleOfThumb (Seq.length observations)

    /// Akaike Information Criterion (AIC)
    let calcAIC (bootstraps:int) (iClustering:int->KClusteringResult<float []>) maxK   =   
        let AICFromClusterResult (cr:KClusteringResult<'a []>) =
            let rss = cr.ClosestDistances |> Array.sumBy (fun (fst,snd) -> snd * snd) //snd
            let k = cr.Centroids.Length
            let m = cr.ClosestDistances.Length
            rss + float (2 * m * k)
        [1..maxK]
        //|> PSeq.map (fun k ->
        |> Seq.map (fun k ->
            let aic' =
                [1..bootstraps]
                |> Seq.averageBy (fun _ ->
                    let kmeansResult = iClustering k                            
                    let aic = AICFromClusterResult kmeansResult
                    aic)
            (k,aic')
            ) 
        |> Seq.toArray
        //|> PSeq.toArray
        //|> Array.sortBy fst


module GapStatistics = 
    
    open FSharp.Stats.ML
    open FSharp.Stats.ML.Unsupervised


// An implementation of the gap statistic algorithm from Tibshirani, Walther, and Hastie's "Estimating the number of clusters in a data set via the gap statistic".
//
// Given a row seq `data`, where rows are observations and columns are individual dimensions, compute and plot the gap statistic 
// (according to a uniform reference distribution taking into account the original dataset's shape ).
 
//A major disadvantage of elbow and average silhouette methods is that they measure a global clustering characteristic only. A more sophisticated method 
//is to use the gap statistic which provides a statistical procedure to formalize the elbow/silhouette heuristic in order to estimate the optimal number of clusters.

(*
The gap statistic compares the total within intra-cluster variation for different values of k with their expected values under null reference distribution 
of the data. The estimate of the optimal clusters will be value that maximize the gap statistic (i.e, that yields the largest gap statistic). This means 
that the clustering structure is far away from the random uniform distribution of points.
https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
*)
    // GenericClusterDispersion, given a dataset (data<'a>) and clusternumber k,
    // returns the cluster dispersion.      //after applying a cluster algorithm to the dataset.
    type GenericClusterDispersion<'a> = array<'a>  -> int -> float
    
    //GenericPointGenerator, given a dataset (data<'a>), 
    //returns dataset' containing random values in range of the original dataset.
    type GenericPointGenerator<'a> = array<'a> -> array<'a>
    
    //Represents a gap statistic result.
    type GapStatisticResult = {
        ClusterIndex        : int;
        Dispersion          : float;
        ReferenceDispersion : float;
        RefDispersion_StDev : float;
        Gaps                : float;
        }

    //Creates a gap statistic result.
    let createGapStatisticResult clusterIndex dispersion referenceDispersion refDispersion_StDev gaps =                
        { ClusterIndex = clusterIndex; Dispersion = dispersion; ReferenceDispersion = referenceDispersion;  RefDispersion_StDev = refDispersion_StDev; Gaps = gaps }


    /// Not used.
    // Calculates dispersion of data clustered into k clusters.
    let dispersion (calcDispersion:GenericClusterDispersion<'a>) (data:array<'a>) k =          
        calcDispersion data k

    // Calculate dispersion of reference data' clustered into k clusters.
    let referenceDispersion (rndPointGenerator:GenericPointGenerator<'a>) (calcDispersion:GenericClusterDispersion<'a>) (data:array<'a>) (bootstraps:int) k = 
        [|1..bootstraps|]
        |> Array.map (fun bIndex ->
            data
            |> rndPointGenerator 
            |> fun data -> calcDispersion data k )

    // Calculates gap statistic of a dataset for clusternumbers from 1 to maxK.
    let calculate (rndPointGenerator:GenericPointGenerator<'a>) (bootstraps:int) (calcDispersion:GenericClusterDispersion<'a>) maxK (data:array<'a>) =
        match maxK with
        | 0 -> failwith "maxK must be 1 or greater"
        | _ ->
                [|1..maxK|]
                //|> PSeq.map //Multi threading with System.Random number generator may eventually lead to zero-loop
                |> Array.map (fun k -> 
                    let refDisp     =  referenceDispersion rndPointGenerator calcDispersion data bootstraps k 
                    let disp        =  calcDispersion data k    
                    
                    let refDispMean = refDisp  |> Seq.mean 
                    let refDispStd  = refDisp  |> Seq.stDev 
                    (createGapStatisticResult k disp refDispMean refDispStd (refDispMean-disp)) ) 
                //|> PSeq.toArray
                |> Array.sortBy (fun c -> c.ClusterIndex)


    module PointGenerators =
    
        // Generate uniform points within the range of `data`.
        let generate_uniform_points (rnd:System.Random) =        
            fun (data:array<float[]>) -> 
                let min = matrix data |> Matrix.mapiCols (fun i x -> Seq.min x) |> Array.ofSeq
                let max = matrix data |> Matrix.mapiCols (fun i x -> Seq.max x) |> Array.ofSeq
                let range = Array.map2 (fun ma mi -> ma - mi) max min 
                
                let generateUniform () =
                    min
                    |> Array.mapi (fun i x ->  
                        x + (rnd.NextDouble() * range.[i]))
      
                Array.init data.Length (fun x -> generateUniform ())

                      
        // Generate uniform points for an appropriate reference distribution data' that takes the original data shape into account (from Tibshirani, Walther and Hastie 2001).
        let generate_uniform_points_PCA (rnd:System.Random)  =                    
            fun (data:array<float[]>) -> 
                //fun (data:array<'a>) -> 
                //let generateUniform (s:Intervals.Interval<float>) =
                //   (Intervals.getStart s) + (rnd.NextDouble() * (Intervals.getRange s))            
                //let matrix    = matrix data
                //let adjCenter = PCA.toAdjustStandardize matrix
                //let comp      = PCA.compute adjCenter matrix 
                //let pcaResult = PCA.transform adjCenter comp matrix                                      
                //let data' =          
                //    PCA.revert adjCenter comp  pcaResult 
                //    |> Matrix.toJaggedArray  
                //data'
                //|> Array.map (fun item -> Intervals.ofSeq item,item)
                //|> Array.map (fun (s,data) -> 
                //    data 
                //    |> Array.map (fun _ -> generateUniform s))  
                let dataMat = (JaggedArray.toArray2D data)
                let (u,s,vt) = Algebra.SVD.compute dataMat
                let X' = (Matrix.ofJaggedArray data) * (Matrix.ofArray2D vt).Transpose
                let min = X' |> Matrix.mapiCols (fun i x -> Seq.min x) |> Array.ofSeq
                let max = X' |> Matrix.mapiCols (fun i x -> Seq.max x) |> Array.ofSeq
                let range = Seq.map2 (fun ma mi -> ma - mi) max min |> Array.ofSeq
                let generateUniform () =
                    min
                    |> Array.mapi (fun i x ->  
                        x + (rnd.NextDouble() * range.[i]))
                //generate uniform data points in elevated space
                let generateUniformSVD = Array.init data.Length (fun x -> generateUniform())
                //backtransform points to get reference data
                (Matrix.ofJaggedArray generateUniformSVD) * (Matrix.ofArray2D vt)  
                |> Matrix.toJaggedArray

    
    module ClusterDispersionMetric =
        
        //[<Obsolete("Do not use. Use [logDispersionKMeans_initRandom] instead.")>]
        //// Calculate log(sum_i(within-cluster_i sum of squares around cluster_i mean)) of kmeans clustering result.
        //let logDispersionKMeans_initRandom_old = 
        //    let rnd = System.Random()
        //    let aggregator = IterativeClustering.avgCentroid
        //    let factory = IterativeClustering.randomCentroids rnd 
        //    (fun (data:array<float[]>) (k:int) -> 
        //                            let kMeans = IterativeClustering.compute DistanceMetrics.euclideanNaN factory aggregator data k    
        //                            kMeans.ClosestDistances
        //                            |> Array.averageBy (fun (ind,dist) -> let ldist = log dist
        //                                                                  ldist*ldist) )
        //if a cluster consists of one point only, the closestDistance is 0, therefore the log is -infinity and the average cant be calculated!!
        //log is nonlinear transformation, so it has to be performed in the last step

        // Calculates the log clustering dispersion as defined in (from Tibshirani, Walther and Hastie 2001)
        let logDispersionKMeans_initRandom = 
            let rnd = System.Random()
            let aggregator = IterativeClustering.avgCentroid
            let factory = IterativeClustering.randomCentroids rnd 
            let clusterDispersion (cluster:float[][]) =
                let distances =
                    cluster 
                    |> Array.sumBy (fun i -> 
                        cluster 
                        |> Array.sumBy (fun j -> DistanceMetrics.euclideanNaNSquared i j )
                        )
                cluster.Length,distances
            (fun (data:array<float[]>) (k:int) -> 
                let kMeans = IterativeClustering.compute DistanceMetrics.euclideanNaNSquared factory aggregator data k    
                data
                |> Array.groupBy (fun x -> kMeans.Classifier x)
                |> Array.map snd
                |> Array.map clusterDispersion
                |> Array.sumBy (fun (n,d) -> 1./(2.* float n) * d)
                |> log
                )

        
        ////[<Obsolete("Do not use. Use [logDispersionKMeans_initCvMax] instead.")>]
        //// Calculate log(sum_i(within-cluster_i sum of squares around cluster_i mean)) of kmeans clustering result.
        //let logDispersionKMeans_initCvMax_old = 
        //    let aggregator = IterativeClustering.avgCentroid
        //    let factory = IterativeClustering.intitCVMAX 
        //    (fun (data:array<float[]>) (k:int) -> 
        //                            let kMeans = IterativeClustering.compute DistanceMetrics.euclideanNaN factory aggregator data k    
        //                            kMeans.ClosestDistances
        //                            |> Array.averageBy (fun (ind,dist) -> let ldist = log dist
        //                                                                  ldist*ldist) )

        // Calculate log(sum_i(within-cluster_i sum of squares around cluster_i mean)) of kmeans clustering result.
        let logDispersionKMeans_initCvMax = 
            let aggregator = IterativeClustering.avgCentroid
            let factory = IterativeClustering.intitCVMAX 
            let clusterDispersion (cluster:float[][]) =
                let distances =
                    cluster 
                    |> Array.sumBy (fun i -> 
                        cluster 
                        |> Array.sumBy (fun j -> DistanceMetrics.euclideanNaNSquared i j )
                        )
                cluster.Length,distances
            (fun (data:array<float[]>) (k:int) -> 
                let kMeans = IterativeClustering.compute DistanceMetrics.euclideanNaNSquared factory aggregator data k    
                data
                |> Array.groupBy (fun x -> kMeans.Classifier x)
                |> Array.map snd
                |> Array.map clusterDispersion
                |> Array.sumBy (fun (n,d) -> 1./(2.* float n) * d)
                |> log
                )