namespace FSharp.Stats.ML.Unsupervised

open System
open FSharp.Stats
open FSharpAux

module ClusterNumber =

    open FSharp.Stats.ML.Unsupervised.IterativeClustering
    
    /// <summary>Simple estimator for number of cluster (k) // can be used as the upper bound for other methods</summary>
    /// <remarks></remarks>
    /// <param name="observations"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let kRuleOfThumb observations = 
        let ruleOfThumb (n:int) = sqrt (float n / 2.)
        ruleOfThumb (Seq.length observations)

    [<Obsolete("Use kRuleOfThumb instead.")>]
    let k_ruleOfThumb observations = kRuleOfThumb observations

    /// <summary>Akaike Information Criterion (AIC)</summary>
    /// <remarks></remarks>
    /// <param name="bootstraps"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
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

    type SilhouetteResult = {
        ClusterNumber       : int
        SilhouetteIndex     : float
        SilhouetteIndexStDev: float
        }

    let private createSilhouetteResult k avg std = {
        ClusterNumber       = k
        SilhouetteIndex     = avg
        SilhouetteIndexStDev= std
        }

    /// <summary>Calculates the silhouette score for a clustered data set where the coordinates of each data point is given as float [].<br />The index ranges from -1 (bad clustering result) to 1 (perfekt clustering result)</summary>
    /// <remarks></remarks>
    /// <param name="clusteredData"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let silhouetteIndex (clusteredData:float [] [] []) =
        let averageDistance (item: float []) (cluster:float[][]) =
            cluster 
            |> Array.averageBy (fun j -> DistanceMetrics.Array.euclideanNaNSquared item j)

        let silhouetteIndexK = 
            clusteredData
            |> Array.mapi (fun i cluster -> 
                let externalPoints = 
                    clusteredData 
                    |> Array.indexed 
                    |> Array.filter (fun (j,cl) -> j <> i) 
                    |> Array.map snd
                cluster
                |> Array.map (fun point ->  
                    let clustersize = float cluster.Length
                    let intraCluster = 
                        averageDistance point cluster  
                        //correction for datapoint itself sum/(n-1) not sum/n
                        |> fun intra -> intra * clustersize / (max 1. (clustersize - 1.)) //max ensures correct result at singletons
                    let interCluster = 
                        //filters out cluster of current point to get interCluster distance
                        externalPoints
                        |> Array.map (averageDistance point)
                        |> Array.min //defines the neighboring cluster
                    (interCluster - intraCluster) / (max interCluster intraCluster))
                |> fun tmp -> tmp.Length,Seq.mean tmp) 
            |> fun silhouetteIndices -> 
                let count = Array.sumBy fst silhouetteIndices
                silhouetteIndices
                |> Array.sumBy (fun (n,sI) -> (float n) * sI)
                |> fun sISum -> sISum / float count
        silhouetteIndexK
        
    /// <summary>The silhouette index can be used to determine the optimal cluster number in k means clustering.<br />bootstraps indicates the number the k means clustering is performed for each k and maxK indicated the maximal cluster number.</summary>
    /// <remarks></remarks>
    /// <param name="bootstraps"></param>
    /// <param name="iClustering"></param>
    /// <param name="data"></param>
    /// <param name="maxK"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let silhouetteIndexKMeans (bootstraps:int) (iClustering:int -> KClusteringResult<float []>) (data:float [] []) maxK =
        [|2..maxK|]
        |> Array.map (fun k ->
            printfn "iteration k = %i" k
            [|1..bootstraps|]
            //|> Array.map (fun b -> 
            |> PSeq.map (fun b -> 
                let kmeansResult :KClusteringResult<float []> = iClustering k 
                let clusteredData = 
                    data
                    |> Array.map (fun x -> kmeansResult.Classifier x,x)
                    |> Array.groupBy (fun ((index,centroid),data) -> index)
                    |> Array.map (fun (index,cluster) -> 
                        cluster 
                        |> Array.map (fun ((index,centroid),data) -> data))
                silhouetteIndex clusteredData
            )
            |> PSeq.toArray
            |> fun x -> createSilhouetteResult k (Seq.mean x) (Seq.stDev x)
        )


    /// <summary>Calculates Normalized Mutual Information as a measure for clustering quality</summary>
    /// <remarks>The correctLabels and Clustered Labels must have the same length</remarks>
    /// <param name="correctLabels">True data labels represented by integers</param>
    /// <param name="clusteredLabels">Cluster indices represented by integers</param>
    /// <returns>Returns a NMI between 0 and 1. With 1 being a perfect match.</returns>
    /// <example>
    /// <code>
    ///    let trueLabels = [|"blue";"blue";"yellow";"red";"yellow"|]
    ///    let trueLabelsAsInt = [|1; 1; 3; 2; 3|]
    ///    let clusteredLabels = [|6; 6; 5; 5; 5|]
    ///    let nmi = calcNMI trueLabelsAsInt clusteredLabels
    ///    //results in 0.77897
    /// </code>
    /// </example>
    let calcNMI (correctLabels: int[]) (clusteredLabels: int[]) =
        
        if correctLabels.Length <> clusteredLabels.Length then failwithf "sequences must be of equal length"

        let f (correctLabels: int[]) (clusteredLabels: int[]) label1 label2 =
            let compare =
                let mutable correct = 0
                for i in 0 .. correctLabels.Length-1 do
                    if correctLabels.[i] = label1 && clusteredLabels.[i] = label2 then correct <- correct + 1
                correct
            float compare

        let compareall (f1: int[]) (f2: int[]) =
            let f1Distinct = f1 |> Array.distinct
            let f2Distinct = f2 |> Array.distinct
            f1Distinct
            |> Array.map (fun label1 -> 
                f2Distinct
                |> Array.map (fun label2 -> 
                    f f1 f2 label1 label2
                )
            )

        let contingencyMatrix: Matrix<float> = compareall correctLabels clusteredLabels |> matrix
                
        let rowSum =
            contingencyMatrix
            |> Matrix.Generic.mapRows Seq.sum
            |> Vector.toArray

        let colSum =
            contingencyMatrix
            |> Matrix.Generic.mapCols Seq.sum
            |> RowVector.toArray

        let totalSum =
            contingencyMatrix
            |> Matrix.Generic.sum

        let pi = Array.map (fun i -> i / totalSum) rowSum

        let pj = Array.map (fun j -> j / totalSum) colSum

        let mutualInfo = 
            [|for i in 0 .. rowSum.Length-1 do
                for j in 0 .. colSum.Length-1 do
                    let pxy = contingencyMatrix.[i, j] / totalSum
                    if pxy > 0.0 then yield pxy * log(pxy / (pi.[i] * pj.[j]))
                    else yield 0.0 
            |]
            |> Array.sum

        let normalizedMutualInformation =
            let entropyTrue = 
                Array.sumBy (fun p -> if p = 0.0 then 0.0 else -p * log(p)) pi
            
            let entropyPredicted = 
                Array.sumBy (fun p -> if p = 0.0 then 0.0 else -p * log(p)) pj
            
            2.0 * mutualInfo / (entropyTrue + entropyPredicted)

        normalizedMutualInformation


module GapStatistics = 
    
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
        RefDispersionStDev : float;
        Gaps                : float;
        }

    //Creates a gap statistic result.
    let createGapStatisticResult clusterIndex dispersion referenceDispersion refDispersionStDev gaps =                
        { ClusterIndex = clusterIndex; Dispersion = dispersion; ReferenceDispersion = referenceDispersion;  RefDispersionStDev = refDispersionStDev; Gaps = gaps }


    /// Not used.
    // Calculates dispersion of data clustered into k clusters.
    let dispersion (calcDispersion:GenericClusterDispersion<'a>) (data:array<'a>) k =          
        calcDispersion data k

    // Calculate dispersion of reference data' clustered into k clusters.
    let referenceDispersion (rndPointGenerator:GenericPointGenerator<'a>) (calcDispersion:GenericClusterDispersion<'a>) (data:array<'a>) (bootstraps:int) k = 
        let referenceDataSets = Array.init bootstraps (fun x -> rndPointGenerator data)
        [|1..bootstraps|]
        |> PSeq.map (fun bIndex -> 
            referenceDataSets.[bIndex-1]
            |> fun data -> calcDispersion data k )
        |> PSeq.toArray

    // Calculates gap statistic of a dataset for clusternumbers from 1 to maxK.
    let calculate (rndPointGenerator:GenericPointGenerator<'a>) (bootstraps:int) (calcDispersion:GenericClusterDispersion<'a>) maxK (data:array<'a>) =
        match maxK with
        | 0 -> failwith "maxK must be 1 or greater"
        | _ ->
                [|1..maxK|]
                //|> PSeq.map (fun k -> //Multi threading with System.Random number generator may eventually lead to zero-loop //Fix to do: referenceDataset generation once prior to the loop? 
                |> Array.map (fun k -> 
                    printfn "iteration k = %i" k
                    let refDisp     =  referenceDispersion rndPointGenerator calcDispersion data bootstraps k
                    let disp        =  calcDispersion data k    
                    
                    let refDispMean = refDisp  |> Seq.mean 
                    let refDispStd  = refDisp  |> Seq.stDev 
                    (createGapStatisticResult k disp refDispMean refDispStd (refDispMean-disp)) ) 
                //|> PSeq.toArray
                |> Array.sortBy (fun c -> c.ClusterIndex)


    module PointGenerators =
    
        /// <summary>Generate uniform points within the range of `data`.</summary>
        /// <remarks></remarks>
        /// <param name="rnd"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let generateUniformPoints (rnd:System.Random) =   
            fun (data:array<float[]>) -> 
                let min = matrix data |> Matrix.mapiCols (fun i x -> Seq.min x) |> Array.ofSeq
                let max = matrix data |> Matrix.mapiCols (fun i x -> Seq.max x) |> Array.ofSeq
                let range = Array.map2 (fun ma mi -> ma - mi) max min 
                
                let generateUniform () =
                    min
                    |> Array.mapi (fun i x ->  
                        x + (rnd.NextDouble() * range.[i]))
      
                Array.init data.Length (fun x -> generateUniform ())

        [<Obsolete("Use generateUniformPoints instead.")>]
        let generate_uniform_points (rnd:System.Random) = generateUniformPoints rnd
            
        // Generate uniform points for an appropriate reference distribution data' that takes the original data shape into account (from Tibshirani, Walther and Hastie 2001).
        let generateUniformPointsPCA (rnd:System.Random) =                    
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

        [<Obsolete("Use generateUniformPointsPCA instead.")>]
        let generate_uniform_points_PCA (rnd:System.Random) = generateUniformPointsPCA rnd

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
        let logDispersionKMeansInitRandom = 
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

        [<Obsolete("Use logDispersionKMeansInitRandom instead.")>]
        let logDispersionKMeans_initRandom = logDispersionKMeansInitRandom

        ////[Obsolete("Use [logDispersionKMeans_initCvMax] instead.")]
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
        let logDispersionKMeansInitCvMax = 
            let aggregator = IterativeClustering.avgCentroid
            let factory = IterativeClustering.initCVMAX 
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

        [<Obsolete("Use logDispersionKMeansInitCvMax instead.")>]
        let logDispersionKMeans_initCvMax = logDispersionKMeansInitCvMax

