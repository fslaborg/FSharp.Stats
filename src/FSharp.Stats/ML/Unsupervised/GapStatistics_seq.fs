namespace  BioFSharp.Stats.ML.Unsupervised

open BioFSharp.Stats



//module GapStatistics =
//    open MathNet.Numerics
//    open MathNet.Numerics.LinearAlgebra      
//    
//    open BioFSharp.Stats.ML
//    open BioFSharp.Stats.Descriptive
//
//
//// An implementation of the gap statistic algorithm from Tibshirani, Walther, and Hastie's "Estimating the number of clusters in a data set via the gap statistic".
////
//// Given a row seq `data`, where rows are observations and columns are individual dimensions, compute and plot the gap statistic 
//// (according to a uniform reference distribution taking into account the original dataset's shape ).
// 
////A major disadvantage of elbow and average silhouette methods is that they measure a global clustering characteristic only. A more sophisticated method 
////is to use the gap statistic which provides a statistical procedure to formalize the elbow/silhouette heuristic in order to estimate the optimal number of clusters.
//
//
//
//    // GenericClusterDispersion, given a dataset (data<'a>) and clusternumber k,
//    // returns the cluster dispersion.      //after applying a cluster algorithm to the dataset.
//    type GenericClusterDispersion<'a> = seq<'a>  -> int -> float
//    
//    //GenericPointGenerator, given a dataset (data<'a>), 
//    //returns dataset' containing random values in range of the original dataset.
//    type GenericPointGenerator<'a> = seq<'a> -> seq<'a>
//    
//    //Represents a gap statistic result.
//    type GapStatisticResult = {
//        ClusterIndex        : int;
//        Dispersion          : float;
//        ReferenceDispersion : float;
//        RefDispersion_StDev : float;
//        Gaps                : float;
//        }
//
//    //Creates a gap statistic result.
//    let createGapStatisticResult clusterIndex dispersion referenceDispersion refDispersion_StDev gaps =                
//        { ClusterIndex = clusterIndex; Dispersion = dispersion; ReferenceDispersion = referenceDispersion;  RefDispersion_StDev = refDispersion_StDev; Gaps = gaps }
//
//
//    /// Not used.
//    // Calculates dispersion of data clustered into k clusters.
//    let dispersion (calcDispersion:GenericClusterDispersion<'a>) (data:seq<'a>) k =          
//        calcDispersion data k
//
//    // Calculate dispersion of reference data' clustered into k clusters.
//    let referenceDispersion (rndPointGenerator:GenericPointGenerator<'a>) (calcDispersion:GenericClusterDispersion<'a>) (data:seq<'a>) (bootstraps:int) k = 
//        [1..bootstraps]
//        |> Seq.map (fun bIndex ->
//            data
//            |> rndPointGenerator 
//            |> fun data -> calcDispersion data k )
//
//    // Calculates gap statistic of a dataset for clusternumbers from 1 to maxK.
//    let calculate (rndPointGenerator:GenericPointGenerator<'a>) (bootstraps:int) (calcDispersion:GenericClusterDispersion<'a>) maxK (data:seq<'a>) =
//        match maxK with
//        | 0 -> failwith "maxK must be 1 or greater."
//        | _ ->
//                [1..maxK]
//                |> Seq.map (fun k -> 
//                            let refDisp     =  referenceDispersion rndPointGenerator calcDispersion data bootstraps k 
//                            let disp        =  calcDispersion data k
//
//                            let refDispMean = refDisp  |> StatisticalMeasure.mean 
//                            let refDispStd  = refDisp  |> StatisticalMeasure.stDev 
//                            (createGapStatisticResult k disp refDispMean refDispStd (refDispMean-disp)) )        
//        
//
//    module PointGenerators =
//
//        // Generate uniform points within the range of `data`.
//        let generate_uniform_points (rnd:System.Random) =        
//            fun (data:seq<float[]>) -> 
//                let generateUniform (s:StatisticalMeasure.Range) =
//                   s.Min + (rnd.NextDouble() * (s.Max - s.Min))
//                                       
//                data
//                |> Seq.map (fun item -> StatisticalMeasure.range (item),item)
//                |> Seq.map (fun ((s:StatisticalMeasure.Range),data) -> 
//                    data 
//                    |> Array.map (fun _ -> generateUniform s))
//                      
//        // Generate uniform points for an appropriate reference distribution data' that takes the original datashape into account.
//        let generate_uniform_points_PCA (rnd:System.Random)  =                    
//            fun (data:seq<'a>) -> 
//                let generateUniform (s:StatisticalMeasure.Range) =
//                   s.Min + (rnd.NextDouble() * (s.Max - s.Min))
//
//                let matrix    = DenseMatrix.ofRowSeq data
//                let adjCenter = PCA.toAdjustStandardize matrix
//                let comp      = PCA.compute adjCenter matrix 
//                let pcaResult = PCA.transform adjCenter comp matrix
//                                       
//                let data' =          
//                    PCA.revert adjCenter comp  pcaResult 
//                    |> Matrix.toRowSeq 
//                    |> Seq.map (fun v -> v.ToArray()) 
//                   
//                data'
//                |> Seq.map (fun item -> StatisticalMeasure.range (item),item)
//                |> Seq.map (fun ((s:StatisticalMeasure.Range),data) -> 
//                    data 
//                    |> Array.map (fun _ -> generateUniform s))    
//
//    
//    module ClusterDispersionMetric =
//
//       // Calculate log(sum_i(within-cluster_i sum of squares around cluster_i mean)) of kmeans clustering result.
//        let logDispersionKMeans_initRandom = 
//            let rnd = System.Random()
//            let aggregator = IterativeClustering.avgCentroid
//            let factory = IterativeClustering.randomCentroids rnd 
//            (fun (data:seq<float[]>) (k:int) -> 
//                                    let kMeans = IterativeClustering.compute DistanceMetrics.euclideanNaN factory aggregator data k    
//                                    kMeans.ClosestDistances
//                                    |> Seq.averageBy (fun (ind,dist) -> let ldist = log dist
//                                                                        ldist*ldist) )
//
//        // Calculate log(sum_i(within-cluster_i sum of squares around cluster_i mean)) of kmeans clustering result.
//        let logDispersionKMeans_initCvMax = 
//            let aggregator = IterativeClustering.avgCentroid
//            let factory = IterativeClustering.intitCVMAX 
//            (fun (data:seq<float[]>) (k:int) -> 
//                                    let kMeans = IterativeClustering.compute DistanceMetrics.euclideanNaN factory aggregator data k    
//                                    kMeans.ClosestDistances
//                                    |> Seq.averageBy (fun (ind,dist) -> let ldist = log dist
//                                                                        ldist*ldist) )
//
//
//




