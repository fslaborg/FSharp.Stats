namespace FSharp.Stats.ML.Unsupervised



module IterativeClustering =
    
    open FSharp.Stats
    open FSharp.Stats.ML
    open DistanceMetrics    
    


    // CentroidsFactory, given a dataset, 
    // should generate n Centroids
    type CentroidsFactory<'a> = 'a array -> int -> 'a array
    // Given a Centroid and observations in a Cluster, create an updated Centroid
    type ToCentroid<'a> = 'a -> 'a array -> 'a


    /// Result of a kmeans clustering
    type KClusteringResult<'a> = {
        /// Centroids with index and data
        Centroids        : (int * 'a) array
        /// Classifier function returns cluster index and data point
        Classifier       : 'a -> int * 'a        
        /// Indices and Distances to closest centroid
        ClosestDistances : (int * float) array
        /// Used distance metric
        DistanceMetric   : DistanceMetrics.Distance<'a>
        }


    /// Creates a k-clustering  result
    let createKClusteringResult centroids classifier closestDistances distanceMetric =
        {Centroids = centroids; Classifier = classifier; ClosestDistances = closestDistances; DistanceMetric = distanceMetric;}


    // Returns the index of and distance to the 
    // Centroid closest to observation
    let private closest (dist: Distance<'a>) centroids (obs: 'a) =
        centroids
        |> Array.mapi (fun i c -> (i, dist c obs)) 
        |> Array.minBy (fun (i, d) -> d)

    
    // Picks k random observations as initial centroids
    let randomCentroids<'a> (rng: System.Random) 
                            (sample: 'a array) 
                            k =
        Array.sampleWithOutReplacement rng sample k



    // cvmax - Algorith by Moth’d Belal. Al-Daoud (Ref.: A New Algorithm for Cluster Initialization)
    let intitCVMAX (sample: float[] array) k =
        let dmatrix = matrix sample
        let cvmax =
            dmatrix
            |> Matrix.Generic.enumerateColumnWise Seq.var
            |> Seq.zip (Matrix.Generic.enumerateColumnWise id dmatrix)
            |> Seq.maxBy snd
            |> fst
            |> Seq.mapi (fun rowI value -> (rowI,value)) 
            |> Seq.toArray 
            |> Array.sortBy snd
                    
        if cvmax.Length < k then failwithf "Number of data points must be at least %i" k        
//        //
//        let intDivide a b = 
//            int (System.Math.Floor((float a) / (float b)))
    
        let chunkSize = cvmax.Length / k
        let midChunk  = chunkSize / 2
        [ for i=1 to k do
            let index = 
                match (chunkSize * i) with
                | x when x < cvmax.Length -> x - midChunk
                | x                       -> chunkSize * (i - 1) + ((cvmax.Length - chunkSize * (i - 1)) / 2)
            //printfn "Array.lenght = %i and index = %i" cvmax.Length (index-1)
            yield cvmax.[index-1] |> fst]
        |> Seq.map (fun rowI -> dmatrix.Row(rowI).ToArray())
        |> Seq.toArray

//    // cvmax - Algorith by Moth’d Belal. Al-Daoud (Ref.: A New Algorithm for Cluster Initialization)
//    let intitCVMAX (sample: float[] array) k =
//        let dmatrix = matrix sample
//        let dmatrix = DenseMatrix.ofRowArrays (sample |> Seq.toArray)
//        let cvmaxIndex = [| for (i,coli) in dmatrix.EnumerateColumnsIndexed() do
//                                yield (i,(StatisticalMeasure.stDevPopulation coli)) |] |> Array.maxBy (snd) |> fst
//        let cvmax = dmatrix.Column(cvmaxIndex) |> Seq.mapi (fun rowI value -> (rowI,value)) |> Seq.toArray |> Array.sortBy snd                 
        
//        if cvmax.Length < k then failwithf "Number of data points must be at least %i" k        
////        //
////        let intDivide a b = 
////            int (System.Math.Floor((float a) / (float b)))
    
//        let chunkSize = cvmax.Length / k
//        let midChunk  = chunkSize / 2
//        [ for i=1 to k do
//            let index = 
//                match (chunkSize * i) with
//                | x when x < cvmax.Length -> x - midChunk
//                | x                       -> chunkSize * (i - 1) + ((cvmax.Length - chunkSize * (i - 1)) / 2)
//            //printfn "Array.lenght = %i and index = %i" cvmax.Length (index-1)
//            yield cvmax.[index-1] |> fst]
//        |> Seq.map (fun rowI -> dmatrix.Row(rowI).ToArray())
//        |> Seq.toArray
    

    // Recompute Centroid as average of given sample (for kmeans)
    let avgCentroid (current: float []) (sample: float [] array) =
        let size = sample.Length
        match size with
        | 0 -> current
        | _ ->
            sample
            |> Array.reduce (fun v1 v2 -> 
                   Array.map2 (fun v1x v2x -> v1x + v2x) v1 v2)
            |> Array.map (fun e -> e / (float)size)


    // Given a distance, centroid factory and
    // centroid aggregation function, identify
    // the k centroids of a dataset
    let compute (dist: Distance<'a>) 
               (factory: CentroidsFactory<'a>) 
               (aggregator: ToCentroid<'a>)
               (dataset: 'a array) 
               k =
        // Recursively update Centroids and
        // the assignment of observations to Centroids
        let rec update (centroids, assignment) =
            // Assign each point to the closest centroid
            let next = 
                dataset 
                |> Array.map (fun obs -> closest dist centroids obs)
                //|> Seq.toList
            // Check if any assignment changed
            let change =
                match assignment with
                | Some(previous) -> 
                    Array.zip previous next    
                    |> Array.exists (fun ((i, _), (j, _)) -> not (i = j))
                | None -> true // initially we have no assignment
            if change 
            then 
                // Update each Centroid position:
                // extract cluster of points assigned to each Centroid
                // and compute the new Centroid by aggregating cluster
                let updatedCentroids =
                    let assignedDataset = Array.zip dataset next
                    centroids 
                    |> Array.mapi (fun i centroid -> 
                        assignedDataset 
                        |> Array.filter (fun (_, (ci, _)) -> ci = i)
                        |> Array.map (fun (obs, _) -> obs)
                        |> aggregator centroid)
                // Perform another round of updates
                update (updatedCentroids, Some(next))
            // No assignment changed, we are done
            else (centroids, next)

        let initialCentroids = factory dataset k
//        let centroids = update (initialCentroids, None) |> fst |> Seq.toList        
//        let classifier = fun datapoint -> 
//            centroids 
//            |> List.minBy (fun centroid -> dist centroid datapoint)        
        let centroids,closestDistances = update (initialCentroids, None)        
        let lCentroids = Seq.zip [1..k] centroids |> Seq.toArray
        let classifier = fun datapoint -> 
            lCentroids 
            |> Array.minBy (fun centroid -> dist (snd centroid) datapoint)
        createKClusteringResult lCentroids classifier closestDistances dist        

    
    let kmeans (dist: Distance<float []>) 
               (factory: CentroidsFactory<float []>)                
               (dataset: float [] array) 
               k =
        compute dist factory avgCentroid dataset k

    // Returns the closest cluster centroid to an input point.
    let nearest (dist: Distance<'a>) (lCentroids:array<'a>) (datapoint:'a) =
        lCentroids
        |> Array.mapi  (fun i centroid -> (i,centroid)) 
        |> Array.minBy (fun centroid -> dist (snd centroid) datapoint)         


    /// Calculates the distance from the data point to the centroid 
    let nearestDistance (dist: Distance<'a>) (lCentroids:array<'a>) (datapoint:'a) =
        lCentroids
        |> Array.map (fun centroid -> dist (centroid) datapoint)
        |> Array.min
 
    
    
    /// Calculates the average squared distance from the data points
    /// to the cluster centroid (also refered to as error)
    let Dispersion (dist: Distance<'a>) (lCentroids:list<'a>) (dataset: 'a seq) =        
        let classifier = fun datapoint -> 
            lCentroids 
            |> List.minBy (fun centroid -> dist centroid datapoint)        
        dataset
        |> Seq.map     (fun data -> dist (classifier data) data)
        |> Seq.averageBy (fun x -> x * x)


    /// Calculates the average squared distance from the data points
    /// to the cluster centroid (also refered to as error)
    let DispersionOfClusterResult (kmeansResult:KClusteringResult<'a>) =                
        kmeansResult.ClosestDistances
        |> Seq.averageBy (fun (index,dist) -> dist * dist)
