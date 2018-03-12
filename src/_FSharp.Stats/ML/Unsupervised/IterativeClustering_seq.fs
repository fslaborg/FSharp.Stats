namespace BioFSharp.Stats.ML.Unsupervised



module IterativeClustering =
    
    open BioFSharp.Stats
    open BioFSharp.Stats.ML
    open DistanceMetrics    
    
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.LinearAlgebra.Double


    // CentroidsFactory, given a dataset, 
    // should generate n Centroids
    type CentroidsFactory<'a> = 'a seq -> int -> 'a seq
    // Given a Centroid and observations in a Cluster, create an updated Centroid
    type ToCentroid<'a> = 'a -> 'a seq -> 'a


    /// Result of a kmeans clustering
    type KClusteringResult<'a> = {
        /// Centroids with index and data
        Centroids        : (int * 'a) list
        /// Classifier function returns cluster index and data point
        Classifier       : 'a -> int * 'a        
        /// Indices and Distances to closest centroid
        ClosestDistances : (int * float) list
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
        |> Seq.mapi (fun i c -> (i, dist c obs)) 
        |> Seq.minBy (fun (i, d) -> d)

    
    // Picks k random observations as initial centroids
    // (this is very lazy, even tolerates duplicates)
    let randomCentroids<'a> (rng: System.Random) 
                            (sample: 'a seq) 
                            k =
        let size = Seq.length sample
        seq { for i in 1 .. k do 
              let pick = Seq.nth (rng.Next(size)) sample
              yield pick }


    // cvmax - Algorith by Moth’d Belal. Al-Daoud (Ref.: A New Algorithm for Cluster Initialization)
    let intitCVMAX (sample: float[] seq) 
                    k =
        let dmatrix = DenseMatrix.ofRowArrays (sample |> Seq.toArray)
        let cvmaxIndex = [| for (i,coli) in dmatrix.EnumerateColumnsIndexed() do
                                yield (i,(StatisticalMeasure.stDevPopulation coli)) |] |> Array.maxBy (snd) |> fst
        let cvmax = dmatrix.Column(cvmaxIndex) |> Seq.mapi (fun rowI value -> (rowI,value)) |> Seq.toArray |> Array.sortBy snd                 
        
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
    

    // Recompute Centroid as average of given sample (for kmeans)
    let avgCentroid (current: float []) (sample: float [] seq) =
        let size = Seq.length sample
        match size with
        | 0 -> current
        | _ ->
            sample
            |> Seq.reduce (fun v1 v2 -> 
                   Array.map2 (fun v1x v2x -> v1x + v2x) v1 v2)
            |> Array.map (fun e -> e / (float)size)


    // Given a distance, centroid factory and
    // centroid aggregation function, identify
    // the k centroids of a dataset
    let compute (dist: Distance<'a>) 
               (factory: CentroidsFactory<'a>) 
               (aggregator: ToCentroid<'a>)
               (dataset: 'a seq) 
               k =
        // Recursively update Centroids and
        // the assignment of observations to Centroids
        let rec update (centroids, assignment) =
            // Assign each point to the closest centroid
            let next = 
                dataset 
                |> Seq.map (fun obs -> closest dist centroids obs)
                |> Seq.toList
            // Check if any assignment changed
            let change =
                match assignment with
                | Some(previous) -> 
                    Seq.zip previous next    
                    |> Seq.exists (fun ((i, _), (j, _)) -> not (i = j))
                | None -> true // initially we have no assignment
            if change 
            then 
                // Update each Centroid position:
                // extract cluster of points assigned to each Centroid
                // and compute the new Centroid by aggregating cluster
                let updatedCentroids =
                    let assignedDataset = Seq.zip dataset next
                    centroids 
                    |> Seq.mapi (fun i centroid -> 
                        assignedDataset 
                        |> Seq.filter (fun (_, (ci, _)) -> ci = i)
                        |> Seq.map (fun (obs, _) -> obs)
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
        let lCentroids = Seq.zip [1..k] centroids |> Seq.toList
        let classifier = fun datapoint -> 
            lCentroids 
            |> List.minBy (fun centroid -> dist (snd centroid) datapoint)
        createKClusteringResult lCentroids classifier closestDistances dist        

    
    let kmeans (dist: Distance<float []>) 
               (factory: CentroidsFactory<float []>)                
               (dataset: float [] seq) 
               k =
        compute dist factory avgCentroid dataset k

    // Returns the closest cluster centroid to an input point.
    let nearest (dist: Distance<'a>) (lCentroids:list<'a>) (datapoint:'a) =
        lCentroids
        |> List.mapi  (fun i centroid -> (i,centroid)) 
        |> List.minBy (fun centroid -> dist (snd centroid) datapoint)         


    /// Calculates the distance from the data point to the centroid 
    let nearestDistance (dist: Distance<'a>) (lCentroids:list<'a>) (datapoint:'a) =
        lCentroids
        |> List.map (fun centroid -> dist (centroid) datapoint)
        |> List.min
 
    
    
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
