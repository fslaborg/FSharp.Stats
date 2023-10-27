namespace FSharp.Stats.ML.Unsupervised


type LabeledPoint<'a, 'l> = {
        p     : 'a
        label : 'l
    }

    with
        static member create(p, l) = {
            p     = p
            label = l
        }

[<RequireQualifiedAccess>]
module KNN =

    open FSharp.Stats.DistanceMetrics

    module Array =

        /// <summary>
        /// The [k-nearest neighbors algorithm](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) to classify a new data point into the target class, 
        /// depending on the features of its neighboring data points.
        /// </summary>
        /// <remarks>May mutate the order of `labeledPoints`.</remarks>
        /// <param name="distance">the distance function, e.g. `euclidean`</param>
        /// <param name="labeledPoints">the array of classified (or labeled) points, use for the classification</param>
        /// <param name="k">The number of nearest neighbors from x to look for.</param>
        /// <param name="x">The point to classify</param>
        /// <returns>The most common label from the k nearest neighbors for x.</returns>
        /// <example> 
        /// <code>
        /// let reds  = [| [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ] |] |> Array.map (fun p -> LabeledPoint<float list, string>.create(p, "red"))
        /// let blues = [| [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] |] |> Array.map (fun p -> LabeledPoint<float list, string>.create(p, "blue"))
        ///
        /// let labeledPoints = Array.append reds blues
        /// let prediction = FSharp.Stats.ML.Unsupervised.KNN.Array.predict FSharp.Stats.DistanceMetrics.euclidean labeledPoints 3
        ///
        /// let color = prediction [3.0; 3.0] // should be: Some "red"
        /// let color = prediction [6.0; 6.0] // should be: Some "blue"
        /// </code> 
        /// </example>
        let inline predict (distance : Distance<'a>) (labeledPoints: LabeledPoint<'a, 'l> array) (k : int) (x: 'a) : 'l option =
            if Array.isEmpty labeledPoints || k <= 0 then
                None
            else
                labeledPoints |> Array.sortInPlaceBy (fun lp -> distance lp.p x)

                let kNearestNeighbors = Array.take k labeledPoints

                let label =
                    kNearestNeighbors
                    |> Array.countBy (fun lp -> lp.label)
                    |> Array.maxBy snd
                    |> fst

                Some label

    module Seq =
    
        /// <summary>
        /// The [k-nearest neighbors algorithm](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) to classify a new data point into the target class, 
        /// depending on the features of its neighboring data points.
        /// </summary>
        /// <param name="distance">the distance function, e.g. `euclidean`</param>
        /// <param name="labeledPoints">the sequence of classified (or labeled) points, use for the classification</param>
        /// <param name="k">The number of nearest neighbors from x to look for.</param>
        /// <param name="x">The point to classify</param>
        /// <returns>The most common label from the k nearest neighbors for x.</returns>
        /// <example> 
        /// <code> 
        /// let points = seq { [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ]; [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] }        ///
        /// let labels = seq { "red"; "red"; "red"; "red"; "red"; "blue"; "blue"; "blue"; "blue"; "blue" }
        /// let prediction = FSharp.Stats.ML.Unsupervised.KNN.Seq.KNN.Seq.predict FSharp.Stats.DistanceMetrics.euclidean points labels 3
        ///
        /// let color = prediction [3.0; 3.0] // should be: Some "red"
        /// let color = prediction [6.0; 6.0] // should be: Some "blue"
        /// </code> 
        /// </example>
        let inline predict<'a, 'l when 'l: equality and 'l: comparison>
            (distance : Distance<'a>) 
            (points   : 'a seq)
            (labels   : 'l seq)
            (k        : int) 
            (x        : 'a) 
            : 'l option =

            if Seq.isEmpty points || Seq.length points <> Seq.length labels || k <= 0 then
                None
            else            
                let distanceIndices=
                    points
                    |> Seq.mapi (fun idx p -> idx, distance p x)
                
                let kNearestNeighborIndices =
                    distanceIndices
                    |> Seq.sortBy snd // snd = distance value
                    |> Seq.take k
                        
                let label =
                    kNearestNeighborIndices
                    |> Seq.countBy (fun (idx, _) -> Seq.item idx labels)
                    |> Seq.maxBy fst
                    |> fst

                Some label


    /// <summary>
    /// The [k-nearest neighbors algorithm](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) to classify new data points into their target classes, 
    /// depending on the features of their neighboring data points.
    /// </summary>
    /// <remarks>Convencience methods for using `KNN.Array.predict`, similiar to `KNeighborsClassifier` in `sklearn.neighbors`.</remarks>
    /// <param name="distance">the distance function, e.g. `euclidean`</param>
    /// <param name="k">The number of nearest neighbors from x to look for.</param>
    /// <returns>The most common label from the k nearest neighbors for x.</returns>
    /// <example> 
    /// <code>
    /// let reds  = [| [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ] |]
    /// let blues = [| [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] |]
    /// 
    /// let knnClassifier = KNN.Classifier(FSharp.Stats.DistanceMetrics.euclidean, 3)
    /// 
    /// // fit the classifier and predict new points
    /// // version 1.
    /// let labeledPoints = Map [ "blue", blues; "red", reds ]
    /// knnClassifier.fit(labeledPoints)
    /// let color  = knnClassifier.predict [3.0; 3.0] // should be: Some "red"
    /// let colors = knnClassifier.predict [| [3.0; 3.0]; [6.0; 6.0] |] // should be: [| Some "red", Some "blue" |]
    /// 
    /// // version 2.
    /// let points = Array.append reds blues
    /// let labels = [| "red"; "red"; "red"; "red"; "red"; "blue"; "blue"; "blue"; "blue"; "blue" |]
    /// knnClassifier.fit(points, labels)
    /// let color  = knnClassifier.predict [3.0; 3.0] // should be: Some "red"
    /// let colors = knnClassifier.predict [| [3.0; 3.0]; [6.0; 6.0] |] // should be: [| Some "red", Some "blue" |]
    /// </code> 
    /// </example>
    type Classifier<'a, 'l when 'l: equality and 'l: comparison>(distance: Distance<'a>, k: int) =

        [<DefaultValue>] val mutable labeledPoints : LabeledPoint<'a, 'l> array
        member val K = k with get, set

        member this.OverwriteK k =
            this.K <- k

        member this.fit(lps : LabeledPoint<'a, 'l> array) =
            this.labeledPoints <- lps

        member this.fit(points : 'a array, labels : 'l array) =
            let lps =
                (points, labels)
                ||> Array.zip
                |> Array.map LabeledPoint.create<'a, 'l>
            this.labeledPoints <- lps

        member this.fit(labeledPoints: Map<'l, 'a array>) =
            let lps =
                labeledPoints
                |> Seq.collect (fun (KeyValue(label, points)) ->
                    points |> Array.map (fun p -> LabeledPoint.create<'a, 'l>(p, label)))
            this.labeledPoints <- Seq.toArray lps

        member this.predict(x, ?overwriteK)  : 'l option =
            Array.predict distance this.labeledPoints (defaultArg overwriteK this.K) x

        member this.predict(points: 'a array, ?overwriteK)  =
           let predict = Array.predict distance this.labeledPoints (defaultArg overwriteK this.K)
           Array.map predict points
