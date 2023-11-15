namespace FSharp.Stats.ML.Unsupervised


[<RequireQualifiedAccess>]
module KNN =

    open FSharp.Stats.DistanceMetrics

    module Array =

        /// <summary>
        /// The [k-nearest neighbors algorithm (KNN)](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) to classify
        /// a new data point into the target class, depending on the features of its neighboring data points.
        /// </summary>
        /// <remarks>May mutate the order of `labeledPoints`.</remarks>
        /// <param name="distance">the distance function, e.g. `euclidean`</param>
        /// <param name="labeledPoints">the array of classified (or labeled) points [in the format (point, label)],
        /// used for the classification</param>
        /// <param name="k">The _positive_ number of nearest neighbors from x to look for.</param>
        /// <param name="x">The point to classify</param>
        /// <returns>The most common label from the k nearest neighbors for x.</returns>
        /// <example> 
        /// <code>
        /// let reds  = [| [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ] |] |> Array.map (fun p -> (p, "red"))
        /// let blues = [| [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] |] |> Array.map (fun p -> (p, "blue"))
        ///
        /// let labeledPoints = Array.append reds blues
        /// let prediction = FSharp.Stats.ML.Unsupervised.KNN.Array.predict FSharp.Stats.DistanceMetrics.euclidean labeledPoints 3
        ///
        /// let color = prediction [3.0; 3.0] // should be: Some "red"
        /// let color = prediction [6.0; 6.0] // should be: Some "blue"
        /// </code> 
        /// </example>
        let inline predict (distance : Distance<'a>) (labeledPoints: ('a * 'l) array) (k : int) (x: 'a) : 'l option =
            if Array.isEmpty labeledPoints || k <= 0 then
                None
            else
                labeledPoints |> Array.sortInPlaceBy (fun (point, _) -> distance point x)

                let kNearestNeighbors = Array.take k labeledPoints

                let label =
                    kNearestNeighbors
                    |> Array.countBy (fun (_, label) -> label)
                    |> Array.maxBy snd
                    |> fst

                Some label

    module Seq =

        /// <summary>
        /// The [k-nearest neighbors algorithm](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) to classify
        /// a new data point into the target class, depending on the features of its neighboring data points.
        /// </summary>
        /// <param name="distance">the distance function, e.g. `euclidean`</param>
        /// <param name="labeledPoints">the array of classified (or labeled) points [in the format (point, label)],
        /// used for the classification</param>
        /// <param name="k">The _positive_ number of nearest neighbors from x to look for.</param>
        /// <param name="x">The point to classify</param>
        /// <returns>The most common label from the k nearest neighbors for x.</returns>
        /// <example> 
        /// <code>
        /// let points = seq { [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ]; [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] }
        /// let labels = seq { "red"; "red"; "red"; "red"; "red"; "blue"; "blue"; "blue"; "blue"; "blue" }
        /// let labeledPoints = Seq.zip points labels
        /// let prediction = FSharp.Stats.ML.Unsupervised.KNN.Seq.KNN.Seq.predict FSharp.Stats.DistanceMetrics.euclidean labeledPoints 3
        ///
        /// let color = prediction [3.0; 3.0] // should be: Some "red"
        /// let color = prediction [6.0; 6.0] // should be: Some "blue"
        /// </code> 
        /// </example>
        let inline predict<'a, 'l when 'l: equality and 'l: comparison>
            (distance : Distance<'a>)
            (labeledPoints: ('a * 'l) seq)
            (k        : int)
            (x        : 'a)
            : 'l option =

            if Seq.isEmpty labeledPoints || k <= 0 then
                None
            else
                let distanceIndices=
                    labeledPoints
                    |> Seq.mapi (fun idx (p, _) -> idx, distance p x)

                let kNearestNeighborIndices =
                    distanceIndices
                    |> Seq.sortBy snd // snd = distance value
                    |> Seq.take k

                let label =
                    kNearestNeighborIndices
                    |> Seq.countBy (fun (idx, _) -> labeledPoints |> Seq.item idx |> snd)
                    |> Seq.maxBy fst
                    |> fst

                Some label


    /// <summary>
    /// The [k-nearest neighbors algorithm](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) to classify
    /// new data points into their target classes, depending on the features of their neighboring data points.
    /// </summary>
    /// <remarks>Convencience methods for using `KNN.Array.predict`, similiar to `KNeighborsClassifier` in `sklearn.neighbors`.</remarks>
    /// <param name="distance">the distance function, e.g. `euclidean`</param>
    /// <param name="k">The _positive_ number of nearest neighbors from x to look for.</param>
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
    /// let colors = knnClassifier.predict [| [3.0; 3.0]; [6.0; 6.0] |] // should be: [| Some "red"; Some "blue" |]
    /// 
    /// // version 2.
    /// let points = Array.append reds blues
    /// let labels = [| "red"; "red"; "red"; "red"; "red"; "blue"; "blue"; "blue"; "blue"; "blue" |]
    /// knnClassifier.fit(points, labels)
    /// let color  = knnClassifier.predict [3.0; 3.0] // should be: Some "red"
    /// let colors = knnClassifier.predict [| [3.0; 3.0]; [6.0; 6.0] |] // should be: [| Some "red"; Some "blue" |]
    /// </code> 
    /// </example>
    type Classifier<'a, 'l when 'l: equality and 'l: comparison>(distance: Distance<'a>, k: int) =
        do if k <= 0 then failwith "The paramter `k` must be positive"

        [<DefaultValue>] val mutable labeledPoints : ('a * 'l) array
        member val K = k with get, set

        /// <summary>
        /// Overwrite the parameter `k` from the constructor.
        /// </summary>
        /// <param name="k">The _positive_ number of nearest neighbors from x to look for.</param>
        member this.OverwriteK k =
            if k > 0 then
                this.K <- k
                Some(k)
            else
                None

        /// <summary>
        /// Fit the constructed `KNN.Classifier`, i.e. provide the points with their labels, used for the prediction.
        /// </summary>
        /// <param name="labeledPoints">the array of classified (or labeled) points [in the format (point, label)],
        /// used for the classification</param>
        /// <example> 
        /// <code>
        /// // .. construct the knnClassifier before ..
        /// let points = seq { [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ]; [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] }
        /// let labels = seq { "red"; "red"; "red"; "red"; "red"; "blue"; "blue"; "blue"; "blue"; "blue" }
        /// let labeledPoints = Seq.zip points labels
        /// knnClassifier.fit(labeledPoints)
        /// </code> 
        /// </example>
        member this.fit(labeledPoints : ('a * 'l) array) =
            this.labeledPoints <- labeledPoints

        /// <summary>
        /// Fit the constructed `KNN.Classifier`, i.e. provide the points with their labels, used for the prediction.
        /// </summary>
        /// <param name="points">the array of points</param>
        /// <param name="labels">the array of labels for the `points`</param>
        /// <remarks>Fails if `points` and `labels` do not have the same length.</remarks>
        /// <example> 
        /// <code>
        /// // .. construct the knnClassifier before ..
        /// let points = seq { [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ]; [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] }
        /// let labels = seq { "red"; "red"; "red"; "red"; "red"; "blue"; "blue"; "blue"; "blue"; "blue" }
        /// knnClassifier.fit(points, labels)
        /// </code> 
        /// </example>
        member this.fit(points : 'a array, labels : 'l array) =
            let lps = Array.zip points labels
            this.labeledPoints <- lps

        /// <summary>
        /// Fit the constructed `KNN.Classifier`, i.e. provide the points with their labels, used for the prediction.
        /// </summary>
        /// <param name="labeledPoints">the array of classified (or labeled) points [in the format "Map<label, point array>"],
        /// used for the classification</param>
        /// <example> 
        /// <code>
        /// // .. construct the knnClassifier before ..
        /// let reds  = [| [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ] |]
        /// let blues = [| [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] |]
        /// let labeledPoints = Map [ "blue", blues; "red", reds ]
        /// knnClassifier.fit(labeledPoints)
        /// </code> 
        /// </example>
        member this.fit(labeledPoints: Map<'l, 'a array>) =
            let lps =
                labeledPoints
                |> Seq.collect (fun (KeyValue(label, points)) ->
                    points |> Array.map (fun p -> (p, label)))
            this.labeledPoints <- Seq.toArray lps

        /// <summary>
        /// Predict (or classify) the given point.
        /// </summary>
        /// <param name="x">the point to be classified.</param>
        /// <example> 
        /// <code>
        /// // .. construct and fit the knnClassifier before ..
        /// knnClassifier.predict [3.0; 3.0] // should be: Some "red"
        /// </code> 
        /// </example>
        member this.predict(x, ?overwriteK)  : 'l option =
            let k = overwriteK |> Option.bind this.OverwriteK |> Option.defaultValue this.K
            Array.predict distance this.labeledPoints (defaultArg overwriteK k) x

        /// <summary>
        /// Predict (or classify) the given collection of points.
        /// </summary>
        /// <param name="points">the array of points to be classified.</param>
        /// <example> 
        /// <code>
        /// // .. construct and fit the knnClassifier before ..
        /// knnClassifier.predict [| [3.0; 3.0]; [6.0; 6.0] |] // should be: [| Some "red"; Some "blue" |]
        /// </code> 
        /// </example>
        member this.predict(points: 'a array, ?overwriteK)  =
            let k = overwriteK |> Option.bind this.OverwriteK |> Option.defaultValue this.K
            let predict = Array.predict distance this.labeledPoints (defaultArg overwriteK k)
            Array.map predict points
