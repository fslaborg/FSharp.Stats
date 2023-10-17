namespace FSharp.Stats.ML.Unsupervised


type LabeledPoint<'a, 'l> = {
        p     : 'a
        label : 'l
    }

    with
        static member create(p, l)= {
            p     = p
            label = l
        }

[<RequireQualifiedAccess>]
module KNN =

    open FSharp.Stats.DistanceMetrics

    module Array =

        /// <summary>TODO.</summary>
        /// <remarks>May mutate the order of `labeledPoints` and is not thread safe.</remarks>
        /// <param name="distance">the distance function, e.g. `euclidean`</param>
        /// <param name="labeledPoints">second vector</param>
        /// <param name="k">The number of nearest neighbors to look from x</param>
        /// <param name="x">The point to classify</param>
        /// <returns>The most common labels from the k nearest neighbors for x.</returns>
        /// <example> 
        /// <code> 
        /// TODO
        /// </code> 
        /// </example>
        let inline predict (distance : Distance<'a>) (labeledPoints: LabeledPoint<'a, 'l> array) (k : int) (x: 'a) : 'l option =
            if Array.isEmpty labeledPoints || k <= 0 then
                None
            elif k = 1 then
                Some labeledPoints.[0].label
            else
                labeledPoints |> Array.sortInPlaceBy (fun lp -> distance lp.p x)

                let kNearestNeighbors = Array.take k labeledPoints

                let label =
                    kNearestNeighbors
                    |> Array.countBy (fun lp -> lp.label)
                    |> Array.maxBy snd
                    |> fst

                Some label

        let inline predictInRef<'l when 'l: equality and 'l: comparison>
            (distance : Distance<'a>) 
            (labeledPoints: inref<LabeledPoint<'a, 'l> array>)
            (k        : int) 
            (x        : 'a) 
            : 'l option =

            if Array.isEmpty labeledPoints || k <= 0 then
                None
            elif k = 1 then
                Some labeledPoints.[0].label
            else

                let distanceIndices =
                    labeledPoints
                    |> Array.mapi (fun idx p -> idx, distance p.p x)
                
                let kNearestNeighborIndices =
                    distanceIndices
                    |> Array.sortBy snd // snd = distance value
                    |> Array.take k
                
                let labels = Array.zeroCreate k

                for i in 0..k do
                    let idx, _ = kNearestNeighborIndices.[i]
                    let label: 'l = labeledPoints.[idx].label
                    labels.[i] <- label

                let label =
                    labels
                    |> Seq.countBy id
                    |> Seq.maxBy fst
                    |> fst

                Some label



    module Seq =
    
        let inline predict<'l when 'l: equality and 'l: comparison>
            (distance : Distance<'a>) 
            (points   : 'a seq)
            (labels   : 'l seq)
            (k        : int) 
            (x        : 'a) 
            : 'l option =

            if Seq.isEmpty points || Seq.length points <> Seq.length labels || k <= 0 then
                None
            elif k = 1 then
                Some (Seq.head labels)
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

            // let inline predict<'l when 'l: equality and 'l: comparison>
            //     (distance      : Distance<'a>) 
            //     (labeledPoints : LabeledPoint<'a, 'l> seq)
            //     (k             : int) 
            //     (x             : 'a) 
            //     : 'l option =

            //     if Seq.isEmpty labeledPoints || k <= 0 then
            //         None
            //     elif k = 1 then
            //         Some (Seq.head labeledPoints).label
            //     else

            //         let distanceIndices =
            //             labeledPoints
            //             |> Seq.map (fun p -> p, distance p.p x)
                    
            //         let kNearestNeighborIndices =
            //             distanceIndices
            //             |> Seq.sortBy snd // snd = distance value
            //             |> Seq.take k
                    
            //         let label =
            //             kNearestNeighborIndices
            //             |> Seq.countBy (fun (p, _) -> p.label)
            //             |> Seq.maxBy fst
            //             |> fst

            //         Some label



    /// Python Style KNeighborsClassifier
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
