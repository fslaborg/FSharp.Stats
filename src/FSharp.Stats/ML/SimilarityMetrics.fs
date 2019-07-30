namespace FSharp.Stats.ML

module SimilarityMetrics =

    open FSharp.Stats

    //  the Similarity between 2 observations 'a is a float
    //  It also better be positive - left to the implementer
    /// Signiture type for similarity functions
    type Similarity<'a> = 'a -> 'a -> float

    module Set =

        ///Computes the Jaccard index of two finite sets, also known as Intersection over Union. 
        ///
        ///The Jaccard coefficient measures similarity between finite sample sets, 
        ///and is defined as the size of the intersection divided by the size of the union of the sample sets
        let inline jaccard (x : Set<'T>) (y : Set<'T>) =
            match  (x.Count, y.Count) with
            | (0,0) -> 1.
            | _ -> (Set.intersect x y |> Set.count |> float) / (Set.union x y |> Set.count |> float)

        ///Computes the overlap coefficient, or Szymkiewicz–Simpson coefficient, 
        ///
        ///The Overlap coefficient measures the overlap between two finite sets. It is related to the Jaccard index and is 
        ///defined as the size of the intersection divided by the smaller of the size of the two sets.
        ///
        ///If set X is a subset of Y or the converse then the overlap coefficient is equal to 1.
        let inline overlap (x : Set<'T>) (y : Set<'T>) =
            match  (x.Count, y.Count) with
            | (0,0) -> 1.
            | (xCount,yCount) -> (Set.intersect x y |> Set.count |> float) / (min xCount yCount |> float)

