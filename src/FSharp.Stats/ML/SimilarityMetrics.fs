namespace FSharp.Stats.ML

module SimilarityMetrics =

    open FSharp.Stats

    //  the Similarity between 2 observations 'a is a float
    //  It also better be positive - left to the implementer
    /// Signiture type for similarity functions
    type Similarity<'a> = 'a -> 'a -> float

    module Set =

        /// <summary>Computes the Jaccard index of two finite sets, also known as Intersection over Union.<br/><br/>The Jaccard coefficient measures similarity between finite sample sets, and is defined as the size of the intersection divided by the size of the union of the sample sets</summary>
        /// <remarks></remarks>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline jaccard (x : Set<'T>) (y : Set<'T>) =
            match  (x.Count, y.Count) with
            | (0,0) -> 1.
            | _     -> (Set.intersect x y |> Set.count |> float) / (Set.union x y |> Set.count |> float)

        /// <summary>Computes the overlap coefficient, or Szymkiewicz�Simpson coefficient<br/><br/>The Overlap coefficient measures the overlap between two finite sets. It is related to the Jaccard index and is defined as the size of the intersection divided by the smaller of the size of the two sets.<br/><br/>If set X is a subset of Y or the converse then the overlap coefficient is equal to 1.</summary>
        /// <remarks></remarks>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline overlap (x : Set<'T>) (y : Set<'T>) =
            match  (x.Count, y.Count) with
            | (0,0)             -> 1.
            | (_,0) | (0,_)     -> 0.
            | (xCount,yCount)   -> (Set.intersect x y |> Set.count |> float) / (min xCount yCount |> float)

        /// <summary>Computes the Sorensen�Dice coefficient similarity measure for two finite sets<br/><br/>ATTENTION: The Sorensen�Dice coefficient doesn't satisfy the triangle inequality. The corresponding difference function (1 - sorensenDice) is not a proper distance measure.</summary>
        /// <remarks></remarks>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline sorensenDice (x : Set<'T>) (y : Set<'T>) =
            match  (x.Count, y.Count) with
            | (0,0) -> 1.
            | (xCount,yCount) -> (2. * (Set.intersect x y |> Set.count |> float)) / ((xCount + yCount) |> float)

        /// <summary>Computes the Tversky index, an asymmetric similarity measure on sets that compares a variant to a prototype.<br/>The Tversky index can be seen as a generalization of Sorencsen-Dice coefficient and Jaccard index.</summary>
        /// <remarks>ATTENTION: this is an asymmetric similarity measure. Use tverskySymmetric if symmetry is needed.</remarks>
        /// <param name="prototypeWeight"></param>
        /// <param name="variantWeight"></param>
        /// <param name="prototype"></param>
        /// <param name="variant"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline tversky (prototypeWeight) (variantWeight) (prototype: Set<'T>) (variant: Set<'T>) =
            match (prototype.Count, variant.Count,prototypeWeight,variantWeight) with
            | (_,_,1.,1.) | (0,0,_,_)     -> 1.
            | _ -> 
                let intersectCount           = Set.intersect prototype variant  |> Set.count |> float
                let differencePrototypeCount = Set.difference prototype variant |> Set.count |> float
                let differenceVariantCount   = Set.difference variant prototype |> Set.count |> float
                intersectCount / (intersectCount + (prototypeWeight * differencePrototypeCount) + (variantWeight * differenceVariantCount))

        /// <summary>Computes the symmetric variant of the Tversky index. https://www.aclweb.org/anthology/S13-1028</summary>
        /// <remarks></remarks>
        /// <param name="prototypeWeight"></param>
        /// <param name="variantWeight"></param>
        /// <param name="prototype"></param>
        /// <param name="variant"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline tverskySymmetric (prototypeWeight) (variantWeight) (prototype: Set<'T>) (variant: Set<'T>) =
            match (prototype.Count, variant.Count,prototypeWeight,variantWeight) with
            | (_,_,1.,1.) | (0,0,_,_)     -> 1.
            | _ -> 
                let intersectCount          = Set.intersect prototype variant  |> Set.count |> float
                let differencePrototypeCount= Set.difference prototype variant |> Set.count |> float
                let differenceVariantCount  = Set.difference variant prototype |> Set.count |> float
                let minDifference           = min differencePrototypeCount differenceVariantCount
                let maxDifference           = max differencePrototypeCount differenceVariantCount
                intersectCount / 
                    (intersectCount + 
                        (variantWeight*((prototypeWeight*minDifference)+((1. - prototypeWeight) * maxDifference)))
                    )
