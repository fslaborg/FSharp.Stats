namespace FSharp.Stats.ML


module DistanceMetrics =

    open FSharp.Stats
    open FSharp.Stats.Algebra
    open FSharp.Stats.Algebra.LinearAlgebra
    
    //  the Distance between 2 observations 'a is a float
    //  It also better be positive - left to the implementer
    /// Signiture type for distance functions
    type Distance<'a> = 'a -> 'a -> float

    module Vector = 
        
        /// Euclidean distance between 2 vectors
        let inline euclidean (v1:Vector<'a>) (v2:Vector<'a>) = 
            let dim = min v1.Length v2.Length
            let mutable dist = LanguagePrimitives.GenericZero< 'a > 
            for i in 0 .. (dim - 1) do
                let x = v1.[i] - v2.[i]
                dist <- dist + (x * x)
            float dist
            |> sqrt

        
        /// Euclidean distance between 2 vectors
        let inline euclideanNaN (v1:Vector<float>) (v2:Vector<float>) = 
            let dim = min v1.Length v2.Length
            let mutable dist = 0.0 
            for i in 0 .. (dim - 1) do
                let x = v1.[i] - v2.[i]
                if not (nan.Equals (x)) then
                    dist <- dist + (x * x)
            sqrt dist 
 

    /// Euclidean distance between 2 vectors
    let euclidean v1 v2 = 
        Seq.zip v1 v2
        |> Seq.fold (fun d (e1,e2) -> d + ((e1 - e2) * (e1 - e2))) 0.
        |> sqrt

    /// Euclidean distance between 2 vectors (ignores nan)
    let euclideanNaN v1 v2 = 
        Seq.zip v1 v2
        |> Seq.map (fun (e1, e2) -> (e1 - e2) * (e1 - e2))
        |> Seq.filter (fun x -> not(System.Double.IsNaN(x)))
        |> Seq.sum
        |> sqrt

    /// "Dissimilarity" uses 1. - pearsons correlation coefficient 
    let dissimilarity v1 v2 =
        1. - Correlation.pearson v1 v2


    // Levenshtein distance between strings, lifted from:
    // http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Levenshtein_distance#F.23
    let inline private min3 one two three = 
        if one < two && one < three then one
        elif two < three then two
        else three

    /// Levenshtein distance between
    let wagnerFischerLazy (s: string) (t: string) =
        let m = s.Length
        let n = t.Length
        let d = Array2D.create (m+1) (n+1) -1
        let rec dist =
            function
            | i, 0 -> i
            | 0, j -> j
            | i, j when d.[i,j] <> -1 -> d.[i,j]
            | i, j ->
                let dval = 
                    if s.[i-1] = t.[j-1] then dist (i-1, j-1)
                    else
                        min3
                            (dist (i-1, j)   + 1) // a deletion
                            (dist (i,   j-1) + 1) // an insertion
                            (dist (i-1, j-1) + 1) // a substitution
                d.[i, j] <- dval; dval 
        dist (m, n)




//Value	Description
//'euclidean'	Euclidean distance.
//'seuclidean'	Standardized Euclidean distance. Each coordinate difference between X and a query point is scaled, meaning divided by a scale value S. The default value of S is the standard deviation computed from X, S=nanstd(X). To specify another value for S, use the Scale name-value pair.
//'mahalanobis'	Mahalanobis distance, computed using a positive definite covariance matrix C. The default value of C is the sample covariance matrix of X, as computed by nancov(X). To specify a different value for C, use the 'Cov' name-value pair.
//'cityblock'	City block distance.
//'minkowski'	Minkowski distance. The default exponent is 2. To specify a different exponent, use the 'P' name-value pair.
//'chebychev'	Chebychev distance (maximum coordinate difference).
//'cosine'	One minus the cosine of the included angle between observations (treated as vectors).
//'correlation'	One minus the sample linear correlation between observations (treated as sequences of values).
//'hamming'	Hamming distance, percentage of coordinates that differ.
//'jaccard'	One minus the Jaccard coefficient, the percentage of nonzero coordinates that differ.
//'spearman'	One minus the sample Spearman's rank correlation between observations (treated as sequences of values).
