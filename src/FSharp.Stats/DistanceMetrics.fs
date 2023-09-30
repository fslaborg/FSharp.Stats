namespace FSharp.Stats

///Functions for computing distances of elements or sets
module DistanceMetrics =

    open FSharp.Stats
    
    //  the Distance between 2 observations 'a is a float
    //  It also better be positive - left to the implementer
    /// Signiture type for distance functions
    type Distance<'a> = 'a -> 'a -> float

    module Vector = 
        
        /// <summary>Euclidean distance between 2 vectors</summary>
        /// <remarks></remarks>
        /// <param name="v1"></param>
        /// <param name="v2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline euclidean (v1:Vector<'a>) (v2:Vector<'a>) = 
            let dim = min v1.Length v2.Length
            let mutable dist = LanguagePrimitives.GenericZero< 'a > 
            for i in 0 .. (dim - 1) do
                let x = v1.[i] - v2.[i]
                dist <- dist + (x * x)
            float dist
            |> sqrt
        

        /// <summary>Squared Euclidean distance between 2 vectors</summary>
        /// <remarks></remarks>
        /// <param name="v1"></param>
        /// <param name="v2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline euclideanSquared (v1:Vector<'a>) (v2:Vector<'a>) = 
            let dim = min v1.Length v2.Length
            let mutable dist = LanguagePrimitives.GenericZero< 'a >
            for i in 0 .. (dim - 1) do
                let x = v1.[i] - v2.[i]
                dist <- dist + (x * x)
            float dist
        
        /// <summary>Euclidean distance between 2 vectors (ignores nan) </summary>
        /// <remarks></remarks>
        /// <param name="v1"></param>
        /// <param name="v2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let euclideanNaN (v1:Vector<float>) (v2:Vector<float>) = 
            let dim = min v1.Length v2.Length
            let mutable dist = 0.0 
            for i in 0 .. (dim - 1) do
                let x = v1.[i] - v2.[i]
                if not (nan.Equals (x)) then
                    dist <- dist + (x * x)
            sqrt dist 

        /// <summary>Cityblock distance of two vectors</summary>
        /// <remarks></remarks>
        /// <param name="v1"></param>
        /// <param name="v2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline cityblock (v1:Vector<'a>) (v2:Vector<'a>) = 
            let dim = min v1.Length v2.Length
            let mutable dist = LanguagePrimitives.GenericZero< 'a > 
            for i in 0 .. (dim - 1) do
                let x = 
                    if v1.[i] > v2.[i] then
                        v1.[i] - v2.[i] 
                    else v2.[i] - v1.[i]                  
                dist <- dist + x
            float dist

        /// <summary>Cityblock distance of two vectors</summary>
        /// <remarks></remarks>
        /// <param name="v1"></param>
        /// <param name="v2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline cityblockNaN (v1:Vector<float>) (v2:Vector<float>) = 
            let dim = min v1.Length v2.Length
            let mutable dist = 0.
            for i in 0 .. (dim - 1) do
                let x = v1.[i] - v2.[i]
                if not (isNan x) then
                    dist <- dist + System.Math.Abs x
            dist
 
    module Array = 
        
        /// <summary>Euclidean distance of two coordinate arrays</summary>
        /// <remarks></remarks>
        /// <param name="a1"></param>
        /// <param name="a2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline euclidean (a1:array<'a>) (a2:array<'a>) = 
            let dim = min a1.Length a2.Length
            let mutable dist = LanguagePrimitives.GenericZero< 'a > 
            for i in 0 .. (dim - 1) do
                let x = a1.[i] - a2.[i]
                dist <- dist + (x * x)
            float dist
            |> sqrt         
        
        /// <summary>Euclidean distance of two coordinate float arrays (ignores nan)</summary>
        /// <remarks></remarks>
        /// <param name="a1"></param>
        /// <param name="a2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let euclideanNaN (a1:array<float>) (a2:array<float>) =                     
            let dim = min a1.Length a2.Length
            let mutable dist = 0.0 
            for i in 0 .. (dim - 1) do
                let x = a1.[i] - a2.[i]
                if not (nan.Equals (x)) then
                    dist <- dist + (x * x)
            sqrt dist 

        /// <summary>Squared Euclidean distance of two coordinate float arrays (ignores nan)</summary>
        /// <remarks></remarks>
        /// <param name="a1"></param>
        /// <param name="a2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let euclideanNaNSquared (a1:array<float>) (a2:array<float>) =               
            let dim = min a1.Length a2.Length
            let mutable dist = 0.0
            for i in 0 .. (dim - 1) do
                let x = a1.[i] - a2.[i]
                if not (isNan x) then
                    dist <- dist + (x * x)
            float dist

        /// <summary>Cityblock distance of two coordinate arrays</summary>
        /// <remarks></remarks>
        /// <param name="a1"></param>
        /// <param name="a2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline cityblock (a1:array<'a>) (a2:array<'a>) = 
            let dim = min a1.Length a2.Length
            let mutable dist = LanguagePrimitives.GenericZero< 'a > 
            for i in 0 .. (dim - 1) do
                let x = 
                    if a1.[i] > a2.[i] then
                        a1.[i] - a2.[i] 
                    else a2.[i] - a1.[i]                  
                dist <- dist + x
            float dist

        /// <summary>Cityblock distance of two coordinate float arrays (ignores nan)</summary>
        /// <remarks></remarks>
        /// <param name="a1"></param>
        /// <param name="a2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let cityblockNaN (a1:array<float>) (a2:array<float>) = 
            let dim = min a1.Length a2.Length
            let mutable dist = 0.0
            for i in 0 .. (dim - 1) do 
                let x = a1.[i] - a2.[i]
                if not (isNan x) then
                    dist <- dist + System.Math.Abs x
            dist

        

    /// <summary>Euclidean distance of two coordinate sequences</summary>
    /// <remarks></remarks>
    /// <param name="s1"></param>
    /// <param name="s2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline euclidean (s1:seq<'a>) (s2:seq<'a>) = 
        Seq.zip s1 s2
        |> Seq.fold (fun acc (c1,c2) -> 
                            let dC = c1 - c2
                            acc + (dC * dC)) LanguagePrimitives.GenericZero< 'a > 
        |> sqrt
       
        
    /// <summary>Euclidean distance of two coordinate float sequences (ignores nan)</summary>
    /// <remarks></remarks>
    /// <param name="s1"></param>
    /// <param name="s2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let euclideanNaN (s1:seq<float>) (s2:seq<float>) =    
        Seq.zip s1 s2
        |> Seq.fold (fun acc (c1,c2) -> 
                            let dC = c1 - c2
                            if not (nan.Equals dC) then 
                                acc + (dC * dC)
                            else acc    
                    ) 0.
                               
        |> sqrt
            
    /// <summary>Squared Euclidean distance of two coordinate float sequences (ignores nan)</summary>
    /// <remarks></remarks>
    /// <param name="s1"></param>
    /// <param name="s2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let euclideanNaNSquared (s1:seq<float>) (s2:seq<float>) =            
        Seq.zip s1 s2
        |> Seq.fold (fun acc (c1,c2) -> 
                            let dC = c1 - c2
                            if not (nan.Equals dC) then 
                                acc + (dC * dC)
                            else acc    
                    ) 0.
                               
    /// <summary>Cityblock distance of two coordinate float sequences</summary>
    /// <remarks></remarks>
    /// <param name="s1"></param>
    /// <param name="s2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline cityblock (s1:seq<'a>) (s2:seq<'a>) = 
        Seq.zip s1 s2
        |> Seq.fold (fun acc (c1,c2) ->
                        if c1 < c2 then 
                            acc + (c2 - c1)
                        else acc + (c1 - c2)            
                    ) LanguagePrimitives.GenericZero< 'a > 
        |> float

    /// <summary>Cityblock distance of two coordinate float sequences</summary>
    /// <remarks></remarks>
    /// <param name="s1"></param>
    /// <param name="s2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let cityblockNaN (s1:seq<float>) (s2:seq<float>) = 
        Seq.zip s1 s2
        |> Seq.fold (fun acc (c1,c2) ->
                        let dC = c1 - c2 |> System.Math.Abs
                        if not (nan.Equals dC) then
                            acc + dC
                        else acc
                    ) 0.

    /// "Dissimilarity" uses 1. - pearsons correlation coefficient 
    let inline dissimilarity v1 v2 =
        1. - Correlation.Seq.pearson v1 v2


    // Levenshtein distance between strings, lifted from:
    // http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Levenshtein_distance#F.23
    let private min3 one two three :int = 
        if one < two && one < three then one
        elif two < three then two
        else three

    /// <summary>Levenshtein distance between</summary>
    /// <remarks></remarks>
    /// <param name="s"></param>
    /// <param name="t"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
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
//'euclideanSquared' Squared euclidean distance. If distance to check for is squared too, the computation is faster because of lacking square root calculation
//'seuclidean'	Standardized Euclidean distance. Each coordinate difference between X and a query point is scaled, meaning divided by a scale value S. The default value of S is the standard deviation computed from X, S=nanstd(X). To specify another value for S, use the Scale name-value pair.
//'mahalanobis'	Mahalanobis distance, computed using a positive definite covariance matrix C. The default value of C is the sample covariance matrix of X, as computed by nancov(X). To specify a different value for C, use the 'Cov' name-value pair.
//'cityblock'	City block distance. (Manhattan distance, taxi cap distance)
//'minkowski'	Minkowski distance. The default exponent is 2. To specify a different exponent, use the 'P' name-value pair.
//'chebychev'	Chebychev distance (maximum coordinate difference).
//'cosine'	One minus the cosine of the included angle between observations (treated as vectors).
//'correlation'	One minus the sample linear correlation between observations (treated as sequences of values).
//'hamming'	Hamming distance, percentage of coordinates that differ.
//'jaccard'	One minus the Jaccard coefficient, the percentage of nonzero coordinates that differ.
//'spearman'	One minus the sample Spearman's rank correlation between observations (treated as sequences of values).
