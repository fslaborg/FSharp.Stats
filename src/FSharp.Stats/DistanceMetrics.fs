namespace FSharp.Stats

///Functions for computing distances of elements or sets
module DistanceMetrics =

    open FSharp.Stats
    
    //  the Distance between 2 observations 'a is a float
    //  It also better be positive - left to the implementer
    /// Signiture type for distance functions
    type Distance<'a> = 'a -> 'a -> float

    module Vector =
        
        /// <summary>Calculates Hamming distance between 2 vectors</summary>
        /// <remarks>Note, distance between Nan and Nan is equal to 1</remarks>
        /// <param name="v1">first vector</param>
        /// <param name="v2">second vector</param>
        /// <returns>Hamming distance between elements of given vectors</returns>
        /// <example> 
        /// <code> 
        /// // e.g. v1 and v2 initialization
        /// let v1 = vector [1; 2; 3]
        /// let s2 = vector [9; 2; 3]
        /// 
        /// // Apply the hamming to v1 and v2
        /// Vector.hamming v1 v2
        /// </code> 
        /// </example>
        let inline hamming (v1: Vector<'a>) (v2: Vector<'a>) =
            let mutable dist = 0
            
            match v1.Length <> v2.Length with
            | true -> failwith "Inputs are not of equal length"
            | _    ->
                for i in 0 .. v1.Length - 1 do
                    if (v1[i] <> v2[i]) then dist <- dist + 1
            dist
        
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
                    dist <- dist + abs x
            dist

        /// <summary>The [Minkowski distance](https://en.wikipedia.org/wiki/Minkowski_distance) between two vectors of order `p`.</summary>
        /// <remarks>The two vectors need not have equal lengths: when one vectors is exhausted any remaining elements in the other vectors are ignored.</remarks>
        /// <param name="s1">first vector</param>
        /// <param name="s2">second vector</param>
        /// <param name="p">float constrained to `p > 0`</param>
        /// <returns>Minkowski distance between elements of given vectors. Returns NaN if vectors contain NaN.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. v1 and v2 initialization
        /// let v1 = vector [3.14; 2.0; 3.1]
        /// let v2 = vector [9.1; 2.5; 3.7]
        /// 
        /// // Apply the minkowski distance to v1 and v2
        /// Vector.minkowski v1 v2 3
        /// </code> 
        /// </example>
        let inline minkowski (v1: Vector<'a>) (v2: Vector<'a>) (p: float) : float option =
            if p <= 0.0 then
                None
            else
                let dim = min v1.Length v2.Length
                let mutable dist = 0.0

                for i in 0 .. (dim - 1) do
                    let diff = 
                        if v1.[i] > v2.[i] then
                            v1.[i] - v2.[i] 
                        else
                            v2.[i] - v1.[i]

                    let d = diff ** p
                    dist <- dist + d

                if p >= 1.0 then
                    Some (dist ** (1.0 / p))
                else
                    Some dist

        /// <summary>The [Minkowski distance](https://en.wikipedia.org/wiki/Minkowski_distance) between two vectors (ignores NaN) of order `p`.</summary>
        /// <remarks>Non-regular differences between the sequences are ignored.
        /// The two vectors need not have equal lengths: when one vectors is exhausted any remaining elements in the other vectors are ignored.</remarks>
        /// <param name="s1">first vector</param>
        /// <param name="s2">second vector</param>
        /// <param name="p">float constrained to `p > 0`</param>
        /// <returns>Minkowski distance between elements of given vectors.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. v1 and v2 initialization
        /// let v1 = vector [3.14; 2.0; 3.1]
        /// let v2 = vector [9.1; 2.5; 3.7]
        /// 
        /// // Apply the minkowski distance to v1 and v2
        /// Vector.minkowskiNaN v1 v2 3
        /// </code> 
        /// </example>
        let inline minkowskiNaN (v1: Vector<float>) (v2: Vector<float>) (p: float) : float option =
            if p <= 0.0 then
                None
            else
                let dim = min v1.Length v2.Length
                let mutable dist = 0.0

                for i in 0 .. (dim - 1) do
                    let diff = abs (v1.[i] - v2.[i])
                    let d = diff ** p
                    
                    if not (isNan d) then
                        dist <- dist + d

                if p >= 1.0 then
                    Some (dist ** (1.0 / p))
                else
                    Some dist
    
    [<RequireQualifiedAccess>]    
    module Array =
        
        /// <summary>Calculates Hamming distance of two coordinate arrays</summary>
        /// <remarks>Note, distance between Nan and Nan is equal to 1</remarks>
        /// <param name="a1">first array</param>
        /// <param name="a2">second array</param>
        /// <returns>Hamming distance between elements of given arrays</returns>
        /// <example> 
        /// <code> 
        /// // e.g. a1 and a2 initialization
        /// let a1 = [|1; 2; 3|]
        /// let a2 = [|9; 2; 3|]
        /// 
        /// // Apply the hamming to a1 and a2
        /// Array.hamming a1 a2
        /// </code> 
        /// </example>
        let inline hamming (a1: array<'a>) (a2: array<'a>) =
            let mutable dist = 0
            
            match a1.Length <> a2.Length with
            | true -> failwith "Inputs are not of equal length"
            | _    ->
                for i in 0 .. a1.Length - 1 do
                    if (a1[i] <> a2[i]) then dist <- dist + 1
            dist
            
        
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

        /// <summary>The [Minkowski distance](https://en.wikipedia.org/wiki/Minkowski_distance) between two arrays of order `p`.</summary>
        /// <remarks>The two arrays need not have equal lengths: when one array is exhausted any remaining elements in the other array are ignored.</remarks>
        /// <param name="s1">first array</param>
        /// <param name="s2">second array</param>
        /// <param name="p">float constrained to `p > 0`</param>
        /// <returns>Minkowski distance between elements of given arrays. Returns NaN if arrays contain NaN.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. a1 and a2 initialization
        /// let a1 = [|3.14; 2.0; 3.1|]
        /// let a2 = [|9.1; 2.5; 3.7|]
        /// 
        /// // Apply the minkowski distance to a1 and a2
        /// Array.minkowski a1 a2 3
        /// </code> 
        /// </example>
        let inline minkowski (a1: array<'a>) (a2: array<'a>) (p: float) : float option =
            if p <= 0.0 then
                None
            else
                let dim = min a1.Length a2.Length
                let mutable dist = 0.0

                for i in 0 .. (dim - 1) do
                    let diff = 
                        if a1.[i] > a2.[i] then
                            a1.[i] - a2.[i] 
                        else
                            a2.[i] - a1.[i]

                    let d = diff ** p
                    dist <- dist + d

                if p >= 1.0 then
                    Some (dist ** (1.0 / p))
                else
                    Some dist

        /// <summary>The [Minkowski distance](https://en.wikipedia.org/wiki/Minkowski_distance) between two arrays (ignores NaN) of order `p`.</summary>
        /// <remarks>Non-regular differences between the sequences are ignored.
        /// The two arrays need not have equal lengths: when one array is exhausted any remaining elements in the other array are ignored.</remarks>
        /// <param name="s1">first array</param>
        /// <param name="s2">second array</param>
        /// <param name="p">float constrained to `p > 0`</param>
        /// <returns>Minkowski distance between elements of given arrays.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. a1 and a2 initialization
        /// let a1 = [|3.14; 2.0; 3.1|]
        /// let a2 = [|9.1; 2.5; 3.7|]
        /// 
        /// // Apply the minkowski distance to a1 and a2
        /// Array.minkowskiNaN a1 a2 3
        /// </code> 
        /// </example>
        let inline minkowskiNaN (a1: array<float>) (a2: array<float>) (p: float) : float option =
            if p <= 0.0 then
                None
            else
                let dim = min a1.Length a2.Length
                let mutable dist = 0.0

                for i in 0 .. (dim - 1) do
                    let diff = abs (a1.[i] - a2.[i])
                    let d = diff ** p
                    
                    if not (isNan d) then
                        dist <- dist + d

                if p >= 1.0 then
                    Some (dist ** (1.0 / p))
                else
                    Some dist


    /// <summary>Calculates Hamming distance of two coordinate items</summary>
    /// <remarks>Note, distance between Nan and Nan is equal to 1</remarks>
    /// <param name="s1">first sequence</param>
    /// <param name="s2">second sequence</param>
    /// <returns>Hamming distance between elements of given sequences</returns>
    /// <example> 
    /// <code> 
    /// // e.g. s1 and s2 initialization
    /// let s1 = seq {1; 2; 3}
    /// let s2 = seq {9; 2; 3}
    /// 
    /// // Apply the hamming to s1 and s2
    /// hamming s1 s2
    /// </code> 
    /// </example>
    let inline hamming (s1: 'a) (s2: 'a) =
        match Seq.length s1 <> Seq.length s2 with
        | true -> failwith "Inputs are not of equal length"
        | _    ->
            Seq.zip s1 s2
            |> Seq.filter (fun (c1, c2) -> c1 <> c2)
            |> Seq.length
    
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

 
    /// <summary>The [Minkowski distance](https://en.wikipedia.org/wiki/Minkowski_distance) between two sequence of order `p`.</summary>
    /// <remarks>The two sequences need not have equal lengths: when one sequence is exhausted any remaining elements in the other sequence are ignored.</remarks>
    /// <param name="s1">first sequence</param>
    /// <param name="s2">second sequence</param>
    /// <param name="p">float constrained to `p > 0`</param>
    /// <returns>Minkowski distance between elements of given sequences. Returns NaN if sequences contain NaN.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. a1 and a2 initialization
    /// let s1 = seq { 3.14; 2.0; 3.1 }
    /// let s2 = { 9.1; 2.5; 3.7 }
    /// 
    /// // Apply the minkowski distance to s1 and s2
    /// minkowski s1 s2 3
    /// </code> 
    /// </example>
    let inline minkowski (s1: seq<'a>) (s2: seq<'a>) (p: float) =
        if p <=0.0 then
            None
        else
            let dist =
                (s1, s2)
                ||> Seq.zip
                |> Seq.fold (fun acc (x, y) ->                
                    let diff =
                        if x > y then
                            x - y
                        else
                            y - x

                    let d = diff ** p
                    acc + d)
                    0.0

            if p >= 1.0 then
                Some (dist ** (1.0 / p))
            else
                Some dist

   
    /// The [Minkowski distance](https://en.wikipedia.org/wiki/Minkowski_distance) between two sequences (ignores NaN) of order `p`.
    /// <remarks>Non-regular differences between the sequences are ignored.
    /// The two sequences need not have equal lengths: when one sequence is exhausted any remaining elements in the other sequence are ignored.</remarks>
    /// <param name="s1">first sequence</param>
    /// <param name="s2">second sequence</param>
    /// <param name="p">float constrained to `p > 0`</param>
    /// <returns>Minkowski distance between elements of given sequences.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. a1 and a2 initialization
    /// let s1 = seq { 3.14; 2.0; 3.1 }
    /// let s2 = { 9.1; 2.5; 3.7 }
    /// 
    /// // Apply the minkowski distance to s1 and s2
    /// minkowskiNaN s1 s2 3
    /// </code> 
    let inline minkowskiNaN (s1: seq<'a>) (s2: seq<'a>) (p: float) =
        if p <= 0.0 then
            None
        else
            let dist =
                (s1, s2)
                ||> Seq.zip
                |> Seq.fold (fun acc (x, y) ->                
                    let diff =
                        if x > y then
                            x - y
                        else
                            y - x

                    let d = diff ** p
                    if not (nan.Equals d) then
                        acc + d
                    else
                        acc)
                    0.0

            if p >= 1.0 then
                Some (dist ** (1.0 / p))
            else
                Some dist




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
