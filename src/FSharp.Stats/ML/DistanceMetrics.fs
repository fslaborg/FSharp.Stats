namespace FSharp.Stats.ML

open System
open FSharp.Stats

[<Obsolete("Use FSharp.Stats.DistanceMetrics instead")>]
module DistanceMetrics =

    module Vector = 
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Vector.euclidean instead")>]
        /// Euclidean distance between 2 vectors
        let inline euclidean (v1:Vector<'a>) (v2:Vector<'a>) = 
            DistanceMetrics.Vector.euclidean v1 v2
        
        /// Squared Euclidean distance between 2 vectors
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Vector.euclideanSquared instead")>]
        let inline euclideanSquared (v1:Vector<'a>) (v2:Vector<'a>) = 
            DistanceMetrics.Vector.euclideanSquared v1 v2
        
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Vector.euclideanNaN instead")>]
        /// Euclidean distance between 2 vectors (ignores nan) 
        let euclideanNaN (v1:Vector<float>) (v2:Vector<float>) = 
            DistanceMetrics.Vector.euclideanNaN v1 v2

        /// Cityblock distance of two vectors
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Vector.cityblock instead")>]
        let inline cityblock (v1:Vector<'a>) (v2:Vector<'a>) = 
            DistanceMetrics.Vector.cityblock v1 v2
        
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Vector.cityblockNaN instead")>]
        /// Cityblock distance of two vectors
        let cityblockNaN (v1:Vector<float>) (v2:Vector<float>) = 
            DistanceMetrics.Vector.cityblockNaN v1 v2
 
    module Array = 
        
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Array.euclidean instead")>]
        /// Euclidean distance of two coordinate arrays
        let inline euclidean (a1:array<'a>) (a2:array<'a>) = 
            DistanceMetrics.Array.euclidean a1 a2
        
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Array.euclideanNaN instead")>]
        /// Euclidean distance of two coordinate float arrays (ignores nan)
        let euclideanNaN (a1:array<float>) (a2:array<float>) =                     
            DistanceMetrics.Array.euclideanNaN a1 a2
        
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Array.euclideanNaNSquared instead")>]
        /// Squared Euclidean distance of two coordinate float arrays (ignores nan)
        let euclideanNaNSquared (a1:array<float>) (a2:array<float>) =               
            DistanceMetrics.Array.euclideanNaNSquared a1 a2
        
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Array.cityblock instead")>]
        /// Cityblock distance of two coordinate arrays
        let inline cityblock (a1:array<'a>) (a2:array<'a>) = 
            DistanceMetrics.Array.cityblock a1 a2
        
        [<Obsolete("Use FSharp.Stats.DistanceMetrics.Array.cityblockNaN instead")>]
        /// Cityblock distance of two coordinate float arrays (ignores nan)
        let cityblockNaN (a1:array<float>) (a2:array<float>) = 
            DistanceMetrics.Array.cityblockNaN a1 a2

        
    [<Obsolete("Use FSharp.Stats.DistanceMetrics.euclidean instead")>]
    /// Euclidean distance of two coordinate sequences
    let inline euclidean (s1:seq<'a>) (s2:seq<'a>) = 
        DistanceMetrics.euclidean s1 s2
       

    [<Obsolete("Use FSharp.Stats.DistanceMetrics.euclideanNaN instead")>]
    /// Euclidean distance of two coordinate float sequences (ignores nan)
    let euclideanNaN (s1:seq<float>) (s2:seq<float>) =    
        DistanceMetrics.euclideanNaN s1 s2

    [<Obsolete("Use FSharp.Stats.DistanceMetrics.euclideanNaNSquared instead")>]
    /// Squared Euclidean distance of two coordinate float sequences (ignores nan)
    let euclideanNaNSquared (s1:seq<float>) (s2:seq<float>) =            
        DistanceMetrics.euclideanNaNSquared s2 s2

    [<Obsolete("Use FSharp.Stats.DistanceMetrics.cityblock instead")>]
    /// Cityblock distance of two coordinate float sequences
    let inline cityblock (s1:seq<'a>) (s2:seq<'a>) = 
        DistanceMetrics.cityblock s1 s2

    [<Obsolete("Use FSharp.Stats.DistanceMetrics.cityblockNaN instead")>]
    /// Cityblock distance of two coordinate float sequences
    let cityblockNaN (s1:seq<float>) (s2:seq<float>) = 
        DistanceMetrics.cityblockNaN s1 s2

    [<Obsolete("Use FSharp.Stats.DistanceMetrics.dissimilarity instead")>]
    /// "Dissimilarity" uses 1. - pearsons correlation coefficient 
    let inline dissimilarity v1 v2 =
        DistanceMetrics.dissimilarity v1 v2

    [<Obsolete("Use FSharp.Stats.DistanceMetrics.wagnerFischerLazy instead")>]
    /// Levenshtein distance between
    let wagnerFischerLazy (s: string) (t: string) =
        DistanceMetrics.wagnerFischerLazy s t



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
