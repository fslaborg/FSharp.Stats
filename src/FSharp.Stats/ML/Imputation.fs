namespace FSharp.Stats.ML

open FSharp.Stats

/// Module for data imputation and missing value filtering
module Imputation =

    module Cleaning =

        let calcFractionBy (isMissing) (dataRow:seq<'a>) =     
            dataRow
            |> Seq.fold (fun (mc,nmc) state -> 
                match isMissing state with
                | true  -> (mc+1,nmc) 
                | false -> (mc,nmc+1) )
                     (0,0)
            |> fun (mc,nmc) -> float mc / float (nmc + mc)

    
        let removeAllBy f threshold (data:seq<#seq<'a>>) =
            data
            |> Seq.filter (fun row -> f row <= threshold )

    
    /// Type definintion for a vector based imputation.
    /// The imputed values are based only on the given array
    type VectorBaseImputation<'a>    = seq<'a>  -> int -> 'a
    
    /// Type definintion for a vector based imputation
    /// The imputed values are based on the given whole dataset
    type MatrixBaseImputation<'a,'b> = seq<'a> -> 'a -> int -> 'b


    /// <summary>Imputation by random sampling from the input vector</summary>
    /// <remarks></remarks>
    /// <param name="rnd"></param>
    /// <param name="fdata"></param>
    /// <param name="index"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let rnd (rnd:System.Random) :  VectorBaseImputation<'a> =        
        fun fdata index ->
            let farr = Array.ofSeq fdata
            if farr.Length < 1 then failwithf "Vector needs at least one non-missing value" 
            farr.[rnd.Next(0,farr.Length - 1)]


    /// Imputation by sampling from a gausian normal distribution based on the input vector
    let normal :  VectorBaseImputation<float> =          
        fun fdata index ->
            let mean = Seq.mean fdata
            let std  = Seq.stDev fdata
            if not(System.Double.IsNaN(mean) || System.Double.IsNaN(std)) then
                Distributions.Continuous.Normal.Sample mean std
            else
                failwithf "Vector needs at least two non-missing value"


    ///// Imputation by sampling from a gausian normal distribution based on the input vector
    //let normalTruncated :  VectorBaseImputation<float> =          
    //    fun fdata index ->
    //        let mean = Seq.mean fdata
    //        let std  = Seq.stDev fdata
    //        if not(System.Double.IsNaN(mean) || System.Double.IsNaN(std)) then
    //            Distributions.Continuous.Normal.Sample mean std
    //        else
    //            failwithf "Vector needs at least two non-missing value"


    /// <summary>Imputation by k-nearest neighbour</summary>
    /// <remarks></remarks>
    /// <param name="k"></param>
    /// <param name="data"></param>
    /// <param name="arr"></param>
    /// <param name="index"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let kNearestImpute k : MatrixBaseImputation<float[],float> = 
        fun data arr index ->
        
            let kNearestFrom (distance:DistanceMetrics.Distance<'a>) k (arr: 'a array) (queryCoordinates:'a) =
                arr
                |> Array.map (fun t -> (distance t queryCoordinates,t))
                |> Array.sortBy fst
                |> Array.take k    
    
            let euclNanSq = DistanceMetrics.euclideanNaNSquared
            let tmpArr =
                kNearestFrom euclNanSq k (data |> Array.ofSeq) arr
                |> Array.map snd
                |> JaggedArray.transpose
                |> Array.map Seq.mean
            tmpArr.[index]


    /// <summary>
    /// Imputation by distance-weighted k-nearest neighbour.
    /// Missing values are replaced by a weighted average of the k nearest neighbours,
    /// where each neighbour's contribution is scaled by a user-supplied weight derived
    /// from its distance to the incomplete row.
    /// </summary>
    /// <param name="distanceMetric">
    /// Distance function between two float arrays.
    /// Use <c>DistanceMetrics.Array.euclideanNaNSquared</c> (the default in
    /// <see cref="kNearestImpute"/>) to skip NaN positions when measuring distance.
    /// </param>
    /// <param name="distanceToWeight">
    /// Converts a raw distance value into a non-negative weight.
    /// For Euclidean-style metrics use an inverse such as <c>fun d -> 1.0 / (d + System.Double.Epsilon)</c>.
    /// For similarity measures (e.g. Pearson correlation) pass <c>id</c> directly,
    /// or its reciprocal if you stored it as a distance.
    /// </param>
    /// <param name="k">Number of nearest neighbours to consider.</param>
    /// <param name="data">Complete rows used as the neighbour pool (rows with missing values are excluded upstream by <see cref="imputeBy"/>).</param>
    /// <param name="arr">The row containing the missing value to impute.</param>
    /// <param name="index">Column index of the missing value within <paramref name="arr"/>.</param>
    /// <returns>Imputed value at <paramref name="index"/>.</returns>
    /// <example>
    /// <code>
    /// // Distance-weighted KNN with inverse-distance weighting
    /// let isMissing = System.Double.IsNaN
    /// let invDistWeight d = 1.0 / (d + System.Double.Epsilon)
    /// let imputer = Imputation.kNearestWeightedImpute DistanceMetrics.Array.euclideanNaNSquared invDistWeight 3
    /// let imputed = Imputation.imputeBy imputer isMissing rawData
    /// </code>
    /// </example>
    let kNearestWeightedImpute
            (distanceMetric: DistanceMetrics.Distance<float[]>)
            (distanceToWeight: float -> float)
            k
            : MatrixBaseImputation<float[],float> =
        fun data arr index ->
            let dataset = data |> Array.ofSeq
            let n = min k dataset.Length
            if n = 0 then
                nan
            else
                let neighbors =
                    dataset
                    |> Array.map (fun row -> (distanceMetric row arr, row))
                    |> Array.sortBy fst
                    |> Array.take n
                let weights = neighbors |> Array.map (fun (d, _) -> distanceToWeight d)
                let totalWeight = Array.sum weights
                if totalWeight = 0.0 then
                    neighbors |> Array.averageBy (fun (_, row: float[]) -> row.[index])
                else
                    let weightedSum =
                        Array.map2 (fun w (_, row: float[]) -> w * row.[index]) weights neighbors
                        |> Array.sum
                    weightedSum / totalWeight


    /// <summary>Imputes column-wise by vector-based imputation</summary>
    /// <remarks></remarks>
    /// <param name="impute"></param>
    /// <param name="isMissing"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let imputeColWiseBy (impute: VectorBaseImputation<'a>) isMissing (data : seq<#seq<'a>>) =        
        data
        |> JaggedArray.ofJaggedSeq
        |> JaggedArray.transpose
        |> Array.map (fun col ->  
                        let fCol = col |> Array.filter (isMissing >> not) 
                        let impute' = impute fCol
                        col
                        |> Array.mapi (fun i v -> if isMissing v then (impute' i) else v)
                        )
        |> JaggedArray.transpose             
    

    /// <summary>Imputes row-wise by vector-based imputation</summary>
    /// <remarks></remarks>
    /// <param name="impute"></param>
    /// <param name="isMissing"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let imputeRowWiseBy (impute: VectorBaseImputation<'a>) isMissing (data : seq<#seq<'a>>) =        
        data
        |> JaggedArray.ofJaggedSeq
        |> Array.map (fun row ->  
                        let fRow = row |> Array.filter (isMissing >> not) 
                        let impute' = impute fRow
                        row
                        |> Array.mapi (fun i v -> if isMissing v then (impute' i) else v)
                        )    


    /// <summary>Imputes rows by matrix-based imputation</summary>
    /// <remarks></remarks>
    /// <param name="impute"></param>
    /// <param name="isMissing"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let imputeBy (impute: MatrixBaseImputation<'a[],'a>) isMissing data =        
        let fData = 
            data
            |> Seq.filter (fun row -> row |> Seq.exists isMissing |> not)
            |> Seq.map (fun row -> row |> Seq.toArray)
            |> Seq.toArray
        
        data
        |> JaggedArray.ofJaggedSeq
        |> Array.map (fun row ->  
                        let row' = row |> Array.ofSeq
                        let impute' = impute fData row'
                        row'
                        |> Array.mapi (fun i v -> if isMissing v then (impute' i) else v)
                        )  





