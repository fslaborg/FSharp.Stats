namespace FSharp.Stats.ML

open FSharp.Stats
//open FSharp.Care
//open FSharp.Care.Collections

/// Module for data imputation and missing value filtering
module Impute =

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





