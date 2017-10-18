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

    /// Imputation by random sampling from the input vector
    let rnd (rnd:System.Random) :  VectorBaseImputation<'a> =        
        fun fdata index ->
            let farr = Array.ofSeq fdata
            if farr.Length < 1 then failwithf "Vector needs at least one non-missing value" 
            farr.[rnd.Next(0,farr.Length - 1)]

    /// Imputation by sampling from a gausian normal distribution based on the input vector
    let normal :  VectorBaseImputation<float> =          
        fun fdata index ->
            let median = Seq.median fdata
            let std  = Seq.stDev fdata
            if not(System.Double.IsNaN(median) || System.Double.IsNaN(std)) then
                Distributions.Continuous.Normal.Sample median std
            else
                failwithf "Vector needs at least two non-missing value"




    ///// Imputation by k-nearest neighbour
    //let kNearestImpute k : MatrixBaseImputation<float[],float> = 
    //    fun data arr index ->
        
    //        let kNearestFrom (distance:DistanceMetrics.Distance<'a>) k (arr: 'a array) (queryCoordinates:'a) =
    //            arr
    //            |> Array.map (fun t -> (distance t queryCoordinates,t))
    //            |> Array.sortBy fst
    //            |> Array.take k    
    
    //        let euclNan = DistanceMetrics.euclidean
    //        let tmpArr =
    //            kNearestFrom euclNan k (data |> Array.ofSeq) arr
    //            |> Array.map snd
    //            |> JaggedArray.transpose
    //            |> Array.map Seq.mean
    //        tmpArr.[index]


    ///// Imputes column-wise by vector-based imputation
    //let imputeColWiseBy (impute: VectorBaseImputation<'a>) isMissing (data : seq<#seq<'a>>) =        
    //    data
    //    |> ofJaggedSeq
    //    |> JaggedArray.transpose
    //    |> Array.map (fun col ->  
    //                    let fCol = col |> Array.filter (isMissing >> not) 
    //                    let impute' = impute fCol
    //                    col
    //                    |> Array.mapi (fun i v -> if isMissing v then (impute' i) else v)
    //                    )
    //    |> JaggedArray.transpose             
    

    ///// Imputes row-wise by vector-based imputation
    //let imputeRowWiseBy (impute: VectorBaseImputation<'a>) isMissing (data : seq<#seq<'a>>) =        
    //    data
    //    |> ofJaggedSeq
    //    |> Array.map (fun row ->  
    //                    let fRow = row |> Array.filter (isMissing >> not) 
    //                    let impute' = impute fRow
    //                    row
    //                    |> Array.mapi (fun i v -> if isMissing v then (impute' i) else v)
    //                    )    


    ///// Imputes rows by matrix-based imputation
    //let imputeBy (impute: MatrixBaseImputation<'a[],'a>) isMissing data =        
    //    let fData = 
    //        data
    //        |> Seq.filter (fun row -> row |> Seq.exists isMissing |> not)
    //        |> Seq.map (fun row -> row |> Seq.toArray)
    //        |> Seq.toArray
        
    //    data
    //    |> ofJaggedSeq
    //    |> Array.map (fun row ->  
    //                    let row' = row |> Array.ofSeq
    //                    let impute' = impute fData row'
    //                    row'
    //                    |> Array.mapi (fun i v -> if isMissing v then (impute' i) else v)
    //                    )  




//let a = [3.;2.;3.;4.;5.;]
//let b = [1.;2.;3.;nan;5.;]
//let c = [nan;2.;3.;4.;nan;]
//let d = [5.;2.;6.;4.;5.;]
//let e = [0.5;2.;3.;5.;5.;]
//
//let data = [a;b;c;d;e]
//
//// Test kNearest imputation
//// ------------------------
//
//// init kNearest MatrixBaseImpute
//let kn : Impute.MatrixBaseImputation<float[],float> = Impute.kNearestImpute 2
//Impute.imputeBy kn Float.isNan data
//
//
//
//// Test random imputation
//// ----------------------
//
//// init random VectorBaseImpute
//let rnd = Impute.rnd (new System.Random())
//
//Impute.imputeRowWiseBy rnd Float.isNan data
//Impute.imputeColWiseBy rnd Float.isNan data
//
//
//
//#time
//for i in [1..10000] do
//    Array.ofSeq (seq [1..100000]) |> ignore
//
//let arr = [|1..100000|]
//
//for i in [1..10000] do
//    Array.ofSeq arr |> ignore

