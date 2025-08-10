namespace FSharp.Stats

open FsMath
open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =


    //----------------------------------------------------------------------------
    // Stats
    //----------------------------------------------------------------------------
    
 
    /// <summary>Creates a vector from a sequence</summary>
    let interval (items:Vector<'T>) =
        let rec loop index (minimum) (maximum) =
            if index < items.Length then
                let current = items.[index]
                loop (index+1) (min current minimum) (max current maximum)
            else
                Interval.CreateClosed<_> (minimum,maximum)
        //Init by fist value
        if items.Length > 1 then
            loop 1 items.[0] items.[0] 
        else
            Interval.Empty

    ///// <summary>Computes the population mean (Normalized by N)            </summary>
    ///// <remarks></remarks>
    ///// <param name="items"></param>
    ///// <returns></returns>
    ///// <example>
    ///// <code>
    ///// </code>
    ///// </example>
    //let inline mean (items:Vector<'T>) = 
    //    Vector.mean items
       

    /// <summary>Computes the sample median</summary>
    /// <remarks></remarks>
    /// <param name="items"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline median (items:Vector<float>) =
        items |> Array.median
        
    /// <summary>Median absolute deviation (MAD)</summary>
    /// <remarks></remarks>
    /// <param name="items"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let medianAbsoluteDev (items:Vector<float>) =       
        items |> Array.medianAbsoluteDev

    
    /// <summary>Returns SummaryStats of vector with N, mean, sum-of-squares, minimum and maximum</summary>
    /// <remarks></remarks>
    /// <param name="items"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline stats (items:Vector<'T>) =
        let zero = LanguagePrimitives.GenericZero< 'T > 
        let one = LanguagePrimitives.GenericOne< 'T >        
        
        let rec loop index n (minimum) (maximum) m1 m2 =
            if index < items.Length then            
                let current  = items.[index]
                let delta    = current - m1               
                let deltaN  = (delta / n)
                //let delta_n2 = deltaN * deltaN
                let m1'    = m1 + deltaN            
                let m2' = m2 + delta * deltaN * (n-one)
                loop (index+1) (n + one) (min current minimum) (max current maximum) m1' m2'
            else
                SummaryStats.createSummaryStats (n-one) m1 m2 minimum maximum
        //Init by fist value
        if items.Length > 1 then
            loop 0 one items.[0] items.[0] zero zero 
        else
            let uNan = zero / zero 
            SummaryStats.createSummaryStats zero uNan uNan uNan uNan


    /// <summary>Returns an estimator of the population covariance of two random variables v1 and v2 </summary>
    /// <remarks></remarks>
    /// <param name="v1"></param>
    /// <param name="v2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let covPopulation (v1:Vector<float>) (v2:Vector<float>) = 
        Seq.covPopulation v1 v2

    /// <summary>Returns the sample covariance of two random variables v1 and v2. (Bessel's correction by N-1) </summary>
    /// <remarks></remarks>
    /// <param name="v1"></param>
    /// <param name="v2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let cov (v1:Vector<float>) (v2:Vector<float>) = 
        Seq.cov v1 v2

    /// <summary>calculates the sample means with a given number of replicates present in the sequence</summary>
    /// <remarks></remarks>
    /// <param name="rep"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getMeanOfReplicates rep (data:Vector<float>) =
        Seq.getMeanOfReplicates rep data
        |> Vector.ofSeq 

    /// <summary>calculates the sample standard deviations with a given number of replicates present in the sequence</summary>
    /// <remarks></remarks>
    /// <param name="rep"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getStDevOfReplicates rep (data:Vector<float>) =
        Seq.getStDevOfReplicates rep data
        |> Vector.ofSeq 

    /// <summary>calculates the coefficient of variation based on the sample standard deviations with a given number of replicates present in the sequence</summary>
    /// <remarks></remarks>
    /// <param name="rep"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getCvOfReplicates rep (data:Vector<float>) =
        Seq.getCvOfReplicates rep data
        |> Vector.ofSeq 

    ///// <summary>Splits a vector according to given indices. Returns (vector including values according to indices, rest)</summary>
    ///// <remarks></remarks>
    ///// <param name="indices"></param>
    ///// <param name="v"></param>
    ///// <returns></returns>
    ///// <example>
    ///// <code>
    ///// </code>
    ///// </example>
    //let splitVector (indices:int[]) (v:Vector<_>) =
    //    let len = v.Length
    //    //let nv  = Vector.Generic.zero (len-indices.Length)
    //    //let nvi = Vector.Generic.zero indices.Length
    //    let nv  = VG.zeroCreate (len-indices.Length)
    //    let nvi = VG.zeroCreate indices.Length
    //    indices |> Array.sortInPlace
    //    let rec loop ni nii i =
    //        match i with
    //        | i when i < 0 -> nvi,nv
    //        | i when nii >= 0 && i = indices.[nii] ->            
    //            nvi.[nii] <- v.[i]                
    //            loop (ni) (nii-1) (i-1)                       
    //        | _ -> 
    //            nv.[ni] <- v.[i]
    //            loop (ni-1) (nii) (i-1) 
    
    //    loop (len-1-indices.Length) (indices.Length-1) (len-1)


    /// Module to compute common statistical measure on 
    module SummaryStats = 

        /// <summary>Returns SummaryStats of vector with N, mean, sum-of-squares, minimum and maximum</summary>
        /// <remarks></remarks>
        /// <param name="a"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let ofVector (a:Vector<'a>) = stats a









//[<AutoOpen>]
//module VectorExtension =

//    type Vector<'T when 'T :> Numerics.INumber<'T>> with 
//        member x.Norm      = Vector.Generic.norm x
//        member x.Copy ()   = Vector.Generic.copy x

//        /// <summary>
//        /// Creates an vector with values between a given interval
//        /// </summary>
//        /// <param name="start">start value (is included)</param>
//        /// <param name="stop">end value (by default is included)</param>
//        /// <param name="num">sets the number of elements in the vector. If not set, stepsize = 1.</param>
//        /// <param name="IncludeEndpoint">If false, the vector does not contain the stop value</param>
//        static member linspace(start:float,stop:float,num:int,?IncludeEndpoint:bool) : vector = 
        
//            let includeEndpoint = defaultArg IncludeEndpoint true
 
//            Seq.linspace(start,stop,num,includeEndpoint) |> Vector.ofSeq

//        /// <summary>
//        /// Creates a geometric vector of floats with values between a given interval.
//        /// </summary>
//        /// <param name="start">start value (is included)</param>
//        /// <param name="stop">end value (by default is included)</param>
//        /// <param name="num">sets the number of elements in the vector. Defaults to 50.</param>
//        /// <param name="IncludeEndpoint">If false, the vector does not contain the stop value. Defaults to true.</param>
//        static member geomspace(start:float,stop:float,num:int,?IncludeEndpoint:bool) : vector = 
//            let includeEndpoint = defaultArg IncludeEndpoint true

//            Seq.geomspace (start, stop ,num, includeEndpoint)
//            |> Vector.ofSeq
   
