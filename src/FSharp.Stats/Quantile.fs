namespace FSharp.Stats


/// Module to estimate different quantile measures
module Quantile =

    type QuantileFunction<'a> =  float -> array<'a> -> float

    let inline private quantileHelper (quatileF : QuantileFunction<'a>) q (data:array<'a>) =

        if (q < 0. || q > 1. || data.Length = 0) then
            nan
        elif (q = 0. || data.Length = 1) then
            Array.min data |> float
        elif (q = 1.) then
            Array.max data |> float
        else            
            quatileF q data


    /// ! Works inplace and can thus causes the data array to be reordered
    module InPlace =

        
        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)<br />Approximately median-unbiased regardless of the sample distribution.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let computeInplace q (data:array<_>) =
        
            let h  = ((float data.Length + 1./3.)*q + 1./3.)
            let h' = h |> int
        
            if (q < 0. || q > 1. || data.Length = 0) then
                nan
            elif (h' <= 0 || q = 0.) then
                Array.min data
            elif (h' >= data.Length || q = 1.) then
                Array.max data
            else
                let a = Array.quickSelectInPlace  h' data
                let b = Array.quickSelectInPlace (h'+1) data
                a + (h - float h') * (b - a);    


        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let empiricalInvCdfInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h = float data.Length * q + 0.5
                //Array.quickSelectInPlace (int (ceil (h-0.5))-1) data
                Array.quickSelectInPlace (int (ceil (h-0.5))) data  // TM checked
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let empiricalInvCdfAverageInPLace q (data:array<_>) =

            let f q (data:array<_>) =
                let h = float data.Length * q + 0.5
                let a = Array.quickSelectInPlace (int (ceil (h - 0.5))) data     // TM checked
                let b = Array.quickSelectInPlace (int (((h + 0.5) - 1.))+1) data // TM checked
                (a + b) * 0.5
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let nearestInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h = float data.Length * q
                //Array.quickSelectInPlace (int (System.Math.Round(h)) - 1) data
                Array.quickSelectInPlace (int (System.Math.Round(h))) data // TM checked
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let californiaInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float data.Length * q
                let h' = int h
                let a = Array.quickSelectInPlace (h') data   //TM
                let b = Array.quickSelectInPlace (h'+1) data //TM
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let hazenInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float data.Length * q + 0.5
                let h' = int h
                let a = Array.quickSelectInPlace (h') data   //TM
                let b = Array.quickSelectInPlace (h'+1) data //TM
                a + (h - float h') * (b - a)
        
            quantileHelper f q data        


        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let nistInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float (data.Length+1) * q
                let h' = int h
                let a = Array.quickSelectInPlace (h') data   //TM
                let b = Array.quickSelectInPlace (h'+1) data //TM
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let modeInPLace q (data:array<_>) =
            let f q (data:array<_>) =                
                let h  = float (data.Length-1) * q + 1.
                let h' = int h
                let a = Array.quickSelectInPlace (h') data   //TM
                let b = Array.quickSelectInPlace (h'+1) data //TM
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the unsorted data array. (in place)</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let normalInPLace q (data:array<_>) =
            let f q (data:_[]) =                
                let h  = (float data.Length + 0.25) * q + 0.375
                let h' = int h
                let a = Array.quickSelectInPlace (h') data |> float //TM
                let b = Array.quickSelectInPlace (h'+1) data |> float //TM
                a + (h - float h') * (b - a)
            
            quantileHelper f q data


    /// ! Input needs to be sorted
    module OfSorted =

        /// ! Input needs to be sorted !
        /// <summary>Estimates the q-th quantile from the sorted data array.<br />Approximately median-unbiased regardless of the sample distribution.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let compute q (data:array<_>) =
        
            let h  = ((float data.Length + 1./3.)*q + 1./3.)
            let h' = h |> int
        
            if (q < 0. || q > 1. || data.Length = 0) then
                nan
            elif (h' <= 0 || q = 0.) then
                Array.min data
            elif (h' >= data.Length || q = 1.) then
                Array.max data
            else
                let a = data.[h'-1]
                let b = data.[h']
                a + (h - float h') * (b - a);    


        /// <summary>Estimates the q-th quantile from the sorted data array.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let empiricalInvCdf q (data:array<_>) =
            let f q (data:array<_>) =
                let h = float data.Length * q + 0.5
                data.[(int (ceil (h-0.5))-1)]
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the sorted data array.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let empiricalInvCdfAverage q (data:array<_>) =

            let f q (data:array<_>) =
                let h = float data.Length * q + 0.5
                let a = data.[ (int (ceil (h - 0.5)) - 1) ]
                let b = data.[ (int (((h + 0.5) - 1.))) ]
                (a + b)*0.5
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the sorted data array.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let nearest q (data:array<_>) =
            let f q (data:array<_>) =
                let h = float data.Length * q
                data.[max (int (System.Math.Round(h)) - 1) 0 ]
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the sorted data array.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let california q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float data.Length * q
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the sorted data array.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let hazen q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float data.Length * q + 0.5
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
        
            quantileHelper f q data        


        /// <summary>Estimates the q-th quantile from the sorted data array.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let nist q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float (data.Length+1) * q
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the sorted data array.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let mode q (data:array<_>) =
            let f q (data:array<_>) =                
                let h  = float (data.Length-1) * q + 1.
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// <summary>Estimates the q-th quantile from the sorted data array.</summary>
        /// <remarks></remarks>
        /// <param name="q"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let normal q (data:array<_>) =
            let f q (data:float[]) =                
                let h  = (float data.Length + 0.25) * q + 0.375
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
            
            quantileHelper f q data



    // ++++++++++++++++++++++++++++++++++++

    /// <summary>Estimates the q-th quantile from the unsorted data.<br />Approximately median-unbiased regardless of the sample distribution.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline compute q (data:seq<_>) =
        let data = Seq.UtilityFunctions.toArrayCopyQuick data
        let h  = ((float data.Length + 1./3.)*q + 1./3.)
        let h' = h |> int
        
        if (q < 0. || q > 1. || data.Length = 0) then
            nan
        elif (h' <= 0 || q = 0.) then
            if Array.exists Ops.isNan data then 
                nan 
            else Array.min data
        elif (h' >= data.Length || q = 1.) then
            if Array.exists Ops.isNan data then 
                nan 
            else Array.max data
        else
            let a = Array.quickSelectInPlace (h') data
            let b = Array.quickSelectInPlace (h'+1) data
            a + (h - float h') * (b - a); 

    /// <summary>Estimates the q-th quantile from the unsorted data.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let empiricalInvCdf q (data:seq<_>) =
        let data' = Seq.UtilityFunctions.toArrayCopyQuick data
        InPlace.empiricalInvCdfInPLace q data'
    
    
    /// <summary>Estimates the q-th quantile from the unsorted data.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let empiricalInvCdfAverage q (data:seq<_>) =
        let data' = Seq.UtilityFunctions.toArrayCopyQuick data
        InPlace.empiricalInvCdfAverageInPLace q data'        


    /// <summary>Estimates the q-th quantile from the unsorted data.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let nearest q (data:seq<_>) =
        let data' = Seq.UtilityFunctions.toArrayCopyQuick data
        InPlace.nearestInPLace q data'   
        
     
    /// <summary>Estimates the q-th quantile from the unsorted data.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let california q (data:seq<_>) =
        let data' = Seq.UtilityFunctions.toArrayCopyQuick data
        InPlace.californiaInPLace q data'
    
    
    /// <summary>Estimates the q-th quantile from the unsorted data.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let hazen q (data:seq<_>) =
        let data' = Seq.UtilityFunctions.toArrayCopyQuick data
        InPlace.hazenInPLace q data'        


    /// <summary>Estimates the q-th quantile from the unsorted data.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let nist q (data:seq<_>) =
        let data' = Seq.UtilityFunctions.toArrayCopyQuick data
        InPlace.nistInPLace q data'
    
    
    /// <summary>Estimates the q-th quantile from the unsorted data.<br />R! default</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mode q (data:seq<_>) =
        let data' = Seq.UtilityFunctions.toArrayCopyQuick data
        InPlace.modeInPLace q data'        
    
    
    /// <summary>Estimates the q-th quantile from the unsorted data.</summary>
    /// <remarks></remarks>
    /// <param name="q"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let normal q (data:float seq) =
        let data' = Seq.UtilityFunctions.toArrayCopyQuick data
        InPlace.normalInPLace q data'



    /// Computes the interquartile range (IQR)
    //  The IQR is the 1st Quartile subtracted from the 3rd Quartile; these quartiles can be clearly seen on a box plot on the data.
    //  It is a trimmed estimator, defined as the 25% trimmed mid-range, and is the most significant basic robust measure of scale.
    let interQuantileRange (qf:QuantileFunction<'a>) (data:array<'a>) =
        (qf 0.75 data) - (qf 0.25 data)
        

    /// <summary>Computes percentiles<br />percentiles: Each percentile must be between 0.0 and 1.0 (inclusive)<br />CalcMethod should be ofSorted array</summary>
    /// <remarks></remarks>
    /// <param name="(calcMethod)"></param>
    /// <param name="percentile"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let computePercentiles (calcMethod) (percentile:seq<float>) (data:seq<float>) =
        let data' = data |> Seq.toArray |> Array.sort
        let qtf = fun q -> calcMethod q data'
        percentile |> Seq.map qtf

