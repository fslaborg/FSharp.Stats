namespace FSharp.Stats


/// Module to estimate different quantile measures
module Quantile =

    type QuantileFunction<'a> =  float -> array<'a> -> float

    let private quantileHelper (quatileF : QuantileFunction<'a>) q (data:array<'a>) =

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

        
        /// Estimates the q-th quantile from the unsorted data array. (in place)
        /// Approximately median-unbiased regardless of the sample distribution.
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


        /// Estimates the q-th quantile from the unsorted data array. (in place)
        let empiricalInvCdfInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h = float data.Length * q + 0.5
                //Array.quickSelectInPlace (int (ceil (h-0.5))-1) data
                Array.quickSelectInPlace (int (ceil (h-0.5))) data  // TM checked
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the unsorted data array. (in place)
        let empiricalInvCdfAverageInPLace q (data:array<_>) =

            let f q (data:array<_>) =
                let h = float data.Length * q + 0.5
                let a = Array.quickSelectInPlace (int (ceil (h - 0.5))) data     // TM checked
                let b = Array.quickSelectInPlace (int (((h + 0.5) - 1.))+1) data // TM checked
                (a + b) * 0.5
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the unsorted data array. (in place)
        let nearestInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h = float data.Length * q
                //Array.quickSelectInPlace (int (System.Math.Round(h)) - 1) data
                Array.quickSelectInPlace (int (System.Math.Round(h))) data // TM checked
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the unsorted data array. (in place)
        let californiaInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float data.Length * q
                let h' = int h
                let a = Array.quickSelectInPlace (h') data   //TM
                let b = Array.quickSelectInPlace (h'+1) data //TM
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the unsorted data array. (in place)
        let hazenInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float data.Length * q + 0.5
                let h' = int h
                let a = Array.quickSelectInPlace (h') data   //TM
                let b = Array.quickSelectInPlace (h'+1) data //TM
                a + (h - float h') * (b - a)
        
            quantileHelper f q data        


        /// Estimates the q-th quantile from the unsorted data array. (in place)
        let nistInPLace q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float (data.Length+1) * q
                let h' = int h
                let a = Array.quickSelectInPlace (h') data   //TM
                let b = Array.quickSelectInPlace (h'+1) data //TM
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the unsorted data array. (in place)
        let modeInPLace q (data:array<_>) =
            let f q (data:array<_>) =                
                let h  = float (data.Length-1) * q + 1.
                let h' = int h
                let a = Array.quickSelectInPlace (h') data   //TM
                let b = Array.quickSelectInPlace (h'+1) data //TM
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the unsorted data array. (in place)
        let normalInPLace q (data:array<_>) =
            let f q (data:array<'a>) =                
                let h  = (float data.Length + 0.25) * q + 0.375
                let h' = int h
                let a = Array.quickSelectInPlace (h') data |> float //TM
                let b = Array.quickSelectInPlace (h'+1) data |> float //TM
                a + (h - float h') * (b - a)
            
            quantileHelper f q data


    /// ! Input needs to be sorted
    module OfSorted =

        /// ! Input needs to be sorted !
        /// Estimates the q-th quantile from the sorted data array.
        /// Approximately median-unbiased regardless of the sample distribution.
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


        /// Estimates the q-th quantile from the sorted data array.
        let empiricalInvCdf q (data:array<_>) =
            let f q (data:array<_>) =
                let h = float data.Length * q + 0.5
                data.[(int (ceil (h-0.5))-1)]
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the sorted data array.
        let empiricalInvCdfAverage q (data:array<_>) =

            let f q (data:array<_>) =
                let h = float data.Length * q + 0.5
                let a = data.[ (int (ceil (h - 0.5)) - 1) ]
                let b = data.[ (int (((h + 0.5) - 1.))) ]
                (a + b)*0.5
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the sorted data array.
        let nearest q (data:array<_>) =
            let f q (data:array<_>) =
                let h = float data.Length * q
                data.[max (int (System.Math.Round(h)) - 1) 0 ]
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the sorted data array.
        let california q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float data.Length * q
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the sorted data array.
        let hazen q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float data.Length * q + 0.5
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
        
            quantileHelper f q data        


        /// Estimates the q-th quantile from the sorted data array.
        let nist q (data:array<_>) =
            let f q (data:array<_>) =
                let h  = float (data.Length+1) * q
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the sorted data array.
        let mode q (data:array<_>) =
            let f q (data:array<_>) =                
                let h  = float (data.Length-1) * q + 1.
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
        
            quantileHelper f q data


        /// Estimates the q-th quantile from the sorted data array.
        let normal q (data:array<_>) =
            let f q (data:array<'a>) =                
                let h  = (float data.Length + 0.25) * q + 0.375
                let h' = int h
                let a = data.[ max (h' - 1) 0]
                let b = data.[ min h' (data.Length-1) ]
                a + (h - float h') * (b - a)
            
            quantileHelper f q data



    // ++++++++++++++++++++++++++++++++++++

    /// Estimates the q-th quantile from the unsorted data.
    /// Approximately median-unbiased regardless of the sample distribution.
    let inline compute q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.compute q data'

    /// Estimates the q-th quantile from the unsorted data.
    let empiricalInvCdf q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.empiricalInvCdf q data'
    
    
    /// Estimates the q-th quantile from the unsorted data.
    let empiricalInvCdfAverage q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.empiricalInvCdfAverage q data'        


    /// Estimates the q-th quantile from the unsorted data.
    let nearest q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.nearest q data'   
        
     
    /// Estimates the q-th quantile from the unsorted data.
    let california q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.california q data'
    
    
    /// Estimates the q-th quantile from the unsorted data.
    let hazen q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.hazen q data'        


    /// Estimates the q-th quantile from the unsorted data.
    let nist q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.nist q data'
    
    
    /// Estimates the q-th quantile from the unsorted data.
    /// R! default
    let mode q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.mode q data'        
    
    
    /// Estimates the q-th quantile from the unsorted data.
    let normal q (data:seq<_>) =
        let data' = Seq.toArray data
        Array.sortInPlace data'
        OfSorted.normal q data'



    /// Computes the interquartile range (IQR)
    //  The IQR is the 1st Quartile subtracted from the 3rd Quartile; these quartiles can be clearly seen on a box plot on the data.
    //  It is a trimmed estimator, defined as the 25% trimmed mid-range, and is the most significant basic robust measure of scale.
    let interQuantileRange (qf:QuantileFunction<'a>) (data:array<'a>) =
        (qf 0.75 data) - (qf 0.25 data)
        

    /// Computes percentiles
    /// percentiles: Each percentile must be between 0.0 and 1.0 (inclusive)
    /// CalcMethod should be ofSorted array
    let computePercentiles (calcMethod) (percentile:seq<float>) (data:seq<float>) =
        let data' = data |> Seq.toArray |> Array.sort
        let qtf = fun q -> calcMethod q data'
        percentile |> Seq.map qtf

