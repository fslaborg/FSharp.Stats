namespace FSharp.Stats.Interpolation

open System

module LinearSpline = 
    
    open FSharp.Stats
    
    type LinearSplineCoef = {
        /// sample points (N+1), sorted ascending
        XValues : float []
        /// Zero order spline coefficients (N)
        C0 : float []
        /// First order spline coefficients (N)
        C1 : float []

        }

    ///
    let createLinearSplineCoef xValues c0 c1 = {
        XValues = xValues;
        C0      = c0;
        C1      = c1;
        }


    let private leftSegmentIdx arr value = 
        let idx = 
            let tmp = Array.BinarySearch(arr, value)
            let idx = if tmp < 0 then ~~~tmp-1 else tmp
            idx
        Math.Min(arr.Length-2,Math.Max(idx, 0))

    
    /// Returns the linear spline interpolation coefficients from sorted x,y data
    let initInterpolateSorted (x:array<float>) (y:array<float>) =
        if x.Length <> y.Length then
            failwith "input arrays differ in length"
        if
            x.Length < 2 then
            failwith "input arrays are too small to perform a spline interpolation"     

        let c1 =
            Array.init (x.Length - 1) (fun i -> (y.[i + 1] - y.[i]) / (x.[i + 1] - x.[i]) )

        createLinearSplineCoef x y c1


    /// Returns the linear spline interpolation coefficients from unsorted x,y data 
    /// Works in place
    let initInterpolateInplace (x:array<float>) (y:array<float>) =
        if x.Length <> y.Length then
            failwith "input arrays differ in length"

        Array.sort2InPlaceByKeys 0 (x.Length) x y
        initInterpolateSorted x y

    /// Returns the linear spline interpolation coefficients from unsorted x,y data     
    let initInterpolate (x:array<float>) (y:array<float>) =
        if x.Length <> y.Length then
            failwith "input arrays differ in length"
        
        let x' = Array.copy x
        let y' = Array.copy y
        Array.sort2InPlaceByKeys 0 (x'.Length) x' y'
        initInterpolateSorted x' y'

    /// Interpolate at point x
    let interpolate (lsc:LinearSplineCoef) x =
        let k = leftSegmentIdx lsc.XValues x
        lsc.C0.[k] + (x - lsc.XValues.[k]) * lsc.C1.[k]        
        

    /// Differentiate at point x
    let differentiate (lsc:LinearSplineCoef) x =
        let k = leftSegmentIdx lsc.XValues x
        lsc.C1.[k]        



