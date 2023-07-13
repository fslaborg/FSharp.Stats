namespace FSharp.Stats.Interpolation

open System

/// <summary>
///   Module to create linear splines from x,y coordinates. x,y coordinates are interpolated by straight lines between two knots.
/// </summary>
/// <remarks>Equivalent to interval-wise simple linear regression between any neighbouring pair of data.</remarks>
module LinearSpline = 
    
    open FSharp.Stats

    /// <summary>
    ///   Record type that contains the x-knots, intersects (C0) and slopes (C1) of each interval.
    /// </summary>
    /// <remarks>Equivalent to interval-wise simple linear regression between any neighbouring pair of data.</remarks>
    type LinearSplineCoef = {
        /// sample points (n+1), sorted ascending
        XValues : float []
        /// Zero order spline coefficients (n), corresponding to line intersects
        C0 : float []
        /// First order spline coefficients (n), corresponding to line slopes
        C1 : float []} with
            static member Create xValues c0 c1 = {
                XValues = xValues;
                C0      = c0;
                C1      = c1;
                }

    let internal leftSegmentIdx arr value = 
        let idx = 
            let tmp = Array.BinarySearch(arr, value)
            let idx = if tmp < 0 then ~~~tmp-1 else tmp
            idx
        Math.Min(arr.Length-2,Math.Max(idx, 0))

    
    /// <summary>
    ///   Returns the linear spline interpolation coefficients from x,y data that is sorted ascending according to x values.
    /// </summary>
    /// <param name="xData">Collection of ascending x values</param>
    /// <param name="yData">Collection of y values</param>
    /// <returns>x-values, intersects, and slopes of interpolating lines</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;2.;3.;4.;5.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;9.;13.;17.;18.;|]
    /// 
    /// // get slopes and intersects for interpolating straight lines
    /// let coefficients = 
    ///     Interpolation.initInterpolateSorted xData yData 
    /// </code> 
    /// </example>
    /// <remarks>The intersects (C0) correspond to the input y values.</remarks>
    let interpolateSorted (xData: array<float>) (yData: array<float>) =
        if xData.Length <> yData.Length then
            failwith "input arrays differ in length"
        if xData.Length < 2 then
            failwith "input arrays are too small to perform a spline interpolation"     

        let c1 =
            Array.init (xData.Length - 1) (fun i -> (yData.[i + 1] - yData.[i]) / (xData.[i + 1] - xData.[i]) )

        LinearSplineCoef.Create xData yData c1

    [<Obsolete("Use interpolateSorted instead!")>]
    let initInterpolateSorted xData yData= 
        interpolateSorted xData yData

    /// <summary>
    ///   Returns the linear spline interpolation coefficients from unsorted x,y data. Works in place and modifies input sequences!
    /// </summary>
    /// <param name="xData">Collection of x values. May be modified when running this function!</param>
    /// <param name="yData">Collection of y values. May be modified when running this function!</param>
    /// <returns>x-values, intersects, and slopes of interpolating lines</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get slopes and intersects for interpolating straight lines
    /// let coefficients = 
    ///     Interpolation.initInterpolateInplace xData yData 
    /// </code> 
    /// </example>
    /// <remarks>Works in place!</remarks>
    let interpolateInplace (xData: array<float>) (yData: array<float>) =
        if xData.Length <> yData.Length then
            failwith "input arrays differ in length"

        Array.sort2InPlaceByKeys 0 (xData.Length) xData yData
        interpolateSorted xData yData

    [<Obsolete("Use interpolateInplace instead!")>]
    let initInterpolateInplace xData yData= 
        interpolateInplace xData yData

    /// <summary>
    ///   Returns the linear spline interpolation coefficients from unsorted x,y data.
    /// </summary>
    /// <param name="xData">Collection of x values.</param>
    /// <param name="yData">Collection of y values.</param>
    /// <returns>x-values, intersects, and slopes of interpolating lines</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get slopes and intersects for interpolating straight lines
    /// let coefficients = 
    ///     Interpolation.initInterpolate xData yData 
    /// </code> 
    /// </example>
    let interpolate (xData: array<float>) (yData: array<float>) =
        if xData.Length <> yData.Length then
            failwith "input arrays differ in length"
        
        let x' = Array.copy xData
        let y' = Array.copy yData
        Array.sort2InPlaceByKeys 0 (x'.Length) x' y'
        interpolateSorted x' y'

    [<Obsolete("Use interpolate instead!")>]
    let initInterpolate xData yData= 
        interpolate xData yData

    /// <summary>
    ///   Predicts the y value at point x. A straight line is fitted between the neighboring x values given.
    /// </summary>
    /// <param name="lsc">Linear spline coefficients given as input x values, intersects, and slopes.</param>
    /// <param name="x">X value at which the corresponding y value should be predicted</param>
    /// <returns>Y value corresponding to the given x value.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get slopes and intersects for interpolating straight lines
    /// let coefficients = 
    ///     Interpolation.initInterpolate xData yData 
    ///
    /// // get y value at 3.4
    /// Interpolation.interpolate coefficients 3.4 
    /// </code> 
    /// </example>
    /// <remarks>X values that don't not lie within the range of the input x values, are predicted using the nearest interpolation line!</remarks>
    let predict (lsc: LinearSplineCoef) x =
        let k = leftSegmentIdx lsc.XValues x
        lsc.C0.[k] + (x - lsc.XValues.[k]) * lsc.C1.[k]        

    /// <summary>
    ///   Predicts the slope at point x. Since linear splines are lines between each pair of adjacend knots, the slope of the function within the interval of adjacent knots is constant.
    /// </summary>
    /// <param name="lsc">Linear spline coefficients given as input x values, intersects, and slopes.</param>
    /// <param name="x">X value at which the corresponding slope should be predicted</param>
    /// <returns>Slope of the function at the given x value.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get slopes and intersects for interpolating straight lines
    /// let coefficients = 
    ///     Interpolation.initInterpolate xData yData 
    ///
    /// // get slope at 3.4
    /// Interpolation.differentiate coefficients 3.4 
    /// </code> 
    /// </example>
    /// <remarks>X values that don't lie within the range of the input x values, are predicted using the nearest interpolation line!</remarks>
    let differentiate (lsc:LinearSplineCoef) x =
        let k = leftSegmentIdx lsc.XValues x
        lsc.C1.[k]
