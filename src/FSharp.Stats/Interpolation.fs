namespace FSharp.Stats

open System
open FSharp.Stats
open FSharp.Stats.Algebra

/// <summary>
///   This module contains functionalities to perform various interpolation methods for two dimensional data.
/// </summary>
module Interpolation = 

    /// <summary>
    ///   Calculates polynomials that interpolatethe two dimensional data. The polynomial order is equal to the number of data points - 1.
    /// </summary>
    /// <remarks>
    ///   In general a polynomial with degree = datapointNumber - 1 is flexible enough to interpolate all datapoints.
    ///   But polynomial regression with degree = datapointNumber - 1 cannot be used for polynomial interpolation 
    ///   because the least squares approach is not sufficient to converge interpolating.
    /// </remarks>
    module Polynomial =
    
        /// contains polynomial coefficients sorted from intercept to highest order factor
        type PolynomialCoef = {
            /// <summary>
            /// vector of polynomial coefficients sorted as [constant;linear;quadratic;...]
            /// </summary>
            C0_CX : Vector<float>
            } with 
                static member Create c = {C0_CX = c}
                /// <summary>
                ///   takes x value to predict the corresponding interpolating y value
                /// </summary>
                /// <param name="x">x value of which the corresponding y value should be predicted</param>
                /// <returns>predicted y value with given polynomial coefficients at X=x</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
                /// // e.g. temperature measured at noon of the days specified in xData 
                /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
                /// 
                /// // Estimate the polynomial coefficients. In Interpolation the order is equal to the data length - 1.
                /// let coefficients = 
                ///     Interpolation.Polynomial.coefficients xData yData 
                /// 
                /// // Predict the temperature value at midnight between day 1 and 2. 
                /// coefficients.Predict 1.5
                /// </code> 
                /// </example>
                member this.Predict = fun x -> this.C0_CX |> Vector.foldi (fun i acc c -> acc + (c * (pown x i))) 0.

        /// <summary>
        ///   Calculates the polynomial coefficients for interpolating the given unsorted data. 
        /// </summary>
        /// <remarks>No duplicates allowed!</remarks>
        /// <param name="xData">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
        /// <param name="yData">vector of y values</param>
        /// <returns>vector of polynomial coefficients sorted as [constant;linear;quadratic;...]</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
        /// // e.g. e.g. temperature measured at noon of the days specified in xData 
        /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
        /// 
        /// // Estimate the polynomial coefficients. In Interpolation the order is equal to the data length - 1.
        /// let coefficients = 
        ///     Interpolation.Polynomial.coefficients xData yData 
        /// </code> 
        /// </example>
        let interpolate (xData: Vector<float>) (yData: Vector<float>) =
            if xData.Length <> yData.Length then
                raise (System.ArgumentException("vector x and y have to be the same size!"))
            let order = xData.Length - 1
            let A =
                Matrix.init (order + 1) (order + 1) (fun i j  -> 
                    pown xData.[i] j
                    )
            let b = yData
            PolynomialCoef.Create (LinearAlgebra.SolveLinearSystem A b)
        
        /// <summary>
        ///   takes polynomial coefficients and x value to predict the corresponding interpolating y value
        /// </summary>
        /// <param name="coef">polynomial coefficients (e.g. determined by Polynomial.coefficients), sorted as [constant;linear;quadratic;...]</param>
        /// <param name="x">x value of which the corresponding y value should be predicted</param>
        /// <returns>predicted y value with given polynomial coefficients at X=x</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
        /// // e.g. temperature measured at noon of the days specified in xData 
        /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
        /// 
        /// // Estimate the polynomial coefficients. In Interpolation the order is equal to the data length - 1.
        /// let coefficients = 
        ///     Interpolation.Polynomial.coefficients xData yData 
        /// 
        /// // Predict the temperature value at midnight between day 1 and 2. 
        /// Interpolation.Polynomial.fit coefficients 1.5
        /// </code> 
        /// </example>
        let predict (coef: PolynomialCoef) (x:float) =
            //coef.C0_CX |> Vector.foldi (fun i acc c -> acc + (c * (pown x i))) 0.
            coef.Predict x

        /// <summary>
        ///   calculates derivative values at X=x with given polynomial coefficients. Level 1 = fst derivative; Level2 = snd derivative ...
        /// </summary>
        /// <param name="coef">polynomial coefficients (e.g. determined by Polynomial.coefficients), sorted as [constant;linear;quadratic;...]</param>
        /// <param name="level">depth of derivative: 1 = slope, 2 = curvature, ... </param>
        /// <param name="x">x value of which the corresponding y value should be predicted</param>
        /// <returns>predicted derivative with given polynomial coefficients at X=x</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
        /// // e.g. temperature measured at noon of the days specified in xData 
        /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
        /// 
        /// // Estimate the polynomial coefficients. In Interpolation the order is equal to the data length - 1.
        /// let coefficients = 
        ///     Interpolation.Polynomial.coefficients xData yData 
        /// 
        /// // Predict the curvature of the interpolating function at midnight between day 1 and 2. 
        /// Interpolation.Polynomial.getDerivative coefficients 2 1.5
        /// </code> 
        /// </example>
        let getDerivative (coef: PolynomialCoef) (level: int) (x: float) =
            let order = coef.C0_CX.Length - 1
            Array.init (order + 1) (fun i -> 
                let factor = 
                    List.init level (fun l -> i-l)
                    |> List.filter (not << isNan)
                    |> List.fold (fun acc c -> acc * (float c)) 1.
                factor * coef.C0_CX.[i] * (pown x (i-level))
                )
            |> Array.filter (not << isNan)
            |> Array.sum

        [<Obsolete("Use Polynomial.interpolate instead!")>]
        let coefficients xData yData= 
            (interpolate xData yData).C0_CX

        [<Obsolete("Use Polynomial.predict instead!")>]
        let fit coef x = 
            predict (PolynomialCoef.Create coef) x



    /// <summary>
    ///   Module to create linear splines from x,y coordinates. x,y coordinates are interpolated by straight lines between two knots.
    /// </summary>
    /// <remarks>Equivalent to interval-wise simple linear regression between any neighbouring pair of data.</remarks>
    module LinearSpline = 
    
        open FSharp.Stats

        let internal leftSegmentIdx arr value = 
            let idx = 
                let tmp = Array.BinarySearch(arr, value)
                let idx = if tmp < 0 then ~~~tmp-1 else tmp
                idx
            Math.Min(arr.Length-2,Math.Max(idx, 0))

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
                /// <summary>
                ///   Predicts the y value at point x. A straight line is fitted between the neighboring x values given.
                /// </summary>
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
                /// coefficients.Predict 3.4 
                /// </code> 
                /// </example>
                /// <remarks>X values that don't not lie within the range of the input x values, are predicted using the nearest interpolation line!</remarks>
                member this.Predict = 
                    fun x -> 
                        let k = leftSegmentIdx this.XValues x
                        this.C0.[k] + (x - this.XValues.[k]) * this.C1.[k] 

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
        /// <remarks>Must not contain duplicate x values. Use Approximation.regularizeValues to preprocess data!</remarks>
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
        /// <remarks>Must not contain duplicate x values. Use Approximation.regularizeValues to preprocess data!</remarks>
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
        /// <remarks>Must not contain duplicate x values. Use Approximation.regularizeValues to preprocess data!</remarks>
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
            //let k = leftSegmentIdx lsc.XValues x
            //lsc.C0.[k] + (x - lsc.XValues.[k]) * lsc.C1.[k]     
            lsc.Predict x

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
        let differentiate (lsc: LinearSplineCoef) x =
            let k = leftSegmentIdx lsc.XValues x
            lsc.C1.[k]


    /// <summary>
    ///   Module to create piecewise cubic polynomials (cubic subsplines) from x,y coordinates. 
    ///   Akima subsplines are more flexible than standard cubic splines because the are NOT continuous in the function curvature, thereby diminishing oscillating behaviour.
    /// </summary>
    module Akima =

        /// <summary>
        ///   Subsplines differ from regular splines because they are discontinuous in the second derivative.
        /// </summary>
        type SubSplineCoef = {
            /// sample points (N+1), sorted ascending
            XValues : float []
            /// Zero order spline coefficients (N)
            C0 : float []
            /// First order spline coefficients (N)
            C1 : float []
            /// Second order spline coefficients (N)
            C2 : float []
            /// Third order spline coefficients (N)
            C3 : float []
            } with 
                static member Create xValues c0 c1 c2 c3 = {
                    XValues=xValues;C0=c0;C1=c1;C2=c2;C3=c3 
                    }
                /// <summary>
                ///   Returns function that takes x value and predicts the corresponding interpolating y value. 
                /// </summary>
                /// <param name="xVal">X value of which the y value should be predicted.</param>
                /// <returns>Function that takes an x value and returns function value.</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|0.;1.;5.;4.;3.;|]
                /// // some measured feature
                /// let yData = vector [|1.;5.;4.;13.;17.|]
                ///
                /// // get coefficients for piecewise interpolating cubic polynomials
                /// let coefficients = 
                ///     Akima.interpolate xData yData
                ///
                /// // get function value at x=3.4
                /// coefficients.Predict 3.4
                /// </code> 
                /// </example>
                /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations! For reference see: http://www.dorn.org/uni/sls/kap06/f08_0204.htm.</remarks>
                member this.Predict = 
                    fun x ->             
                        let k = LinearSpline.leftSegmentIdx this.XValues x 
                        let x = x - this.XValues.[k]
                        this.C0.[k] + x*(this.C1.[k] + x*(this.C2.[k] + x*this.C3.[k]))


        /// <summary>
        ///   Computes coefficients for piecewise interpolating subsplines.
        /// </summary>
        /// <param name="xValues">x values that are sorted ascending. Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
        /// <param name="yValues">function value at x values</param>
        /// <param name="firstDerivatives">first derivatives at x values</param>
        /// <returns>Coefficients that define the interpolating function.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        ///
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     Akima.interpolate xData yData
        ///
        /// </code> 
        /// </example>
        /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations! For reference see: http://www.dorn.org/uni/sls/kap06/f08_0204.htm.</remarks>
        let interpolateHermiteSorted (xValues:float []) (yValues:float []) (firstDerivatives:float []) = 
            if xValues.Length <> yValues.Length || xValues.Length <> firstDerivatives.Length then
                failwith "input arrays differ in length"
            elif
                xValues.Length < 2 then
                failwith "input arrays are too small to perform a spline interpolation" 
    
            let c0 = Array.zeroCreate (xValues.Length-1)
            let c1 = Array.zeroCreate (xValues.Length-1)
            let c2 = Array.zeroCreate (xValues.Length-1)
            let c3 = Array.zeroCreate (xValues.Length-1)
            for i = 0 to (c0.Length-1) do
                let w = xValues.[i+1] - xValues.[i]
                let ww = w*w 
                c0.[i] <- yValues.[i] 
                c1.[i] <- firstDerivatives.[i]
                c2.[i] <- (3.*(yValues.[i+1] - yValues.[i])/w - 2. * firstDerivatives.[i] - firstDerivatives.[i+1])/w 
                c3.[i] <- (2.*(yValues.[i] - yValues.[i+1])/w +      firstDerivatives.[i] + firstDerivatives.[i+1])/ww 
            SubSplineCoef.Create xValues c0 c1 c2 c3

        /// <summary>
        ///   Computes coefficients for piecewise interpolating subsplines.
        /// </summary>
        /// <param name="xValues">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
        /// <param name="yValues">function value at x values</param>
        /// <returns>Coefficients that define the interpolating function.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        ///
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     Akima.interpolate xData yData
        ///
        /// </code> 
        /// </example>
        /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations! For reference see: http://www.dorn.org/uni/sls/kap06/f08_0204.htm.</remarks>
        let interpolate (xValues:float []) (yValues:float []) =
            if xValues.Length <> yValues.Length then
                failwith "input arrays differ in length"
            elif
                xValues.Length < 5 then
                failwith "input arrays are too small to perform a spline interpolation" 
            // prepare divided differences (diff) and weights (we)
            let diff = 
                let tmp = Array.zeroCreate (xValues.Length-1)
                for i = 0 to tmp.Length-1 do
                    tmp.[i] <- (yValues.[i+1] - yValues.[i])/(xValues.[i+1] - xValues.[i])
                tmp
            let we  = 
                let tmp = Array.zeroCreate (xValues.Length-1)
                for i = 1 to tmp.Length-1 do
                    tmp.[i] <- (diff.[i]-diff.[i-1]) |> abs 
                tmp
            // prepare Hermite interpolation scheme   
            let dd = 
                let tmp = Array.zeroCreate xValues.Length
                for i = 2 to tmp.Length-3 do 
                    let ddi = 
                        if Precision.almostEqualNorm we.[i-1] 0.0 && Precision.almostEqualNorm we.[i+1] 0.0 then
                            (((xValues.[i + 1] - xValues.[i])*diff.[i - 1]) + ((xValues.[i] - xValues.[i - 1])*diff.[i]))/(xValues.[i + 1] - xValues.[i - 1])
                        else
                            ((we.[i + 1]*diff.[i - 1]) + (we.[i - 1]*diff.[i]))/(we.[i + 1] + we.[i - 1])
                    tmp.[i] <- ddi 
                tmp.[0]          <- Integration.Differentiation.differentiateThreePoint xValues yValues 0 0 1 2
                tmp.[1]          <- Integration.Differentiation.differentiateThreePoint xValues yValues 1 0 1 2 
                tmp.[xValues.Length-2] <- Integration.Differentiation.differentiateThreePoint xValues yValues (xValues.Length-2) (xValues.Length-3) (xValues.Length-2) (xValues.Length-1)
                tmp.[xValues.Length-1] <- Integration.Differentiation.differentiateThreePoint xValues yValues (xValues.Length-2) (xValues.Length-3) (xValues.Length-2) (xValues.Length-1)
                tmp
            interpolateHermiteSorted xValues yValues dd
    
        /// <summary>
        ///   Returns function that takes x value and predicts the corresponding interpolating y value. 
        /// </summary>
        /// <param name="splineCoeffs">Interpolation functions coefficients.</param>
        /// <param name="xVal">X value of which the y value should be predicted.</param>
        /// <returns>Function that takes an x value and returns function value.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        ///
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     Akima.interpolate xData yData
        ///
        /// // get function to predict y value
        /// let interpolFunc = 
        ///     Akima.predict coefficients
        ///
        /// // get function value at x=3.4
        /// interpolFunc 3.4
        /// </code> 
        /// </example>
        /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations! For reference see: http://www.dorn.org/uni/sls/kap06/f08_0204.htm.</remarks>
        let predict (splineCoeffs: SubSplineCoef) xVal =
            //let k = LinearSpline.leftSegmentIdx splineCoeffs.XValues xVal 
            //let x = xVal - splineCoeffs.XValues.[k]
            //splineCoeffs.C0.[k] + x*(splineCoeffs.C1.[k] + x*(splineCoeffs.C2.[k] + x*splineCoeffs.C3.[k]))
            splineCoeffs.Predict xVal

        /// <summary>
        ///   Returns function that takes x value and predicts the corresponding slope. 
        /// </summary>
        /// <param name="splineCoeffs">Interpolation functions coefficients.</param>
        /// <param name="xVal">X value of which the slope should be predicted.</param>
        /// <returns>Function that takes an x value and returns slope.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        ///
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     Akima.interpolate xData yData
        ///
        /// // get function to predict slope
        /// let interpolFunc = 
        ///     Akima.getFirstDerivative coefficients
        ///
        /// // get slope at x=3.4
        /// interpolFunc 3.4
        /// </code> 
        /// </example>
        /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations! For reference see: http://www.dorn.org/uni/sls/kap06/f08_0204.htm.</remarks>
        let getFirstDerivative (splineCoeffs: SubSplineCoef) xVal =
            let k = LinearSpline.leftSegmentIdx splineCoeffs.XValues xVal 
            let x = xVal - splineCoeffs.XValues.[k]
            splineCoeffs.C1.[k] + x*(2.*splineCoeffs.C2.[k] + x*3.*splineCoeffs.C3.[k])

        /// <summary>
        ///   Returns function that takes x value and predicts the corresponding interpolating curvature. 
        /// </summary>
        /// <param name="splineCoeffs">Interpolation functions coefficients.</param>
        /// <param name="xVal">X value of which the curvature should be predicted.</param>
        /// <returns>Function that takes an x value and returns curvature.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        ///
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     Akima.interpolate xData yData
        ///
        /// // get function to predict curvature
        /// let interpolFunc = 
        ///     Akima.getSecondDerivative coefficients
        ///
        /// // get curvature at x=3.4
        /// interpolFunc 3.4
        /// </code> 
        /// </example>
        /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations! For reference see: http://www.dorn.org/uni/sls/kap06/f08_0204.htm.</remarks>
        let getSecondDerivative (splineCoeffs: SubSplineCoef) xVal =
            let k = LinearSpline.leftSegmentIdx splineCoeffs.XValues xVal 
            let x = xVal - splineCoeffs.XValues.[k]
            2.*splineCoeffs.C2.[k] + x*6.*splineCoeffs.C3.[k]

        let internal computeIndefiniteIntegral (splineCoeffs: SubSplineCoef) = 
            let integral = 
                let tmp = Array.zeroCreate splineCoeffs.C0.Length
                for i = 0 to tmp.Length-2 do
                    let w = splineCoeffs.XValues.[i+1] - splineCoeffs.XValues.[i] 
                    tmp.[i+1] <- tmp.[i] + w*(splineCoeffs.C0.[i] + w*(splineCoeffs.C1.[i]/2. + w*(splineCoeffs.C2.[i]/3. + w*splineCoeffs.C3.[i]/4.)))
                tmp
            integral

        /// <summary>
        ///   Returns integral from interpolating function from x=0 to x=xVal. 
        /// </summary>
        /// <param name="splineCoeffs">Interpolation functions coefficients.</param>
        /// <param name="xVal">X value up to which the integral should be calculated.</param>
        /// <returns>Integral (area under the curve) from x=0 to x=xVal</returns>
        let integrate (splineCoeffs: SubSplineCoef) xVal = 
            let integral = computeIndefiniteIntegral splineCoeffs 
            let k = LinearSpline.leftSegmentIdx splineCoeffs.XValues xVal 
            let x = xVal - splineCoeffs.XValues.[k]
            integral.[k] + x*(splineCoeffs.C0.[k] + x*(splineCoeffs.C1.[k]/2. + x*(splineCoeffs.C2.[k]/3. + x*splineCoeffs.C3.[k]/4.)))
        
        /// <summary>
        ///   Returns integral from interpolating function from x=xVal1 to x=xVal2. 
        /// </summary>
        /// <param name="splineCoeffs">Interpolation functions coefficients.</param>
        /// <param name="xVal1">X value from where the integral should be calculated.</param>
        /// <param name="xVal2">X value up to which the integral should be calculated.</param>
        /// <returns>Integral (area under the curve) from x=xVal1 to x=xVal2</returns>
        let definiteIntegral (integrateF: float -> float) xVal1 xVal2 =
            integrateF xVal2 - integrateF xVal1 



    /// <summary>
    ///   Cubic splines interpolate two dimensional data by applying piecewise cubic polynomials that are continuous at the input coordinates (knots).
    ///   The function itself, the first- and second derivative are continuous at the knots.
    /// </summary>
    module CubicSpline = 
        open FSharp.Stats

        type BoundaryCondition =
            ///<summary>most used spline variant: f'' at borders is set to 0</summary>
            | Natural
            ///f' at first point is the same as f' at the last point
            | Periodic
            ///f'' at first/second and last/penultimate knot are equal
            | Parabolic
            ///f''' at second and penultimate knot are continuous
            | NotAKnot
            ///first and last polynomial are quadratic, not cubic
            | Quadratic
            ///f' at first and last knot are set by user
            | Clamped
    
        /// <summary>
        /// Contains x data, y data (c0), slopes (c1), curvatures (c2), and the third derivative at each knot
        /// </summary>
        type CubicSplineCoef = {
            XData : vector
            /// <summary>
            /// vector of [a0;b0;c0;d0;a1;b1;...;d(n-2)] where f_n(x) = (an)x^3 + (bn)x^2 + (cn)x + (dn)
            /// </summary>
            C0_3 : vector} with
                static member Create x c = {XData=x;C0_3=c}

                /// <summary>
                ///   Returns function that takes x value (that lies within the range of input x values) and predicts the corresponding interpolating y value.
                /// </summary>
                /// <param name="x">X value of which the y value should be predicted.</param>
                /// <returns>Function that takes an x value and returns function value.</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|0.;1.;5.;4.;3.;|]
                /// // some measured feature
                /// let yData = vector [|1.;5.;4.;13.;17.|]
                /// 
                /// // get coefficients for piecewise interpolating cubic polynomials
                /// let coefficients = 
                ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
                ///
                /// // get function value at x=3.4
                /// coefficients.PredictWithinRange 3.4
                ///
                /// </code> 
                /// </example>
                /// <remarks>Only defined within the range of the given xValues!</remarks>
                member this.PredictWithinRange =
                    fun x ->             
                        let sortedX = this.XData |> Seq.sort
                        let intervalNumber =
                
                            if x > Seq.last sortedX || x < Seq.head sortedX then 
                                failwith "Spline is not defined outside of the interval of the xValues"
                
                            if x = Seq.last sortedX then 
                                Seq.length sortedX - 2
                            else
                                sortedX
                                |> Seq.findIndex(fun xKnot -> (xKnot - x) > 0.)
                                |> fun nextInterval -> nextInterval - 1
            
                        let yValue = 
                            this.C0_3.[4 * intervalNumber + 0] * (pown x 3) +    //a*x³
                            this.C0_3.[4 * intervalNumber + 1] * (pown x 2) +    //b*x²
                            this.C0_3.[4 * intervalNumber + 2] * x          +    //c*x
                            this.C0_3.[4 * intervalNumber + 3]                   //d
            
                        yValue
                        
                /// <summary>
                ///   Returns function that takes x value and predicts the corresponding interpolating y value.
                /// </summary>
                /// <param name="x">X value of which the y value should be predicted.</param>
                /// <returns>Function that takes an x value and returns function value.</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|0.;1.;5.;4.;3.;|]
                /// // some measured feature
                /// let yData = vector [|1.;5.;4.;13.;17.|]
                /// 
                /// // get coefficients for piecewise interpolating cubic polynomials
                /// let coefficients = 
                ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
                ///
                /// // get function value at x=3.4
                /// coefficients.Predict 3.4
                ///
                /// </code> 
                /// </example>
                /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
                member this.Predict =
                    fun x -> 
                        let sortedX = this.XData |> Seq.sort
                        let xHead = this.XData |> Seq.head
                        let xLast = this.XData |> Seq.last
            
                        let intercept intervalNumber x = 
                            this.C0_3.[4 * intervalNumber + 0] * (pown x 3) +    //a*x³
                            this.C0_3.[4 * intervalNumber + 1] * (pown x 2) +    //b*x²
                            this.C0_3.[4 * intervalNumber + 2] * x          +    //c*x
                            this.C0_3.[4 * intervalNumber + 3]                   //d           
            
                        let slope intervalNumber x = 
                            3. * this.C0_3.[4 * intervalNumber + 0] * (pown x 2) +   //3a*x²
                            2. * this.C0_3.[4 * intervalNumber + 1] * x +            //2b*x
                            this.C0_3.[4 * intervalNumber + 2]                       //c

                        if x >= Seq.last sortedX then 
                            let intervalNr = Seq.length sortedX - 2
                            let diffX = x - xLast
                            let y = intercept intervalNr xLast + diffX * (slope intervalNr xLast)
                            y
                        elif x < Seq.head sortedX then 
                            let intervalNr = 0
                            let diffX = x - xHead
                            let y = intercept intervalNr xHead + diffX * (slope intervalNr xHead)
                            y
                        else
                            this.PredictWithinRange x

        /// <summary>
        ///   Computes coefficients for piecewise interpolating splines. In the form of [a0;b0;c0;d0;a1;b1;...;d(n-2)]. 
        ///   where: fn(x) = (an)x^3+(bn)x^2+(cn)x+(dn)
        /// </summary>
        /// <param name="boundaryCondition">Condition that defines how slopes and curvatures are defined at the left- and rightmost knot</param>
        /// <param name="xValues">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
        /// <param name="yValues">function value at x values</param>
        /// <returns>Coefficients that define the interpolating function.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        ///
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
        /// </code> 
        /// </example>
        let interpolate (boundaryCondition: BoundaryCondition) (xValues: Vector<float>) (yValues: Vector<float>) =
            //f(x)   = ax³+bx²+cx+d
            //f'(x)  = 3ax²+2bx+c
            //f''(x) = 6ax+2b

            let (xVal,yVal) =
                let indices =
                    xValues
                    |> Seq.indexed
                    |> Seq.sortBy snd
                    |> Seq.map fst
                let sortedXValues = indices |> Seq.map (fun i -> xValues.[i]) |> vector
                let sortedYValues = indices |> Seq.map (fun i -> yValues.[i]) |> vector
                sortedXValues,sortedYValues

            let intervalNumber = xVal.Length - 1

            let interpolatingConstraints intervalIndex (x:float) (y:float) =
                let tmp = Array.init (4 * intervalNumber) (fun x -> 0.)
                tmp.[4 * intervalIndex + 0] <- pown x 3  
                tmp.[4 * intervalIndex + 1] <- pown x 2 
                tmp.[4 * intervalIndex + 2] <- x
                tmp.[4 * intervalIndex + 3] <- 1.
                (tmp,y)

            let firstDerivativeConstraints intervalIndex x =
                let tmp = Array.init (4 * intervalNumber) (fun x -> 0.)
                let f'a = 3. * (pown x 2)
                let f'b = 2. * x
                let f'c = 1.
                let f'd = 0.
                tmp.[4 * intervalIndex + 0] <- f'a
                tmp.[4 * intervalIndex + 1] <- f'b
                tmp.[4 * intervalIndex + 2] <- f'c
                //tmp.[4 * intervalIndex + 3] <- f'd
                tmp.[4 * intervalIndex + 4] <- - f'a
                tmp.[4 * intervalIndex + 5] <- - f'b
                tmp.[4 * intervalIndex + 6] <- - f'c
                //tmp.[4 * intervalIndex + 7] <- - f'd
                (tmp,0.)

            let secondDerivativeConstraints intervalIndex x =
                let tmp = Array.init (4 * intervalNumber) (fun x -> 0.)
                let f''a = 6. * x
                let f''b = 2.
                let f''c = 0.
                let f''d = 0.
                tmp.[4 * intervalIndex + 0] <- f''a
                tmp.[4 * intervalIndex + 1] <- f''b
                //tmp.[4 * intervalIndex + 2] <- f''c
                //tmp.[4 * intervalIndex + 3] <- f''d
                tmp.[4 * intervalIndex + 4] <- - f''a
                tmp.[4 * intervalIndex + 5] <- - f''b
                //tmp.[4 * intervalIndex + 6] <- - f''c
                //tmp.[4 * intervalIndex + 7] <- - f''d
                (tmp,0.)
            
            let boundaryCondition = 
                let firstX = xVal.[0]
                let secondX = xVal.[1]
                let lastX = xVal.[intervalNumber]
                let penultimate = xVal.[intervalNumber - 1]
                let tmp1 = Array.init (4 * intervalNumber) (fun x -> 0.)
                let tmp2 = Array.init (4 * intervalNumber) (fun x -> 0.)
                match boundaryCondition with
                | Natural ->
                    //first condition: f''0(x0) = 0
                    tmp1.[0] <- 6. * firstX
                    tmp1.[1] <- 2.
                    //tmp.[2] <- 0.
                    //tmp.[3] <- 0.

                    //second condition: f''n-1(xn) = 0
                    tmp2.[4 * (intervalNumber - 1) + 0] <- 6. * lastX
                    tmp2.[4 * (intervalNumber - 1) + 1] <- 2.
                    //tmp2.[4 * (intervalNumber - 1) + 2] <- 0.
                    //tmp2.[4 * (intervalNumber - 1) + 3] <- 0.

                    [|(tmp1,0.);(tmp2,0.)|]

                | Periodic ->
                    //first conditionf'0(x0)-f'n-1(xn) = 0
                    tmp1.[0] <- 3. * (pown firstX 2)
                    tmp1.[1] <- 2. * firstX
                    tmp1.[2] <- 1.
                    tmp1.[4 * (intervalNumber - 1) + 0] <- -3. * (pown lastX 2)
                    tmp1.[4 * (intervalNumber - 1) + 1] <- -2. * lastX
                    tmp1.[4 * (intervalNumber - 1) + 2] <- -1.

                    //second condition: f''0(x0)-f''n-1(xn) = 0
                    tmp2.[0] <- 6. * firstX
                    tmp2.[1] <- 2. 
                    tmp2.[4 * (intervalNumber - 1) + 0] <- -6. * lastX
                    tmp2.[4 * (intervalNumber - 1) + 1] <- -2. 

                    [|(tmp1,0.);(tmp2,0.)|]

                | Parabolic -> 
                    //first condition: f''0(x0) - f''0(x1) = 0
                    tmp1.[0] <- 6. * firstX
                    tmp1.[1] <- 2.
                    tmp1.[4] <- -6. * secondX
                    tmp1.[5] <- -2.
                
                    //second condition: f''n-1(x(n-1)) - f''n-1(xn) = 0
                    tmp2.[4 * (intervalNumber - 1) + 0] <- 6. * lastX
                    tmp2.[4 * (intervalNumber - 1) + 1] <- 2. 
                    tmp2.[4 * (intervalNumber - 2) + 0] <- -6. * penultimate
                    tmp2.[4 * (intervalNumber - 2) + 1] <- -2. 
                
                    [|(tmp1,0.);(tmp2,0.)|]
                
                | NotAKnot ->
                    //first condition: f'''0(x1) - f'''1(x1) = 0
                    tmp1.[0] <- 1.
                    tmp1.[4] <- -1.
                
                    //second condition: f'''n-1(x(n-1)) - f'''n-2(x(n-1)) = 0
                    tmp2.[4 * (intervalNumber - 1) + 0] <- 1.
                    tmp2.[4 * (intervalNumber - 2) + 0] <- -1.
                
                    [|(tmp1,0.);(tmp2,0.)|]

                | Quadratic ->
                    //first condition: a1 = 0
                    tmp1.[0] <- 1.
                
                    //second condition: an = 0.
                    tmp2.[4 * (intervalNumber - 1) + 0] <- 1.
                
                    [|(tmp1,0.);(tmp2,0.)|]
                
                | Clamped -> //user defined border f''
                    failwith "Not implemented yet. Slopes m1 and m2 have to be set by user"
                    ////first condition: f''0(x0) = m1
                    //tmp1.[0] <- 6. * firstX
                    //tmp1.[1] <- 2.
                    ////second condition: f''n-1(xn) = m2
                    //tmp2.[4 * (intervalNumber - 1) + 0] <- 6. * lastX
                    //tmp2.[4 * (intervalNumber - 1) + 1] <- 2.
                    //[|(tmp1,m1);(tmp2,m2)|]

            let (equations,solutions) =
                let interPolConstraints =
                    [|0 .. intervalNumber - 1|]
                    |> Array.map (fun i -> 
                        [|
                        interpolatingConstraints i xVal.[i] yVal.[i]
                        interpolatingConstraints i xVal.[i+1] yVal.[i+1]
                        |])
                        |> Array.concat

                let derivativeConstraints =
                    [|0 .. intervalNumber - 2|]
                    |> Array.map (fun i -> 
                        [|
                        firstDerivativeConstraints  i xVal.[i+1]
                        secondDerivativeConstraints i xVal.[i+1]
                        |])
                    |> Array.concat
                
                [|interPolConstraints;derivativeConstraints;boundaryCondition|]
                |> Array.concat
                |> Array.unzip
                
            let A = Matrix.ofJaggedArray equations
            let b = Vector.ofArray solutions

            let coeffs = Algebra.LinearAlgebra.SolveLinearSystem A b 
            CubicSplineCoef.Create xValues coeffs

        /// <summary>
        ///   Returns function that takes x value (that lies within the range of input x values) and predicts the corresponding interpolating y value.
        /// </summary>
        /// <param name="coefficients">Interpolation functions coefficients.</param>
        /// <param name="x">X value of which the y value should be predicted.</param>
        /// <returns>Function that takes an x value and returns function value.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        /// 
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
        ///
        /// // get interpolating value
        /// let func = CubicSpline.predictWithinRange(coefLinSpl)
        ///
        /// // get function value at x=3.4
        /// func 3.4
        ///
        /// </code> 
        /// </example>
        /// <remarks>Only defined within the range of the given xValues!</remarks>
        let predictWithinRange (coefficients: CubicSplineCoef) x =
            coefficients.PredictWithinRange x

        /// <summary>
        ///   Returns function that takes x value and predicts the corresponding interpolating y value.
        /// </summary>
        /// <param name="coefficients">Interpolation functions coefficients.</param>
        /// <param name="x">X value of which the y value should be predicted.</param>
        /// <returns>Function that takes an x value and returns function value.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        /// 
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
        ///
        /// // get interpolating function 
        /// let func = CubicSpline.predict(coefLinSpl)
        ///
        /// // get function value at x=3.4
        /// func 3.4
        ///
        /// </code> 
        /// </example>
        /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
        let predict (coefficients: CubicSplineCoef) x =
            coefficients.Predict x

        let internal getDerivative order (coefficients: CubicSplineCoef) x =
            let sortedX = coefficients.XData |> Seq.sort
            let intervalNumber =
                if x >= Seq.last sortedX then 
                    Seq.length sortedX - 2
                elif x < Seq.head sortedX then 
                    0
                else
                    sortedX
                    |> Seq.findIndex(fun xKnot -> (xKnot - x) > 0.)
                    |> fun nextInterval -> nextInterval - 1            
            match order with
            | d when d = 1 ->
                let firstDerivative = 
                    3. * coefficients.C0_3.[4 * intervalNumber + 0] * (pown x 2) +   //3a*x²
                    2. * coefficients.C0_3.[4 * intervalNumber + 1] * x +            //2b*x
                    coefficients.C0_3.[4 * intervalNumber + 2]                       //c          
                firstDerivative
            | d when d = 2 ->
                let secondDerivative = 
                    6. * coefficients.C0_3.[4 * intervalNumber + 0] * x +   //6ax
                    2. * coefficients.C0_3.[4 * intervalNumber + 1]         //2b      
                secondDerivative
            | d when d = 3 ->
                let thirdDerivative = 
                    6. * coefficients.C0_3.[4 * intervalNumber + 0]  //6a     
                thirdDerivative
            | _ -> failwithf "for cubic splines no derivative > 3 is defined"

        /// <summary>
        ///   Returns function that takes x value and predicts the corresponding slope.
        /// </summary>
        /// <param name="coefficients">Interpolation functions coefficients.</param>
        /// <param name="x">X value of which the slope should be predicted.</param>
        /// <returns>Function that takes an x value and returns the function slope.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        /// 
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
        ///
        /// // get slope function
        /// let func = CubicSpline.getFirstDerivative(coefLinSpl)
        ///
        /// // get slope at x=3.4
        /// func 3.4
        ///
        /// </code> 
        /// </example>
        /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
        let getFirstDerivative (coefficients: CubicSplineCoef) x =
            getDerivative 1 coefficients x

        /// <summary>
        ///   Returns function that takes x value and predicts the corresponding curvature.
        /// </summary>
        /// <param name="coefficients">Interpolation functions coefficients.</param>
        /// <param name="x">X value of which the curvature should be predicted.</param>
        /// <returns>Function that takes an x value and returns the function curvature.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        /// 
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
        ///
        /// // get curvature function
        /// let func = CubicSpline.getSecondDerivative(coefLinSpl)
        ///
        /// // get curvature at x=3.4
        /// func 3.4
        ///
        /// </code> 
        /// </example>
        /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
        let getSecondDerivative (coefficients: CubicSplineCoef) x =
            getDerivative 2 coefficients x    
        
        /// <summary>
        ///   Returns function that takes x value and predicts the corresponding third derivative.
        /// </summary>
        /// <param name="coefficients">Interpolation functions coefficients.</param>
        /// <param name="x">X value of which the y value should be predicted.</param>
        /// <returns>Function that takes an x value and returns the function third derivative.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        /// 
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
        ///
        /// // get third derivative function
        /// let func = CubicSpline.getThirdDerivative(coefLinSpl)
        ///
        /// // get third derivative at x=3.4
        /// func 3.4
        ///
        /// </code> 
        /// </example>
        /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
        let getThirdDerivative (coefficients: CubicSplineCoef) x =
            getDerivative 3 coefficients x

        /// <summary>
        /// Hermite cubic splines are defined by the function values and their slopes (first derivatives). If the slopws are unknown, they must be estimated.
        /// </summary>
        module Hermite =

            type HermiteCoef = {
                /// sample points, sorted ascending
                XValues : vector
                /// Zero order spline coefficients, intersects, y values
                YValues : vector
                /// First order spline coefficients, slopes at knots
                Slopes : vector
                } with 
                    static member Create xValues yValues slopes  = {
                        XValues=xValues;YValues=yValues;Slopes=slopes
                        }

                    /// <summary>
                    ///   Returns function that takes x value and predicts the corresponding interpolating y value. 
                    /// </summary>
                    /// <param name="x">X value of which the y value should be predicted.</param>
                    /// <returns>Function that takes an x value and returns function value.</returns>
                    /// <example> 
                    /// <code> 
                    /// // e.g. days since a certain event
                    /// let xData = vector [|0.;1.;5.;4.;3.;|]
                    /// // some measured feature
                    /// let yData = vector [|1.;5.;4.;13.;17.|]
                    /// 
                    /// // get coefficients for piecewise interpolating cubic polynomials
                    /// let coefficients = 
                    ///     CubicSpline.Hermite.interpolate xData yData
                    ///
                    /// // get function value at x=3.4
                    /// coefficients.Predict 3.4
                    /// </code> 
                    /// </example>
                    /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
                    member this.Predict = 
                        fun x ->                 
                            let n = this.XValues.Length

                            let phi0 t tAdd1 x =
                                let tmp = (x - t) / (tAdd1 - t)
                                2. * (pown tmp 3) - 3. * (pown tmp 2) + 1.
                           
                            let phi1 t tAdd1 x =
                                let tmp = (x - t) / (tAdd1 - t)
                                - 2. * (pown tmp 3) + 3. * (pown tmp 2)    

                            let psi0 t tAdd1 x =
                                let tmp = (x - t) / (tAdd1 - t)
                                let a = tAdd1 - t
                                let b = (pown tmp 3) - 2. * (pown tmp 2) + tmp
                                a * b
                
                            let psi1 t tAdd1 x =
                                let tmp = (x - t) / (tAdd1 - t)
                                let a = tAdd1 - t
                                let b = (pown tmp 3) - (pown tmp 2)
                                a * b 

                            let calculate index x =
                                let ph0 = this.YValues.[index]    * phi0 this.XValues.[index] this.XValues.[index+1] x
                                let ph1 = this.YValues.[index+1]  * phi1 this.XValues.[index] this.XValues.[index+1] x
                                let ps0 = this.Slopes.[index]   * psi0 this.XValues.[index] this.XValues.[index+1] x
                                let ps1 = this.Slopes.[index+1] * psi1 this.XValues.[index] this.XValues.[index+1] x
                                ph0 + ph1 + ps0 + ps1

                            if x = Seq.last this.XValues then 
                                Seq.last this.YValues
                            else                 
                                let i = 
                                    match Array.tryFindIndexBack (fun xs -> xs <= x) (this.XValues |> Vector.toArray) with 
                                    | Some x -> x 
                                    | None   -> failwith "The given xValue is out of the range defined in xData"
                                calculate i x
                            

            /// <summary>
            ///   Computes coefficients for piecewise interpolating splines.
            ///   The x data has to be sorted ascending.
            /// </summary>
            /// <param name="xData">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
            /// <param name="yData">function value at x values</param>
            /// <returns>Coefficients that define the interpolating function.</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since a certain event
            /// let xData = vector [|0.;1.;5.;4.;3.;|]
            /// // some measured feature
            /// let yData = vector [|1.;5.;4.;13.;17.|]
            ///
            /// // get coefficients for piecewise interpolating cubic polynomials
            /// let coefficients = 
            ///     CubicSpline.Hermite.interpolate xData yData
            ///
            /// </code> 
            /// </example>
            /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations!</remarks>
            let interpolate (xData: Vector<float>) (yData: Vector<float>) = 
                let slopes = 
                    Vector.init xData.Length (fun i ->
                        if i = 0 then
                            (yData.[i] - yData.[i+1]) / (xData.[i] - xData.[i+1])
                        elif i = xData.Length - 1 then 
                            (yData.[i] - yData.[i-1]) / (xData.[i] - xData.[i-1])
                        else 
                            let s1 = (yData.[i] - yData.[i+1]) / (xData.[i] - xData.[i+1])
                            let s2 = (yData.[i] - yData.[i-1]) / (xData.[i] - xData.[i-1])
                            (s1 + s2) / 2.
                        )
                HermiteCoef.Create xData yData slopes

            /// <summary>
            ///   Computes coefficients for piecewise interpolating splines. 
            ///   If the knots are monotone in/decreasing, the spline also is monotone (http://www.korf.co.uk/spline.pdf)        
            ///   The x data has to be sorted ascending
            /// </summary>
            /// <param name="yData">function value at x values</param>
            /// <returns>Coefficients that define the interpolating function.</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since a certain event
            /// let xData = vector [|0.;1.;5.;4.;3.;|]
            /// // some measured feature
            /// let yData = vector [|1.;5.;6.;13.;13.1|]
            ///
            /// // get coefficients for piecewise interpolating cubic polynomials
            /// let coefficients = 
            ///     CubicSpline.Hermite.interpolateTryMonotonicity xData yData
            ///
            /// </code> 
            /// </example>
            /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations!</remarks>
            let interpolateTryMonotonicity (xData: Vector<float>) (yData: Vector<float>) =
                let calcSlope i =
                    let s1 = (xData.[i+1] - xData.[i]) / (yData.[i+1] - yData.[i])
                    let s2 = (xData.[i] - xData.[i-1]) / (yData.[i] - yData.[i-1])
                    2. / (s1 + s2)

                let rec loop i acc =
                    if i = xData.Length - 1 then
                        let s1 = (3. * (yData.[i] - yData.[i-1])) / (2. * (xData.[i] - xData.[i-1]))
                        let s2 = calcSlope (i-1) / 2.
                        let tmp = s1 - s2
                        (tmp::acc) |> List.rev
                    else 
                        let tmp = calcSlope i
                        if ((yData.[i] - yData.[i-1]) * (yData.[i+1] - yData.[i])) <= 0. then 
                            loop (i+1) (0.::acc)
                        else 
                            loop (i+1) (tmp::acc)

                let slopeAtFstKnot = 
                    let s1 = (3. * (yData.[1] - yData.[0])) / (2. * (xData.[1] - xData.[0]))
                    let s2 = calcSlope 1 / 2.
                    let slope = s1 - s2
                    slope
 
                let slopes = loop 1 [slopeAtFstKnot] 
                HermiteCoef.Create xData yData (slopes |> vector)


            /// <summary>
            ///   Returns function that takes x value and predicts the corresponding interpolating y value. 
            /// </summary>
            /// <param name="coefficients">Interpolation functions coefficients.</param>
            /// <param name="x">X value of which the y value should be predicted.</param>
            /// <returns>Function that takes an x value and returns function value.</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since a certain event
            /// let xData = vector [|0.;1.;5.;4.;3.;|]
            /// // some measured feature
            /// let yData = vector [|1.;5.;4.;13.;17.|]
            /// 
            /// // get coefficients for piecewise interpolating cubic polynomials
            /// let coefficients = 
            ///     CubicSpline.Hermite.interpolate xData yData
            ///
            /// // get interpolating function 
            /// let func = CubicSpline.Hermite.predict coefLinSpl
            ///
            /// // get function value at x=3.4
            /// func 3.4
            /// </code> 
            /// </example>
            /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
            let predict (coef: HermiteCoef) x =
                coef.Predict x

            ///calculates the slopes by averaging the slopes of neighbouring tangents
            [<Obsolete("Use interpolate instead to get slopes")>]
            let getSimpleSlopes (xData: Vector<float>) (yData: Vector<float>) = 
                Vector.init xData.Length (fun i ->
                    if i = 0 then
                        (yData.[i] - yData.[i+1]) / (xData.[i] - xData.[i+1])
                    elif i = xData.Length - 1 then 
                        (yData.[i] - yData.[i-1]) / (xData.[i] - xData.[i-1])
                    else 
                        let s1 = (yData.[i] - yData.[i+1]) / (xData.[i] - xData.[i+1])
                        let s2 = (yData.[i] - yData.[i-1]) / (xData.[i] - xData.[i-1])
                        (s1 + s2) / 2.
                    )

            ///if the knots are monotone in/decreasing, the spline also is monotone (http://www.korf.co.uk/spline.pdf)
            [<Obsolete("Use interpolateTryMonotonicity instead")>]
            let getSlopesTryMonotonicity (xData: Vector<float>) (yData: Vector<float>) =
                let calcSlope i =
                    let s1 = (xData.[i+1] - xData.[i]) / (yData.[i+1] - yData.[i])
                    let s2 = (xData.[i] - xData.[i-1]) / (yData.[i] - yData.[i-1])
                    2. / (s1 + s2)

                let rec loop i acc =
                    if i = xData.Length - 1 then
                        let s1 = (3. * (yData.[i] - yData.[i-1])) / (2. * (xData.[i] - xData.[i-1]))
                        let s2 = calcSlope (i-1) / 2.
                        let tmp = s1 - s2
                        (tmp::acc) |> List.rev
                    else 
                        let tmp = calcSlope i
                        if ((yData.[i] - yData.[i-1]) * (yData.[i+1] - yData.[i])) <= 0. then 
                            loop (i+1) (0.::acc)
                        else 
                            loop (i+1) (tmp::acc)

                let slopeAtFstKnot = 
                    let s1 = (3. * (yData.[1] - yData.[0])) / (2. * (xData.[1] - xData.[0]))
                    let s2 = calcSlope 1 / 2.
                    let slope = s1 - s2
                    slope
 
                let slopes = loop 1 [slopeAtFstKnot] 
                slopes |> vector

        [<Obsolete("Use Interpolation.CubicSpline instead!")>]
        module Simple = 
    
            [<Obsolete("Use Interpolation.CubicSpline.coefficients instead!")>]
            let coefficients (boundaryCondition: BoundaryCondition) (xValues: Vector<float>) (yValues: Vector<float>) =
                interpolate boundaryCondition xValues yValues 
    
            [<Obsolete("Use Interpolation.CubicSpline.predictWithinRange instead!")>]
            let fit (coefficients: Vector<float>) (xValues: Vector<float>) x =
                predictWithinRange (CubicSplineCoef.Create xValues coefficients) x
    
            [<Obsolete("Use Interpolation.CubicSpline.predict instead!")>]
            let fitWithLinearPrediction (coefficients: Vector<float>) (xValues: Vector<float>) x =
                predict (CubicSplineCoef.Create xValues coefficients) x
    
            [<Obsolete("Use Interpolation.CubicSpline.getFirstDerivative instead!")>]
            let getFirstDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
                getFirstDerivative (CubicSplineCoef.Create xValues coefficients)
    
            [<Obsolete("Use Interpolation.CubicSpline.getSecondDerivative instead!")>]
            let getSecondDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
                getSecondDerivative (CubicSplineCoef.Create xValues coefficients)
    
            [<Obsolete("Use Interpolation.CubicSpline.getThirdDerivative instead!")>]
            let getThirdDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
                getThirdDerivative (CubicSplineCoef.Create xValues coefficients)
    
        [<Obsolete("Use Interpolation.Akima instead!")>]
        module Differentiable = 
    
            [<Obsolete("Use Interpolation.Akima.interpolateHermiteSorted instead!")>]
            let interpolateHermiteSorted (xValues:float []) (yValues:float []) (firstDerivatives:float []) = 
                Akima.interpolateHermiteSorted xValues yValues firstDerivatives
    
            [<Obsolete("Use Interpolation.Akima.interpolate instead!")>]
            let coefficients (xValues:float []) (yValues:float []) =
                Akima.interpolate xValues yValues
    
            [<Obsolete("Use Interpolation.Akima.predict instead!")>]
            let interpolateAtX (splineCoeffs: Akima.SubSplineCoef) xVal =
                Akima.predict splineCoeffs xVal
    
            [<Obsolete("Use Interpolation.Akima.getFirstDerivative instead!")>]
            let firstDerivative (splineCoeffs: Akima.SubSplineCoef) xVal =
                Akima.getFirstDerivative splineCoeffs xVal
    
            [<Obsolete("Use Interpolation.Akima.secondDerivative instead!")>]
            let secondDerivative (splineCoeffs: Akima.SubSplineCoef) xVal =
                Akima.getSecondDerivative splineCoeffs xVal
    
            [<Obsolete("Use Interpolation.Akima.computeIndefiniteIntegral instead!")>]
            let computeIndefiniteIntegral (splineCoeffs: Akima.SubSplineCoef) = 
                Akima.computeIndefiniteIntegral splineCoeffs
    
            [<Obsolete("Use Interpolation.Akima.integrate instead!")>]
            let integrate (splineCoeffs: Akima.SubSplineCoef) xVal = 
                Akima.integrate splineCoeffs xVal
    
            [<Obsolete("Use Interpolation.Akima.definiteIntegral instead!")>]
            let definiteIntegral (integrateF: float -> float) xVal1 xVal2 =
                Akima.definiteIntegral xVal1 xVal2


    /// <summary>
    /// Approximation
    /// </summary>
    module Approximation =

        type Spacing = 
            | Equally
            | Chebyshev

        /// <summary>
        ///   Zips x and y values, sorts them by x values and applies a given function to y values of x duplicate (like R! regularize.values).
        ///   1. pairs x-y values 
        ///   2. filters non finite entries and sorts value pairs by x values
        ///   3. handles y values of x value ties by given function
        /// </summary> 
        /// <param name="xData">unsorted x values</param>
        /// <param name="yData">y values of corresponding x values</param>
        /// <param name="ties">function to transform a collection of y values, that have equal x values (e.g. Seq.average, Seq.max, Seq.min)</param>
        /// <returns>Collection of sorted x,y tuples with unique x values</returns>
        let regularizeValues (xData: seq<float>) (yData: seq<float>) (ties: seq<float> -> float) =
            if Seq.length xData <> Seq.length yData then
                raise (System.ArgumentException("x and y are of different length!"))
            let xy = 
                Seq.zip xData yData
                // Remove nan
                |> Seq.filter (fun (x,y) -> not (isNan x || isNan y || isInf x || isInf y))
                // sort by x
                |> Seq.sortBy fst
                |> Seq.groupBy fst
                |> Seq.map (fun (key,values) -> 
                    let ny = values |> Seq.map snd |> ties  //|> Seq.averageBy ( fun (x,y) -> y) //
                    (key,ny) 
                    )
            xy

        /// <summary>
        ///   Return a sequence of points which interpolate the given data points by straight lines.    
        /// </summary> 
        /// <param name="xData">unsorted x values</param>
        /// <param name="yData">y values of corresponding x values</param>
        /// <param name="v">Collection of x values of which the y values should be predicted using straight interpolating lines between the input knots.</param>
        /// <param name="ties">function to transform a collection of y values, that have equal x values (e.g. Seq.average, Seq.max, Seq.min)</param>
        /// <returns>Collection of predicted v,y tuples.</returns>
        let approx (xData: seq<float>) (yData: seq<float>) (v: seq<float>) (ties: seq<float> -> float) =
            let xy = regularizeValues xData yData ties
            let nx,ny = xy |> Seq.toArray |> Array.unzip 
            let interPol = LinearSpline.interpolate nx ny
            v |> Seq.map (LinearSpline.predict interPol)

        /// <summary>
        ///   Creates a collection of ordered x values within a given interval. The spacing is according to Chebyshev to remove runges phenomenon when approximating a given function.
        /// </summary> 
        /// <param name="interval">start and end value of interpolation range</param>
        /// <param name="n">number of points that should be placed within the interval (incl. start and end)</param>
        /// <returns>Collection of chebyshev spaced x values.</returns>
        /// <remarks>www.mscroggs.co.uk/blog/57</remarks>
        let chebyshevNodes (interval: Interval<float>) n = 
            let center = 0.5 * (interval.TryStart.Value + interval.TryEnd.Value)
            let halfrange = 0.5 * (interval.TryEnd.Value - interval.TryStart.Value) 
            Array.init n (fun i -> 
                center + halfrange * cos(Math.PI * (2. * (float i + 1.) - 1.)/(2. * float n)) 
                )
            |> Array.sort
            |> vector

        /// <summary>
        ///   Creates a collection of ordered x values within a given interval. The spacing of the values is always the same.
        /// </summary> 
        /// <param name="interval">start and end value of interpolation range</param>
        /// <param name="n">number of points that should be placed within the interval (incl. start and end)</param>
        /// <returns>Collection of equally spaced x values.</returns>
        let equalNodes (interval: Interval<float>) n = 
            let range = Interval.getSize interval
            Vector.init n (fun k -> interval.TryStart.Value + float k * range / (float n - 1.))
    

    type Approximation() =

        /// <summary>
        ///   Determines polynomial coefficients to approximate the given function with (i) n equally spaced nodes or (ii) n nodes spaced according to Chebyshev.
        ///   Use Polynomial.fit to get a polynomial function of the coefficients.
        /// </summary> 
        /// <param name="f">Function that should be approximated by a polynomial interpolating function.</param>
        /// <param name="i">Interval for which the function should be approximated</param>
        /// <param name="n">number of points that should be placed within the interval (incl. start and end)</param>
        /// <param name="spacing">X values can be spaced equally or according to chebyshev.</param>
        /// <returns>Coefficients for polynomial interpolation. Use Polynomial.fit to predict function values.</returns>
        static member approxWithPolynomial (f: float -> float,i: Interval<float>,n: int,spacing: Approximation.Spacing) = 
            match spacing with
            | Approximation.Equally -> 
                let xVal = Approximation.equalNodes i n 
                let yVal = Vector.map f xVal
                Polynomial.interpolate xVal yVal
            | Approximation.Chebyshev ->    
                let xVal = Approximation.chebyshevNodes i n 
                let yVal = Vector.map f xVal
                Polynomial.interpolate xVal yVal

        /// <summary>
        ///   Determines polynomial coefficients to approximate the given data with (i) n equally spaced nodes or (ii) n nodes spaced according to Chebyshev.
        ///   Use Polynomial.fit to get a polynomial function of the coefficients.
        /// </summary> 
        /// <param name="xData">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
        /// <param name="yData">vector of y values</param>
        /// <param name="n">number of points that should be placed within the interval (incl. start and end)</param>
        /// <param name="spacing">X values can be spaced equally or according to chebyshev.</param>
        /// <returns>Coefficients for polynomial interpolation. Use Polynomial.fit to predict function values.</returns>
        static member approxWithPolynomialFromValues (xData: seq<float>,yData: seq<float>,n: int,spacing: Approximation.Spacing) = 
            match spacing with
            | Approximation.Equally -> 
                let i = Interval.ofSeq xData
                let linearSplineCoeff = LinearSpline.interpolate (Array.ofSeq xData) (Array.ofSeq yData)
                let f = LinearSpline.predict linearSplineCoeff
                let xVal = Approximation.equalNodes i n 
                let yVal = Vector.map f xVal
                Polynomial.interpolate xVal yVal
            | Approximation.Chebyshev ->    
                let i = Interval.ofSeq xData
                let linearSplineCoeff = LinearSpline.interpolate (Array.ofSeq xData) (Array.ofSeq yData)
                let f = LinearSpline.predict linearSplineCoeff
                let xVal = Approximation.chebyshevNodes i n 
                let yVal = Vector.map f xVal
                Polynomial.interpolate xVal yVal

    //open FSharp.Stats
    //open FSharp.Stats.Fitting.LinearRegression
    //open System
    //open Plotly.NET

    //let myFunction = fun x -> 1./(1. + 25.*x**2.)

    //let interval = Intervals.Interval.Create (-1.,1.)

    //let chart n =
    //    let eqX = Interpolation.Approximation.equalNodes interval n
    //    let chX = Interpolation.Approximation.chebyshevNodes interval n
    //    let eqCoeffs = Interpolation.Approximation.approxPolynomial myFunction interval n
    //    let chCoeffs = Interpolation.Approximation.approxChebyshevPolynomial myFunction interval n

    //    let getCh f = 
    //        [interval.TryStart.Value .. 0.01 .. interval.TryEnd.Value]
    //        |> List.map (fun x -> 
    //            x,f x)
    //        |> Chart.Line

    //    let x,y = 
    //        [interval.TryStart.Value .. 0.001 .. interval.TryEnd.Value]
    //        |> List.map (fun x -> 
    //            x,myFunction x)
    //        |> List.unzip
        
    //    let olscoef = OLS.Polynomial.coefficient n (vector x) (vector y)
    //    let olsfit = OLS.Polynomial.fit n olscoef

    //    [
    //        getCh myFunction |> Chart.withLineStyle(Width=4.,Color=Color.fromHex "2ca02c") |> Chart.withTraceName "original"
    //        eqX |> Seq.map (fun x -> x,0) |> Chart.Point |> Chart.withMarkerStyle(Color=Color.fromHex "#1f77b4")|> Chart.withTraceName "eqX"
    //        chX |> Seq.map (fun x -> x,0) |> Chart.Point |> Chart.withMarkerStyle(Color=Color.fromHex "#ff7f0e")|> Chart.withTraceName "chX"
    //        getCh (Interpolation.Polynomial.fit eqCoeffs) |> Chart.withLineStyle(Color=Color.fromHex "#1f77b4") |> Chart.withTraceName "equally"
    //        getCh (Interpolation.Polynomial.fit chCoeffs) |> Chart.withLineStyle(Color=Color.fromHex "#ff7f0e") |> Chart.withTraceName "chebyshev"
    //        getCh olsfit |> Chart.withLineStyle(Color=Color.fromHex "#9467bd")|> Chart.withTraceName "OLS"
    //    ]
    //    |> Chart.combine
    //    |> Chart.withTemplate ChartTemplates.lightMirrored

    //chart 9
    //|> Chart.show


open Interpolation

/// <summary>
///   Lets the user choose between 5 interpolation methods. One simply connects dots (LinearSpline), one forms a single interpolating polynomial (Polynomial, and three of them employ piecewise cubic polynomials.
/// </summary>
[<RequireQualifiedAccess>]
type InterpolationMethod =
    /// <summary>
    ///   Creates a linear spline from x,y coordinates. x,y coordinates are interpolated by straight lines between two knots.
    ///   Equivalent to interval-wise simple linear regression between any neighbouring pair of data.
    /// </summary>
    | LinearSpline
    /// <summary>
    ///   Creates a polynomial of degree n-1 that interpolate all n knots.
    /// </summary>
    | Polynomial
    /// <summary>
    ///   Creates a spline as piecewise cubic polynomials with continuous first and second derivative at the knots.
    /// </summary>
    /// <param name="CubicSpline.BoundaryCondition">One of four conditions to manipulate the curvatures at the outer knots.</param>
    | CubicSpline of CubicSpline.BoundaryCondition
    /// <summary>
    ///   Creates a subspline as piecewise cubic polynomials with continuous first derivative but DIScontinuous second derivative at the knots.
    /// </summary>
    | AkimaSubSpline
    /// <summary>
    ///   Creates a spline as piecewise cubic polynomials.
    /// </summary>
    | HermiteSpline

/// <summary>This type summarizes coefficient types from various interpolation methods.</summary>
[<RequireQualifiedAccess>]
type InterpolationCoefficients =
    | LinearSplineCoef     of LinearSpline.LinearSplineCoef
    | PolynomialCoef       of Polynomial.PolynomialCoef
    | CubicSplineCoef      of CubicSpline.CubicSplineCoef
    | AkimaSubSplineCoef   of Akima.SubSplineCoef
    | HermiteSplineCoef    of CubicSpline.Hermite.HermiteCoef

/// <summary>
///   This type contains functionalities to perform various interpolation methods for two dimensional data. It summarizes functions contained within the interpolation module.
/// </summary>
type Interpolation() = 

    /// <summary>
    ///   Determines interpolation coefficients for two dimensional data. CubicSpline, AkimaSpline and HermiteSpline use piecewise cubic splines.
    /// </summary>
    /// <param name="xValues">Input x values. Must not contain duplicates.</param>
    /// <param name="yValues">Input y values</param>
    /// <param name="method">Interpolation Method</param>
    /// <returns>Coefficients for interpolation function.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get coefficient for interpolating straight lines
    /// Interpolation.interpolate(xData,yData,InterpolationMethod.LinearSpline)
    /// // get coefficient for interpolating cubic spline with periodic behaviour
    /// Interpolation.interpolate(xData,yData,InterpolationMethod.CubicSpline CubicSpline.BoundaryCondition.Periodic)
    /// </code> 
    /// </example>
    static member interpolate(xValues,yValues,method) = 
        let interpolate (method: InterpolationMethod) xData yData = 
            match method with
            | InterpolationMethod.LinearSpline      -> LinearSpline.interpolate xData yData |> InterpolationCoefficients.LinearSplineCoef
            | InterpolationMethod.Polynomial        -> Polynomial.interpolate (vector xData) (vector yData) |> InterpolationCoefficients.PolynomialCoef
            | InterpolationMethod.CubicSpline bc    -> CubicSpline.interpolate bc (vector xData) (vector yData) |> InterpolationCoefficients.CubicSplineCoef
            | InterpolationMethod.AkimaSubSpline    -> Akima.interpolate xData yData |> InterpolationCoefficients.AkimaSubSplineCoef
            | InterpolationMethod.HermiteSpline     -> CubicSpline.Hermite.interpolate (vector xData) (vector yData) |> InterpolationCoefficients.HermiteSplineCoef
        interpolate method xValues yValues 

    /// <summary>
    ///   Takes interpolation coefficients to create a interpolation function.
    /// </summary>
    /// <param name="coef">Interpolation coefficients</param>
    /// <returns>Function that takes an x value and returns the corresponding y value.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get slopes and intersects for interpolating straight lines
    /// let coefficients = 
    ///     Interpolation.interpolate(xData,yData,InterpolationMethod.LinearSpline)
    /// 
    /// // get coefficient for interpolating straight lines
    /// let coefLinSpl = Interpolation.interpolate(xData,yData,InterpolationMethod.LinearSpline)
    ///
    /// // get interpolating function
    /// let func = Interpolation.predict(coefLinSpl)
    ///
    /// // get y value at x=3.4
    /// func 3.4
    ///
    /// </code> 
    /// </example>
    /// <remarks>X values that don't lie within the range of the input x values, may fail or are predicted using the nearest interpolation line!</remarks>
    static member predict(coef) = 
        let predict (coefs: InterpolationCoefficients) x = 
            match coefs with
            | InterpolationCoefficients.LinearSplineCoef c    -> LinearSpline.predict c x
            | InterpolationCoefficients.PolynomialCoef c      -> Polynomial.predict c x
            | InterpolationCoefficients.CubicSplineCoef c     -> CubicSpline.predict c x
            | InterpolationCoefficients.AkimaSubSplineCoef c  -> Akima.predict c x
            | InterpolationCoefficients.HermiteSplineCoef c   -> CubicSpline.Hermite.predict c x
        (fun x -> predict coef x)

    /// <summary>
    ///   Takes interpolation coefficients to create a function that calculates the slope of the interpolation function.
    /// </summary>
    /// <param name="coef">Interpolation coefficients</param>
    /// <returns>Function that takes an x value and returns the corresponding slope.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get slopes and intersects for interpolating straight lines
    /// let coefficients = 
    ///     Interpolation.interpolate(xData,yData,InterpolationMethod.LinearSpline)
    /// 
    /// // get coefficient for interpolating straight lines
    /// let coefLinSpl = Interpolation.interpolate(xData,yData,InterpolationMethod.LinearSpline)
    ///
    /// // get first derivative
    /// let func = Interpolation.getFirstDerivative(coefLinSpl)
    ///
    /// // get slope at x=3.4
    /// func 3.4
    ///
    /// </code> 
    /// </example>
    /// <remarks>X values that don't lie within the range of the input x values, may fail or are predicted using the nearest interpolation line!</remarks>
    static member getFirstDerivative(coef) = 
        let diff (coefs: InterpolationCoefficients) x = 
            match coefs with
            | InterpolationCoefficients.LinearSplineCoef c    -> LinearSpline.differentiate c x
            | InterpolationCoefficients.PolynomialCoef c      -> Polynomial.getDerivative c 1 x
            | InterpolationCoefficients.CubicSplineCoef c     -> CubicSpline.getFirstDerivative c x
            | InterpolationCoefficients.AkimaSubSplineCoef c  -> Akima.getFirstDerivative c x
            | InterpolationCoefficients.HermiteSplineCoef c   -> failwithf "Derivative not yet implemented for Hermite spline"
        (fun x -> diff coef x)

    /// <summary>
    ///   Takes interpolation coefficients to create a function that calculates the curvature of the interpolation function.
    /// </summary>
    /// <param name="coef">Interpolation coefficients</param>
    /// <returns>Function that takes an x value and returns the corresponding curvature.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get slopes and intersects for interpolating straight lines
    /// let coefficients = 
    ///     Interpolation.interpolate(xData,yData,InterpolationMethod.Polynomial)
    /// 
    /// // get coefficients for interpolating polynomial
    /// let coef = Interpolation.interpolate(xData,yData,InterpolationMethod.PolynomialCoef)
    ///
    /// // get second derivative
    /// let func = Interpolation.getSecondDerivative(coef)
    ///
    /// // get curvature at x=3.4
    /// func 3.4
    ///
    /// </code> 
    /// </example>
    /// <remarks>X values that don't lie within the range of the input x values, may fail or are predicted using the nearest interpolation line!</remarks>
    static member getSecondDerivative(coef) = 
        let diff (coefs: InterpolationCoefficients) x = 
            match coefs with
            | InterpolationCoefficients.LinearSplineCoef c    -> 0.
            | InterpolationCoefficients.PolynomialCoef c      -> Polynomial.getDerivative c 2 x
            | InterpolationCoefficients.CubicSplineCoef c     -> CubicSpline.getSecondDerivative c x
            | InterpolationCoefficients.AkimaSubSplineCoef c  -> Akima.getSecondDerivative c x
            | InterpolationCoefficients.HermiteSplineCoef c   -> failwithf "Derivative not yet implemented for Hermite spline"
        (fun x -> diff coef x)
