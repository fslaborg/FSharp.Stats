namespace FSharp.Stats.Interpolation

open System
open FSharp.Stats

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
    
[<RequireQualifiedAccess>]
type InterpolationCoefficients =
    | LinearSplineCoef     of LinearSpline.LinearSplineCoef
    | PolynomialCoef       of Polynomial.PolynomialCoef
    | CubicSplineCoef      of CubicSpline.CubicSplineCoef
    | AkimaSubSplineCoef   of Akima.SubSplineCoef
    | HermiteSplineCoef    of CubicSpline.Hermite.HermiteCoef

/// <summary>
///   This type contains functionalities to perform various interpolation methods for two dimensional data
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
