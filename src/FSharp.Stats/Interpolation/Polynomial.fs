namespace FSharp.Stats.Interpolation

open System
open FSharp.Stats
open FSharp.Stats.Algebra

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
        /// vector of polynomial coefficients sorted as [intercept;constant;quadratic;...]
        /// </summary>
        C0_CX : Vector<float>
        } with static member Create c = {C0_CX = c}

    /// <summary>
    ///   Calculates the polynomial coefficients for interpolating the given unsorted data. 
    /// </summary>
    /// <remarks>No duplicates allowed!</remarks>
    /// <param name="xData">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
    /// <param name="yData">vector of y values</param>
    /// <returns>vector of polynomial coefficients sorted as [intercept;constant;quadratic;...]</returns>
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
    /// <param name="coef">polynomial coefficients (e.g. determined by Polynomial.coefficients), sorted as [intercept;constant;quadratic;...]</param>
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
        coef.C0_CX |> Vector.foldi (fun i acc c -> acc + (c * (pown x i))) 0.

    /// <summary>
    ///   calculates derivative values at X=x with given polynomial coefficients. Level 1 = fst derivative; Level2 = snd derivative ...
    /// </summary>
    /// <param name="coef">polynomial coefficients (e.g. determined by Polynomial.coefficients), sorted as [intercept;constant;quadratic;...]</param>
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
