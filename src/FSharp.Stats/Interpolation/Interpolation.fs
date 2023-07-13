namespace FSharp.Stats.Interpolation

open System
open FSharp.Stats
open FSharp.Stats

[<RequireQualifiedAccess>]
type InterpolationMethod =
    | LinearSpline
    | Polynomial
    | CubicSpline of CubicSpline.BoundaryCondition
    | AkimaSpline
    | HermiteSpline

type InterpolationCoefficients =
    | LinearSplineCoef  of LinearSpline.LinearSplineCoef
    | PolynomialCoef    of Polynomial.PolynomialCoef
    | CubicSplineCoef   of CubicSpline.CubicSplineCoef
    | AkimaSplineCoef   of Akima.SplineCoef
    | HermiteSplineCoef of CubicSpline.Hermite.HermiteCoef

   
type Interpolation() = 

    static member interpolate(xVal,yVal,method) = 
        let interpolate (method: InterpolationMethod) xData yData = 
            match method with
            | InterpolationMethod.LinearSpline      -> LinearSpline.interpolate xData yData |> LinearSplineCoef
            | InterpolationMethod.Polynomial        -> Polynomial.interpolate (vector xData) (vector yData) |> PolynomialCoef
            | InterpolationMethod.CubicSpline bc    -> CubicSpline.interpolate bc (vector xData) (vector yData) |> CubicSplineCoef
            | InterpolationMethod.AkimaSpline       -> Akima.interpolate xData yData |> AkimaSplineCoef
            | InterpolationMethod.HermiteSpline     -> CubicSpline.Hermite.interpolate (vector xData) (vector yData) |> HermiteSplineCoef
        interpolate method xVal yVal 

    static member predict(coef) = 
        let predict (coefs: InterpolationCoefficients) x = 
            match coefs with
            | InterpolationCoefficients.LinearSplineCoef c    -> LinearSpline.predict c x
            | InterpolationCoefficients.PolynomialCoef c      -> Polynomial.predict c x
            | InterpolationCoefficients.CubicSplineCoef c     -> CubicSpline.predict c x
            | InterpolationCoefficients.AkimaSplineCoef c     -> Akima.predict c x
            | InterpolationCoefficients.HermiteSplineCoef c   -> CubicSpline.Hermite.predict c x
        (fun x -> predict coef x)
