(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta8"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta8"
#r "nuget: FSharp.Stats"
#endif // IPYNB

(**

# Interpolation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Interpolation.ipynb)

_Summary:_ This tutorial demonstrates several ways of interpolating with FSharp.Stats

### Table of contents

- [Polynomial interpolation](#Polynomial-interpolation)
- [Cubic interpolating Spline](#Cubic-interpolating-Spline)
- [Hermite interpolation](#Hermite-interpolation)

## Polynomial Interpolation

Here a polynomial is fitted to the data. In general, a polynomial with degree = dataPointNumber - 1 has sufficient flexibility to interpolate all data points.
The least squares approach is not sufficient to converge to an interpolating polynomial! A degree other than n-1 results in a regression polynomial.

*)

open FSharp.Stats

let xData = vector [|1.;2.;3.;4.;5.;6.|]
let yData = vector [|4.;7.;9.;8.;7.;9.;|]

//Polynomial interpolation

//Define the polynomial coefficients. In Interpolation the order is equal to the data length - 1.
let coefficients = 
    Interpolation.Polynomial.coefficients xData yData 
let interpolFunction x = 
    Interpolation.Polynomial.fit coefficients x


open Plotly.NET

//some axis styling
let myAxis title = Axis.LinearAxis.init(Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)
let myAxisRange title range = Axis.LinearAxis.init(Title=title,Range=StyleParam.Range.MinMax range,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)
let styleChart x y chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxis y)

let rawChart = 
    Chart.Point(xData,yData)
    |> Chart.withTraceName "raw data"
    
let interpolPol = 
    let fit = [|1. .. 0.1 .. 6.|] |> Array.map (fun x -> x,interpolFunction x)
    fit
    |> Chart.Line
    |> Chart.withTraceName "interpolating polynomial"

let chartPol = 
    [rawChart;interpolPol] 
    |> Chart.Combine
    |> styleChart "" ""

(*** condition: ipynb ***)
#if IPYNB
chartPol
#endif // IPYNB

(***hide***)
chartPol |> GenericChart.toChartHTML
(***include-it-raw***)

(**
## Cubic interpolating Spline

Splines are flexible strips of wood, that were used by shipbuilders to draw smooth shapes. In graphics and mathematics a piecewise cubic polynomial (order = 3) is called spline.
The curvature (second derivative) of a cubic polynomial is proportional to its tense energy and in spline theory the curvature is minimized. Therefore, the resulting function is very smooth.
To solve for the spline coefficients it is necessary to define two additional constraints, so called boundary conditions:

 - natural spline (most used spline variant): `f''` at borders is set to 0

 - periodic spline: `f'` at first point is the same as `f'` at the last point

 - parabolic spline: `f''` at first/second and last/penultimate knot are equal

 - notAKnot spline: `f'''` at second and penultimate knot are continuous

 - quadratic spline: first and last polynomial are quadratic, not cubic

 - clamped spline: `f'` at first and last knot are set by user

In general, piecewise cubic splines only are defined within the region defined by the used x values.

### Related information
 - [Cubic Spline Interpolation](https://en.wikiversity.org/wiki/Cubic_Spline_Interpolation)

 - [Boundary conditions](https://timodenk.com/blog/cubic-spline-interpolation/)

 - [Cubic spline online tool](https://tools.timodenk.com/cubic-spline-interpolation)
*)


open Plotly.NET
open FSharp.Stats.Interpolation

let xValues = vector [1.;2.;3.;4.;5.5;6.]
let yValues = vector [1.;8.;6.;3.;7.;1.]

//calculates the spline coefficients for a natural spline
let coeffSpline = 
    CubicSpline.Simple.coefficients CubicSpline.Simple.BoundaryCondition.Natural xValues yValues
//cubic interpolating splines are only defined within the region defined in xValues
let fit  x = 
    CubicSpline.Simple.fit coeffSpline xValues x
//to fit x_Values that are out of the region defined in xValues
//fits the interpolation spline with linear prediction at borderknots
let fitIntPo x = 
    CubicSpline.Simple.fitWithLinearPrediction coeffSpline xValues x

//to compare the spline fit with an interpolating polynomial:
let coeffPolynomial = 
    Interpolation.Polynomial.coefficients xValues yValues
let fitPol x = 
    Interpolation.Polynomial.fit coeffPolynomial x
//A linear spline draws straight lines to interpolate all data
let coeffLinearSpline = Interpolation.LinearSpline.initInterpolate (Array.ofSeq xValues) (Array.ofSeq yValues)
let fitLinSp = Interpolation.LinearSpline.interpolate coeffLinearSpline

let splineChart =
    [
    Chart.Point(xValues,yValues)                                           |> Chart.withTraceName "raw data"
    [ 1. .. 0.1 .. 6.] |> List.map (fun x -> x,fitPol x)   |> Chart.Line |> Chart.withTraceName "fitPolynomial"
    [-1. .. 0.1 .. 8.] |> List.map (fun x -> x,fitIntPo x) |> Chart.Line |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash) |> Chart.withTraceName "fitSplineLinPred"
    [ 1. .. 0.1 .. 6.] |> List.map (fun x -> x,fit x)      |> Chart.Line |> Chart.withTraceName "fitSpline"
    [ 1. .. 0.1 .. 6.] |> List.map (fun x -> x,fitLinSp x) |> Chart.Line |> Chart.withTraceName "fitLinearSpline"
    ]
    |> Chart.Combine
    |> Chart.withTitle "Interpolation methods"
    |> Chart.withY_Axis (myAxisRange "" (-10.,10.))
    |> Chart.withX_Axis (myAxis "" )

(*** condition: ipynb ***)
#if IPYNB
splineChart
#endif // IPYNB

(***hide***)
splineChart |> GenericChart.toChartHTML
(***include-it-raw***)

//additionally you can calculate the derivatives of the spline
//The cubic spline interpolation is continuous in f, f', and  f''.
let derivativeChart =
    [
        Chart.Point(xValues,yValues) |> Chart.withTraceName "raw data"
        [1. .. 0.1 .. 6.] |> List.map (fun x -> x,fit x) |> Chart.Line  |> Chart.withTraceName "spline fit"
        [1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.Simple.getFirstDerivative  coeffSpline xValues x) |> Chart.Point |> Chart.withTraceName "fst derivative"
        [1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.Simple.getSecondDerivative coeffSpline xValues x) |> Chart.Point |> Chart.withTraceName "snd derivative"
        [1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.Simple.getThirdDerivative  coeffSpline xValues x) |> Chart.Point |> Chart.withTraceName "trd derivative"
    ]
    |> Chart.Combine
    |> Chart.withTitle "Cubic spline derivatives"
    |> styleChart "" ""

(*** condition: ipynb ***)
#if IPYNB
derivativeChart
#endif // IPYNB

(***hide***)
derivativeChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
## Hermite interpolation

In Hermite interpolation the user can define the slopes of the function in the knots. This is especially useful if the function is oscillating and thereby generates local minima/maxima.
Intuitively the slope of a knot should be between the slopes of the adjacent straight lines. By using this slope calculation a monotone knot behavior results in a monotone spline.


 - [Slope calculation](http://www.korf.co.uk/spline.pdf)

*)

open FSharp.Stats
open FSharp.Stats.Interpolation
open FSharp.Stats.Interpolation.CubicSpline
open Plotly.NET

//example from http://www.korf.co.uk/spline.pdf
let xDataH = vector [0.;10.;30.;50.;70.;80.;82.]
let yDataH = vector [150.;200.;200.;200.;180.;100.;0.]

//Get slopes for Hermite spline. Try to fit a monotone function.
let tryMonotoneSlope = Simple.Hermite.getSlopesTryMonotonicity xDataH yDataH    
//get function for Hermite spline
let funHermite = Simple.Hermite.cubicHermite xDataH yDataH tryMonotoneSlope

//get coefficients and function for a classic natural spline
let coeffSpl = Simple.coefficients Simple.BoundaryCondition.Natural xDataH yDataH
let funNaturalSpline x = Simple.fit coeffSpl xDataH x

//get coefficients and function for a classic polynomial interpolation
let coeffPolInterpol = 
    //let neutralWeights = Vector.init 7 (fun x -> 1.)
    //Fitting.LinearRegression.OrdinaryLeastSquares.Polynomial.coefficientsWithWeighting 6 neutralWeights xDataH yDataH
    Interpolation.Polynomial.coefficients xDataH yDataH
let funPolInterpol x = 
    //Fitting.LinearRegression.OrdinaryLeastSquares.Polynomial.fit 6 coeffPolInterpol x
    Interpolation.Polynomial.fit coeffPolInterpol x

let splineComparison =
    [
    Chart.Point(xDataH,yDataH) |> Chart.withTraceName "raw data"
    [0. .. 82.] |> List.map (fun x -> x,funNaturalSpline x) |> Chart.Line  |> Chart.withTraceName "natural spline"
    [0. .. 82.] |> List.map (fun x -> x,funHermite x      ) |> Chart.Line  |> Chart.withTraceName "hermite spline"
    [0. .. 82.] |> List.map (fun x -> x,funPolInterpol x  ) |> Chart.Line  |> Chart.withTraceName "polynomial"
    ]
    |> Chart.Combine
    |> styleChart "" ""

(*** condition: ipynb ***)
#if IPYNB
splineComparison
#endif // IPYNB

(***hide***)
splineComparison |> GenericChart.toChartHTML
(***include-it-raw***)
