(**
---
title: Interpolation
index: 8
category: Documentation
categoryindex: 0
---
*)

(*** hide ***)

(*** condition: prepare ***)
#I "../src/FSharp.Stats/bin/Release/netstandard2.0/"
#r "FSharp.Stats.dll"
#r "nuget: Plotly.NET, 4.0.0"

Plotly.NET.Defaults.DefaultDisplayOptions <-
    Plotly.NET.DisplayOptions.init (PlotlyJSReference = Plotly.NET.PlotlyJSReference.NoReference)

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: Plotly.NET.Interactive, 4.0.0"
#r "nuget: FSharp.Stats"

open Plotly.NET
#endif // IPYNB

(**

# Interpolation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Interpolation.ipynb)
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

_Summary:_ This tutorial demonstrates several ways of interpolating with FSharp.Stats

### Table of contents

- [Polynomial interpolation](#Polynomial-interpolation)
- [Cubic interpolating spline](#Cubic-spline-interpolation)
- [Akima interpolating spline](#Akima-spline-interpolation)
- [Hermite interpolation](#Hermite-interpolation)
- [Chebyshev function approximation](#Chebyshev-function-approximation)

## Polynomial Interpolation

Here a polynomial is fitted to the data. In general, a polynomial with degree = dataPointNumber - 1 has sufficient flexibility to interpolate all data points.
The least squares approach is not sufficient to converge to an interpolating polynomial! A degree other than n-1 results in a regression polynomial.

*)


open Plotly.NET
open FSharp.Stats

let xData = vector [|1.;2.;3.;4.;5.;6.|]
let yData = vector [|4.;7.;9.;8.;7.;9.;|]

//Polynomial interpolation

//Define the polynomial coefficients. In Interpolation the order is equal to the data length - 1.
let coefficients = 
    Interpolation.Polynomial.coefficients xData yData 
let interpolFunction x = 
    Interpolation.Polynomial.fit coefficients x

let rawChart = 
    Chart.Point(xData,yData)
    |> Chart.withTraceInfo "raw data"
    
let interpolPol = 
    let fit = [|1. .. 0.1 .. 6.|] |> Array.map (fun x -> x,interpolFunction x)
    fit
    |> Chart.Line
    |> Chart.withTraceInfo "interpolating polynomial"

let chartPol = 
    [rawChart;interpolPol] 
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
chartPol
#endif // IPYNB

(***hide***)
chartPol |> GenericChart.toChartHTML
(***include-it-raw***)

(**
## Cubic spline interpolation

Splines are flexible strips of wood, that were used by shipbuilders to draw smooth shapes. In graphics and mathematics a piecewise cubic polynomial (order = 3) is called spline.
The curvature (second derivative) of a cubic polynomial is proportional to its tense energy and in spline theory the curvature is minimized. Therefore, the resulting function is very smooth.
To solve for the spline coefficients it is necessary to define two additional constraints, so called boundary conditions:

 - natural spline (most used spline variant): `f''` at borders is set to 0

 - periodic spline: `f'` at first point is the same as `f'` at the last point

 - parabolic spline: `f''` at first/second and last/penultimate knot are equal

 - notAKnot spline: `f'''` at second and penultimate knot are continuous

 - quadratic spline: first and last polynomial are quadratic, not cubic

 - clamped spline: `f'` at first and last knot are set by user

In general, piecewise cubic splines only are defined within the region defined by the used x values. Using `predict` with x values outside this range, uses the slopes and intersects of the nearest knot and utilizes them for prediction.

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
    CubicSpline.fit CubicSpline.BoundaryCondition.Natural xValues yValues

//cubic interpolating splines are only defined within the region defined in xValues
let fitFunctionWithinRange  x = 
    CubicSpline.predictWithinRange coeffSpline xValues x

//to fit x_Values that are out of the region defined in xValues
//fits the interpolation spline with linear prediction at borderknots
let fitFunction x = 
    CubicSpline.predict coeffSpline xValues x

//to compare the spline fit with an interpolating polynomial:
let coeffPolynomial = 
    Interpolation.Polynomial.coefficients xValues yValues
let fitFunctionPol x = 
    Interpolation.Polynomial.fit coeffPolynomial x
//A linear spline draws straight lines to interpolate all data
let coeffLinearSpline = Interpolation.LinearSpline.initInterpolate (Array.ofSeq xValues) (Array.ofSeq yValues)
let fitFunctionLinSp = Interpolation.LinearSpline.interpolate coeffLinearSpline

let splineChart =
    [
    Chart.Point(xValues,yValues)                                        |> Chart.withTraceInfo "raw data"
    [ 1. .. 0.1 .. 6.] |> List.map (fun x -> x,fitFunctionPol x)        |> Chart.Line |> Chart.withTraceInfo "fitPolynomial"
    [-1. .. 0.1 .. 8.] |> List.map (fun x -> x,fitFunction x)           |> Chart.Line |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash) |> Chart.withTraceInfo "fitSpline"
    [ 1. .. 0.1 .. 6.] |> List.map (fun x -> x,fitFunctionWithinRange x)|> Chart.Line |> Chart.withTraceInfo "fitSpline_withinRange"
    [ 1. .. 0.1 .. 6.] |> List.map (fun x -> x,fitFunctionLinSp x)      |> Chart.Line |> Chart.withTraceInfo "fitLinearSpline"
    ]
    |> Chart.combine
    |> Chart.withTitle "Interpolation methods"
    |> Chart.withTemplate ChartTemplates.lightMirrored

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
        Chart.Point(xValues,yValues) |> Chart.withTraceInfo "raw data"
        [1. .. 0.1 .. 6.] |> List.map (fun x -> x,fitFunction x) |> Chart.Line  |> Chart.withTraceInfo "spline fit"
        [1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.getFirstDerivative  coeffSpline xValues x) |> Chart.Point |> Chart.withTraceInfo "fst derivative"
        [1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.getSecondDerivative coeffSpline xValues x) |> Chart.Point |> Chart.withTraceInfo "snd derivative"
        [1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.getThirdDerivative  coeffSpline xValues x) |> Chart.Point |> Chart.withTraceInfo "trd derivative"
    ]
    |> Chart.combine
    |> Chart.withTitle "Cubic spline derivatives"
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
derivativeChart
#endif // IPYNB

(***hide***)
derivativeChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
## Akima spline interpolation

Akima splines are highly connected to default cubic spline interpolation. The main difference is the missing constraint of curvature continuity. This enhanced curvature flexibility diminishes oscillations of the 
interpolating piecewise cubic splines.

*)

let xVal = [|1. .. 10.|]
let yVal = [|1.;-0.5;2.;2.;2.;3.;3.;3.;5.;4.|]

let akimaCoeffs = Akima.fit xVal yVal

let akima = 
    [0. .. 0.1 .. 11.]
    |> List.map (fun x -> 
        x,Akima.predict akimaCoeffs x)
    |> Chart.Line

let cubicCoeffs = CubicSpline.fit CubicSpline.BoundaryCondition.Natural (vector xVal) (vector yVal)

let cubicSpline = 
    [0. .. 0.1 .. 11.]
    |> List.map (fun x -> 
        x,CubicSpline.predict cubicCoeffs (vector xVal) x)
    |> Chart.Line

let akimaChart = 
    [
        Chart.Point(xVal,yVal,Name="data")
        cubicSpline |> Chart.withTraceInfo "cubic spline"
        akima |> Chart.withTraceInfo "akima spline"
    ]
    |> Chart.combine
    |> Chart.withTitle "Cubic spline derivatives"
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
akimaChart
#endif // IPYNB

(***hide***)
akimaChart |> GenericChart.toChartHTML
(***include-it-raw***)



(**
## Hermite interpolation

In Hermite interpolation the user can define the slopes of the function in the knots. This is especially useful if the function is oscillating and thereby generates local minima/maxima.
Intuitively the slope of a knot should be between the slopes of the adjacent straight lines. By using this slope calculation a monotone knot behavior results in a monotone spline.


 - [Slope calculation](http://www.korf.co.uk/spline.pdf)

*)

open FSharp.Stats
open FSharp.Stats.Interpolation
open Plotly.NET

//example from http://www.korf.co.uk/spline.pdf
let xDataH = vector [0.;10.;30.;50.;70.;80.;82.]
let yDataH = vector [150.;200.;200.;200.;180.;100.;0.]

//Get slopes for Hermite spline. Try to fit a monotone function.
let tryMonotoneSlope = CubicSpline.Hermite.getSlopesTryMonotonicity xDataH yDataH    
//get function for Hermite spline
let funHermite = CubicSpline.Hermite.cubicHermite xDataH yDataH tryMonotoneSlope

//get coefficients and function for a classic natural spline
let coeffSpl = CubicSpline.fit CubicSpline.BoundaryCondition.Natural xDataH yDataH
let funNaturalSpline x = CubicSpline.predict coeffSpl xDataH x

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
    Chart.Point(xDataH,yDataH) |> Chart.withTraceInfo "raw data"
    [0. .. 82.] |> List.map (fun x -> x,funNaturalSpline x) |> Chart.Line  |> Chart.withTraceInfo "natural spline"
    [0. .. 82.] |> List.map (fun x -> x,funHermite x      ) |> Chart.Line  |> Chart.withTraceInfo "hermite spline"
    [0. .. 82.] |> List.map (fun x -> x,funPolInterpol x  ) |> Chart.Line  |> Chart.withTraceInfo "polynomial"
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
splineComparison
#endif // IPYNB

(***hide***)
splineComparison |> GenericChart.toChartHTML
(***include-it-raw***)



(**

## Chebyshev function approximation

Polynomials are great when it comes to slope/area determination or the investigation of signal properties.
When faced with an unknown (or complex) function it may be beneficial to approximate the data using polynomials, even if it does not correspond to the real model.

Polynomial regression can cause difficulties if the signal is flexible and the required polynomial degree is high. Floating point errors sometimes lead to vanishing coefficients and even though the
SSE should decrease, it does not and a strange, squiggly shape is generated. 
Polynomial interpolation can help to obtain a robust polynomial description of the data, but is prone to Runges phenomenon. 

In the next section, data is introduced that should be converted to a polynomial approximation.

*)


let xs = [|0. .. 0.2 .. 3.|]
let ys = [|5.;5.5;6.;6.1;4.;1.;0.7;0.3;0.5;0.9;5.;9.;9.;8.;6.5;5.;|]

let chebyChart =
    Chart.Line(xs,ys,Name="raw",ShowMarkers=true)
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
chebyChart
#endif // IPYNB

(***hide***)
chebyChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
Let's fit a interpolating polynomial to the points:

*)

// calculates the coefficients of the interpolating polynomial
let coeffs = 
    Interpolation.Polynomial.coefficients (vector xs) (vector ys)

// determines the y value of a given x value with the interpolating coefficients
let interpolatingFunction x = 
    Interpolation.Polynomial.fit coeffs x

// plot the interpolated data
let interpolChart =
    let ys_interpol = 
        [|0. .. 0.01 .. 3.|] 
        |> Seq.map (fun x -> x,interpolatingFunction x)
    Chart.Line(ys_interpol,Name="interpol")
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle "xs"
    |> Chart.withYAxisStyle "ys"

let cbChart =
    [
    chebyChart
    interpolChart
    ]
    |> Chart.combine

(*** condition: ipynb ***)
#if IPYNB
cbChart
#endif // IPYNB

(***hide***)
cbChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
Because of Runges phenomenon the interpolating polynomial overshoots in the outer areas of the data. It would be detrimental if this function approximation is used to investigate signal properties.

To reduce this overfitting you can use x axis nodes that are spaced according to Chebyshev. Here, nodes are sparse in the center of the analysed function and are more dense in the outer areas. 

*)

// new x values are determined in the x axis range of the data. These should reduce overshooting behaviour.
// since the original data consisted of 16 points, 16 nodes are initialized
let xs_cheby = 
    Interpolation.Approximation.chebyshevNodes (Intervals.Interval.Create(0.,3.)) 16

// to get the corresponding y values to the xs_cheby a linear spline is generated that approximates the new y values
let ys_cheby =
    let ls = Interpolation.LinearSpline.initInterpolate xs ys
    xs_cheby |> Vector.map (Interpolation.LinearSpline.interpolate ls)

// again polynomial interpolation coefficients are determined, but here with the x and y data that correspond to the chebyshev spacing
let coeffs_cheby = Interpolation.Polynomial.coefficients xs_cheby ys_cheby


// Note: the upper panel can be summarized by the follwing function:
Interpolation.Approximation.approxChebyshevPolynomialFromValues xs ys 16

(**

Using the determined polynomial coefficients, the standard approach for fitting can be used to plot the signal together with the function approximation. Obviously the example data 
is difficult to approximate, but the chebyshev spacing of the x-nodes drastically reduces the overfitting in the outer areas of the signal.

*)

// function using the cheby_coefficients to get y values of given x value
let interpolating_cheby x = Interpolation.Polynomial.fit coeffs_cheby x

let interpolChart_cheby =
    let ys_interpol_cheby = 
        vector [|0. .. 0.01 .. 3.|] 
        |> Seq.map (fun x -> x,interpolating_cheby x)

    Chart.Line(ys_interpol_cheby,Name="interpol_cheby")
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle "xs"
    |> Chart.withYAxisStyle "ys"


let cbChart_cheby =
    [
    chebyChart
    interpolChart
    Chart.Line(xs_cheby,ys_cheby,ShowMarkers=true,Name="cheby_nodes") |> Chart.withTemplate ChartTemplates.lightMirrored|> Chart.withXAxisStyle "xs"|> Chart.withYAxisStyle "ys"
    interpolChart_cheby
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle "xs"
    |> Chart.withYAxisStyle "ys"


(*** condition: ipynb ***)
#if IPYNB
cbChart_cheby
#endif // IPYNB

(***hide***)
cbChart_cheby |> GenericChart.toChartHTML
(***include-it-raw***)


(**
If a non-polynomal function should be approximated as polynomial you can use `Interpolation.Approximation.approxChebyshevPolynomial` with specifying the interval in which the function should be approximated.

## Further reading
- Amazing blog post regarding Runges phenomenon and chebyshev spacing https://www.mscroggs.co.uk/blog/57

*)


