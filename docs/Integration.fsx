(**
---
title: Integration
index: 16
category: Documentation
categoryindex: 0
---
*)

(***hide***)

(*** condition: prepare ***)
#I "../src/FSharp.Stats/bin/Release/netstandard2.0/"
#r "FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-preview.16"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-preview.16"
#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.16"
#r "nuget: FSharp.Stats"
#endif // IPYNB

(**
# Numerical integration

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Integration.ipynb)

Numerical integration comprises a broad family of algorithms for calculating the numerical value of a definite integral, typically by using values from the funcion range.

See also: https://en.wikipedia.org/wiki/Numerical_integration

## Numerical integration methods

The algorithms implemented in FSharp.Stats are:

- Left endpoint rule (`LeftEndpoint`)
- Right endpoint rule (`RightEndpoint`)
- Midpoint rule  (`Midpoint`)
- Trapezoidal rule (`Trapezoidal`)
- Simpson's rule (`Simpson`)

## Usage

You can either integrate a function (`float -> float`) or observations. When estimating the integral of observations, Mid-values are calculated as the average of two observations

### Integrating functions

Any function with domain and range of float (`float -> float`) can be numerically integrated for an interval $[a,b]$ with $n$ partitions, which will be evenly spaced in the interval (partition length = $\frac{(b-a)}n$)

Use the `NumericalIntegration.definiteIntegral` function and pass the desired estimation method together with the integration interval start/endpoints and the amount of partitions(more on those methods in the chapters below).

the expected exact value for the definite integral of $f(x) = x^3$ is 0.25

*)

open FSharp.Stats.Integration

let f (x: float) = x * x * x

// integrate f in the interval [0.,1.] with 100 partitions using the left endpoint method
f |> NumericalIntegration.definiteIntegral(LeftEndpoint, 0., 1., 100)
(***include-it***)

// integrate f in the interval [0.,1.] with 100 partitions using the right endpoint method
f |> NumericalIntegration.definiteIntegral(RightEndpoint, 0., 1., 100)
(***include-it***)

// integrate f in the interval [0.,1.] with 100 partitions using the midpoint method
f |> NumericalIntegration.definiteIntegral(Midpoint, 0., 1., 100)
(***include-it***)

// integrate f in the interval [0.,1.] with 100 partitions using the trapezoidal method
f |> NumericalIntegration.definiteIntegral(Trapezoidal, 0., 1., 100)
(***include-it***)

// integrate f in the interval [0.,1.] with 100 partitions using the simpson method
f |> NumericalIntegration.definiteIntegral(Simpson, 0., 1., 100)
(***include-it***)

(**

It should be noted that the accuracy of the estimation increases with the amount of partitions in the integration interval.

### Integrating observations

Instead of integrating a function by sampling the function values in a set interval, we can also calculate the definite integral of (x,y) pairs with these methods.

This may be of use for example for calculating the area under the curve for prediction metrics such as the ROC(Receiver operator characteristic), which yields a distinct set of (Specificity/Fallout) pairs.

Use the `NumericalIntegration.definiteIntegral` function and pass the desired estimation method (more on those methods in the chapters below).

the expected exact value for the definite integral of $f(x) = x^2$ is $0.\overline3$
*)

//x,y pairs of f(x) = x^2 in the interval of [0,1], with random values removed to show that this works with unevenly spaced data
let rnd = new System.Random(69)
let observations = 
    [|0. .. 0.01 .. 1.|] 
    |> Array.map(fun x -> x, x * x)
    |> Array.filter (fun (x,y) -> rnd.NextDouble() < 0.85 )

observations.Length
(***include-it***)

// integrate observations using the left endpoint method
observations |> NumericalIntegration.definiteIntegral LeftEndpoint
(***include-it***)

// integrate observations using the right endpoint method
observations |> NumericalIntegration.definiteIntegral RightEndpoint
(***include-it***)

// integrate observations using the midpoint method
observations |> NumericalIntegration.definiteIntegral Midpoint
(***include-it***)

// integrate observations using the trapezoidal method
observations |> NumericalIntegration.definiteIntegral Trapezoidal
(***include-it***)

// integrate observations using the simpson method
observations |> NumericalIntegration.definiteIntegral Simpson
(***include-it***)

(**
## Explanation of the methods

In the following chapter, each estimation method is introduced briefly and visualized for the example of $f(x) = x^3$ in the interval $[0,1]$ using 5 partitions.

*)

(***hide***)
let x = [0. .. 0.01 .. 1.]
let y = x |> List.map f


open Plotly.NET
open Plotly.NET.LayoutObjects

let functionChart = 
    Chart.Spline(x,y, Name="f(x) = x^3")
    |> Chart.withTitle "f(x) = x^3"
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle "x"
    |> Chart.withYAxisStyle "f(x)"
    |> Chart.withSize(600,400)

(**

A large class of quadrature rules can be derived by constructing interpolating functions that are easy to integrate. 
Typically these interpolating functions are polynomials. In practice, since polynomials of very high degree tend to oscillate wildly, only polynomials of low degree are used, typically linear and quadratic.

The approximation of all these methods increase with the size of subintervals in the integration interval.

### Left endpoint rule

The interpolating function is a constant function (a polynomial of degree zero), passing the leftmost points of the partition boundaries of the interval to integrate.

For a single partition $[a,b]$ in the integration interval, the integral is estimated by

$$
\int_a^b f(x)\,dx \approx (b-a) * f(a)
$$

The integral of the whole integration interval is obtained by summing the integral of n partitions.

*)

(***hide***)
let plotEstimationBars (data: seq<float*float>) =
    Chart.Column(
        data, 
        Name = "Left endpoint integrals", 
        MarkerPatternShape = StyleParam.PatternShape.DiagonalDescending, 
        MarkerColor = Color.fromString "rgba(0,0,0,0)", 
        MarkerOutline = Line.init(Color=Color.fromKeyword Black)
    )
    |> Chart.withLayout(Layout.init(BarGap=0.))

let leftEndpointChart = 
    let bars = 
        [
            0. , 0.;
            0.2, f 0.2
            0.4, f 0.4
            0.6, f 0.6
            0.8, f 0.8
        ]
        |> fun data -> plotEstimationBars data
        |> GenericChart.mapTrace (fun t -> t?offset <- 0.; t)
    let markers = Chart.Point([for i in 0. .. 0.2 .. 1. -> i, f i], Name = "Partition boundaries", MarkerColor = Color.fromKeyword Black, MarkerSymbol = StyleParam.MarkerSymbol.X)
    [bars; functionChart; markers] |> Chart.combine
    |> Chart.withTitle "Left endpoint rule"
    |> Chart.withSize(800,400)


(*** condition: ipynb ***)
#if IPYNB
leftEndpointChart
#endif // IPYNB

(***hide***)
leftEndpointChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

### Right endpoint rule

The interpolating function is a constant function (a polynomial of degree zero), passing the rightmost points of the partition boundaries of the interval to integrate.

For a single partition $[a,b]$ in the integration interval, the integral is estimated by

$$
\int_a^b f(x)\,dx \approx (b-a) * f(b)
$$

The integral of the whole integration interval is obtained by summing the integral of n partitions.

*)
(***hide***)
let rightEndpointChart = 
    let bars = 
        [
            0. , f 0.2
            0.2, f 0.4
            0.4, f 0.6
            0.6, f 0.8
            0.8, f 1.
        ]
        |> fun data -> plotEstimationBars data
        |> GenericChart.mapTrace (fun t -> t?offset <- 0; t)
    let markers = Chart.Point([for i in 0. .. 0.2 .. 1. -> i, f i], Name = "Partition boundaries", MarkerColor = Color.fromKeyword Black, MarkerSymbol = StyleParam.MarkerSymbol.X)
    [bars; functionChart; markers] |> Chart.combine
    |> Chart.withTitle "Right endpoint rule"
    |> Chart.withSize(800,400)

(*** condition: ipynb ***)
#if IPYNB
rightEndpointChart
#endif // IPYNB

(***hide***)
rightEndpointChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

### Midpoint rule

The interpolating function is a constant function (a polynomial of degree zero), passing the mid-points of the partition boundaries of the interval to integrate. 

For a single partition $[a,b]$ in the integration interval, the integral is estimated by

$$
\int_a^b f(x)\,dx \approx (b-a) * f(\frac{a+b}2))
$$

The integral of the whole integration interval is obtained by summing the integral of n partitions.

*)
(***hide***)
let midpointChart = 
    let bars = 
        [
            0.1 ,((f 0.) + (f 0.2)) / 2.
            0.3, ((f 0.2) + (f 0.4)) / 2.
            0.5, ((f 0.4) + (f 0.6)) / 2.
            0.7, ((f 0.6) + (f 0.8)) / 2.
            0.9, ((f 0.8) + (f 1.)) / 2.
        ]
        |> fun data -> plotEstimationBars data
    let markers = Chart.Point([for i in 0. .. 0.2 .. 1. -> i, f i], Name = "Partition boundaries", MarkerColor = Color.fromKeyword Black, MarkerSymbol = StyleParam.MarkerSymbol.X)
    [bars; functionChart; markers] |> Chart.combine
    |> Chart.withTitle "Midpoint rule"
    |> Chart.withSize(800,400)

(*** condition: ipynb ***)
#if IPYNB
midpointChart
#endif // IPYNB

(***hide***)
midpointChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

### Trapezoidal rule

The interpolating function is a straight line (an affine function, i.e. a polynomial of degree 1) passing through the partition boundaries of the interval to integrate. 

For a single partition $[a,b]$ in the integration interval, the integral is estimated by

$$
\int_a^b f(x)\,dx \approx (b-a) (\frac{f(a) + f(b)}2)
$$

The integral of the whole integration interval is obtained by summing the integral of n partitions.

*)
(***hide***)
let trapezoidalChart = 
    let lines = 
        [
            0.0, f 0.0
            0.2, f 0.2
            0.4, f 0.4
            0.6, f 0.6
            0.8, f 0.8
            1.0, f 1.0
        ]
        |> fun data -> Chart.Area(data, Name = "Trapezoidal Integrals")
    let indicators = 
        [
            0.0, f 0.0
            0.2, f 0.2
            0.4, f 0.4
            0.6, f 0.6
            0.8, f 0.8
            1.0, f 1.0
        ]
        |> Seq.map (fun (x,y) -> Chart.Line([x,0.; x,y], LineDash = StyleParam.DrawingStyle.Dash, LineColor = Color.fromKeyword Gray, ShowLegend = false))
        |> Chart.combine
    let markers = Chart.Point([for i in 0. .. 0.2 .. 1. -> i, f i], Name = "Partition boundaries", MarkerColor = Color.fromKeyword Black, MarkerSymbol = StyleParam.MarkerSymbol.X)
    [lines; functionChart; indicators; markers] |> Chart.combine
    |> Chart.withTitle "Trapezoidal rule"
    |> Chart.withSize(800,400)

(*** condition: ipynb ***)
#if IPYNB
trapezoidalChart
#endif // IPYNB

(***hide***)
trapezoidalChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

### Simpson's rule (`Simpson`)

For a single partition $[a,b]$ in the integration interval, the integral is estimated by

$$
\int_a^b f(x)\,dx \approx \frac{b - a}6 [f(a) + 4f(\frac{a+b}2) + f(b)]
$$

The integral of the whole integration interval is obtained by summing the integral of n partitions.

This rule can be derived by constructing parabolas that have the value of $f(x)$ for the partition boundaries $a$ and $b$, and the midpoint $m = \frac{a+b}2$ and calculating their definite integral for $[a,b]$

Another possibility to derive this rule is the weighted average of the midpoint ($M$) and trapezoidal ($T$) rules $\frac{2M + T}3$

![Simpson's One-Third Rule.gif](https://upload.wikimedia.org/wikipedia/commons/f/fc/Simpson%27s_One-Third_Rule.gif)

[Source](https://en.wikipedia.org/wiki/Simpson%27s_rule)

*)