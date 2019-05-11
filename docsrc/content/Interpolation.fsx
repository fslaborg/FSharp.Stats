(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "netstandard.dll"
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
open FSharp.Plotly




(**

#Interpolation

##Polynomial Interpolation

In polynomial interpolation a polynomial is fitted to the data using the least squares approach (see Fitting documentation). By the use of the degree equal to the number of data points - 1 it results in a interpolating curve.
A degree other than n-1 results in a regression polynomial.

*)

open FSharp.Stats
open FSharp.Stats.Fitting.LinearRegression
open FSharp.Plotly

let x_data = vector [|1. .. 4.|]
let y_data = vector [|4.;7.;9.;8.;|]

//Polynomial interpolation

//Define the order the polynomial should have. In Interpolation the order is equal to the data length - 1
let order = x_data.Length - 1
let coefficients = 
    OrdinaryLeastSquares.Polynomial.coefficient order x_data y_data 
let interpolFunction x = 
    OrdinaryLeastSquares.Polynomial.fit order coefficients x

let rawChart = 
    Chart.Point(x_data,y_data)
    |> Chart.withTraceName "raw data"
    
let interpolPol = 
    let fit = [|1. .. 0.01 .. 4.|] |> Array.map (fun x -> x,interpolFunction x)
    fit
    |> Chart.Line
    |> Chart.withTraceName "interpolating polynomial"

(*** do-not-eval ***)
let chartPol = 
    [rawChart;interpolPol] 
    |> Chart.Combine


(*** include-value:chartPol ***)

(**
##Cubic interpolating Spline

Splines are flexible strips of wood, that were used by shipbuilders to draw smooth shapes. In graphics and mathematics a piecewise cubic polynomial (order = 3) is called spline.
The curvature (second derivative) of a cubic polynomial is proportional to its tense energy and in spline theory the curvature is minimized. Therefore the resulting function is very smooth.
To solve for the spline coefficients it is necessary to define two additional constraints, so called boundary conditions:

 - natural spline (most used spline variant): `f''` at borders is set to 0

 - periodic spline: `f'` at first point is the same as `f'` at the last point

 - parabolic spline: `f''` at first/second and last/penultimate knot are equal

 - notAKnot spline: `f'''` at second and penultimate knot are continuous

 - quadratic spline: first and last polynomial are quadratic, not cubic

 - clamped spline: `f'` at first and last knot are set by user

In general piecewise cubic splines only are defined within the region defined by the used x values.

### Related information
 - [Cubic Spline Interpolation](https://en.wikiversity.org/wiki/Cubic_Spline_Interpolation)

 - [Boundary conditions](https://timodenk.com/blog/cubic-spline-interpolation/)

 - [Cubic spline online tool](https://tools.timodenk.com/cubic-spline-interpolation)
*)


open FSharp.Stats.Interpolation
open FSharp.Stats.Fitting.LinearRegression

let x_Data = vector [1.;2.;3.;4.;5.;6.]
let y_Data = vector [1.;8.;6.;3.;7.;1.]

//calculates the spline coefficients for a natural spline
let coeffSpline = 
    CubicSpline.Simple.coefficients CubicSpline.Simple.BoundaryCondition.Natural x_Data y_Data
//splines are only defined within the region defined in x_Data
let fit  x = 
    CubicSpline.Simple.fit coeffSpline x_Data x
//to fit x_Values that are out of the region defined in x_Data you have to force it (Caution!).
let fitForce x = 
    CubicSpline.Simple.fitForce coeffSpline x_Data x

//to compare the spline fit with an interpolating polynomial a leastSquares fit with order = n - 1 is created.
let coeffPolynomial = 
    OrdinaryLeastSquares.Polynomial.coefficient 5 x_Data y_Data
let fitPol x = 
    OrdinaryLeastSquares.Polynomial.fit 5 coeffPolynomial x

(*** do-not-eval ***)
let splineChart =
    [
    Chart.Point(x_Data,y_Data)                                           |> Chart.withTraceName "raw data"
    [1. .. 0.1 .. 6.] |> List.map (fun x -> x,fitPol x)    |> Chart.Line |> Chart.withTraceName "fitPolynomial"
    [-1. .. 0.1 .. 8.] |> List.map (fun x -> x,fitForce x) |> Chart.Line |> Chart.withTraceName "fitSplineForce"
    [1. .. 0.1 .. 6.] |> List.map (fun x -> x,fit x)       |> Chart.Line |> Chart.withTraceName "fitSpline"
    ]
    |> Chart.Combine
    |> Chart.Show


(*** include-value:splineChart ***)

(*** do-not-eval ***)
//additionally you can calculate the derivatives of the spline
[
Chart.Point(x_Data,y_Data) |> Chart.withTraceName "raw data"
[1. .. 0.1 .. 6.] |> List.map (fun x -> x,fit x) |> Chart.Line  |> Chart.withTraceName "spline fit"
[1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.Simple.getFirstDerivative  coeffSpline x_Data x) |> Chart.Point |> Chart.withTraceName "fst derivative"
[1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.Simple.getSecondDerivative coeffSpline x_Data x) |> Chart.Point |> Chart.withTraceName "snd derivative"
[1. .. 0.1 .. 6.] |> List.map (fun x -> x,CubicSpline.Simple.getThirdDerivative  coeffSpline x_Data x) |> Chart.Point |> Chart.withTraceName "trd derivative"
]
|> Chart.Combine
|> Chart.Show



(**
Interpolation.Approximation
Interpolation.LinearSpline
*)
