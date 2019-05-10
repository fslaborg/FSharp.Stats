(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"
open FSharp.Plotly
#r "netstandard.dll"




(**

#Interpolation

##Polynomial Interpolation

In polynomial interpolation a higher degree (d > 1) polynomial is fitted to the data. The coefficients are chosen that the sum of squared residuals is minimized.

*)

open FSharp.Stats
open FSharp.Stats.Fitting.LinearRegression
open FSharp.Plotly

let x_Data = vector [|1. .. 4.|]
let y_Data = vector [|4.;7.;9.;8.;|]

//Polynomial regression

//Define the order the polynomial should have. In Interpolation the order is equal to the data length
let order = x_Data.Length
let coefficients = OrdinaryLeastSquares.Polynomial.coefficient order x_Data y_Data 
let interpolFunction x = OrdinaryLeastSquares.Polynomial.fit order coefficients x

let rawChart = 
    Chart.Point(x_Data,y_Data)
    |> Chart.withTraceName "raw data"
    
let interpolPol = 
    let fit = [|1. .. 0.01 .. 4.|] |> Array.map (fun x -> x,interpolFunction x)
    Chart.Line(fit)
    |> Chart.withTraceName "interpolating polynomial"

(*** do-not-eval ***)
[rawChart;interpolPol] 
|> Chart.Combine
|> Chart.Show

(**
##Cubic interpolating Spline

Splines are flexible strips of wood, that were used by shipbuilders to draw smooth shapes. In graphics and mathematics a piecewise cubic polynomial (order = 3) is called spline.
The curvature (second derivative) of a cubic polynomial is proportional to its tense energy and in spline theory the curvature is minimized. Therefore the resulting function is very smooth


*)

(**
Interpolation.Approximation
Interpolation.LinearSpline.
*)
