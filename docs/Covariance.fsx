(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta3"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta3"
#r "nuget: FSharp.Stats"
#endif // IPYNB


(** 

#Covariance

_Summary:_ This tutorial explains how to investigate the covariance of two samples with FSharp.Stats
*)

open Plotly.NET
open Plotly.NET.Axis
open Plotly.NET.StyleParam
open FSharp.Stats

let myAxis title = LinearAxis.init(Title=title,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
let myAxisRange title range = LinearAxis.init(Title=title,Range=Range.MinMax range,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
let styleChart x y chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxis y)
let styleChartRange x y rx ry chart = chart |> Chart.withX_Axis (myAxisRange x rx) |> Chart.withY_Axis (myAxisRange y ry)

let rnd = System.Random()
let error() = rnd.Next(11)

let sampleA = Vector.init 50 (fun x -> float x)
let sampleB = Vector.init 50 (fun x -> float (x + error()))
let sampleBHigh = sampleB |> Vector.map (fun x -> 200. + x)
let sampleC = Vector.init 50 (fun x -> 100. - float (x + 3 * error()))
let sampleD = Vector.init 50 (fun x -> 100. + float (10 * error()))


(**

The covariance of two samples describes the relationship of both variables. If one variable 
tends to be high if its pair is high also, the covariance is positive. If on variable is low while its pair is high
the covariance is negative. If there is no (monotone) relationship between both variables, the covariance is zero. 

A positive covariance indicates a positive slope of a regression line, while a negative covariance indicates a negative slope.
If the total population is given the covPopulation without Bessel's correction can be calculated.

Cov(X,Y) = E([X-E(X)][Y-E(Y)])

_Note: The amplitude of covariance does not correlate with the slope, neither it correlates with the spread of the data points from the regression line._

A standardized measure for how well the data lie on the regression line is given by correlation analyis. The pearson correlation coefficient
is defined as &rho;(X,Y)=Cov(X,Y)/(&sigma;<sub>X</sub> &sigma;<sub>Y</sub>).


References:

- Fahrmeir L et al., Statistik - Der Weg zur Datenanalyse, 8. Auflage, doi 10.1007/978-3-662-50372-0

*)

let sampleChart =
    [
        Chart.Point(sampleA,sampleB,"AB")
        Chart.Point(sampleA,sampleC,"AC")
        Chart.Point(sampleA,sampleD,"AD")  
        Chart.Point(sampleA,sampleBHigh,"AB+")   
    ]
    |> Chart.Combine
    |> styleChart "x" "y"
    |> Chart.withTitle "test cases for covariance calculation"

(*** condition: ipynb ***)
#if IPYNB
sampleChart
#endif // IPYNB

(***hide***)
sampleChart |> GenericChart.toChartHTML
(***include-it-raw***)

let covAB     = FSharp.Stats.Vector.cov sampleA sampleB
let covAC     = FSharp.Stats.Vector.cov sampleA sampleC
let covAD     = FSharp.Stats.Vector.cov sampleA sampleD
let covABHigh = FSharp.Stats.Vector.cov sampleA sampleBHigh

let covPopAB     = FSharp.Stats.Vector.covPopulation sampleA sampleB
let covPopAC     = FSharp.Stats.Vector.covPopulation sampleA sampleC
let covPopAD     = FSharp.Stats.Vector.covPopulation sampleA sampleD
let covPopABHigh = FSharp.Stats.Vector.covPopulation sampleA sampleBHigh

(*** hide ***)
let pearsonAB     = Correlation.Seq.pearson sampleA sampleB
let pearsonAC     = Correlation.Seq.pearson sampleA sampleC
let pearsonAD     = Correlation.Seq.pearson sampleA sampleD
let pearsonABHigh = Correlation.Seq.pearson sampleA sampleBHigh

let covs = 
    sprintf """Covariance of the presented four test cases
AB (blue)   cov: %.2f    covPopulation: %.2f   pearson: %.3f
AC (orange) cov: %.1f    covPopulation: %.1f   pearson: %.2f
AD (green)  cov: %.2f    covPopulation: %.2f   pearson: %.3f
AB+(red)    cov: %.2f    covPopulation: %.2f   pearson: %.3f""" 
        covAB      covPopAB     pearsonAB    
        covAC      covPopAC     pearsonAC    
        covAD      covPopAD     pearsonAD    
        covABHigh  covPopABHigh pearsonABHigh

(*** include-value:covs ***)

(***hide***)
sampleChart |> GenericChart.toChartHTML
(***include-it-raw***)

