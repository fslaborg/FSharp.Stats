(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta3"
#r "nuget: Plotly.NET.Interactive, 2.0.0-alpha5"
#r "nuget: FSharp.Stats"
#endif // IPYNB


(** 

#Covariance

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Covariance
.ipynb)

_Summary:_ This tutorial explains how to investigate the covariance of two samples with FSharp.Stats

Lets first define some sample data:
*)

open FSharp.Stats

let rnd = System.Random()
let error() = rnd.Next(11)

let sampleA = Vector.init 50 (fun x -> float x)
let sampleB = Vector.init 50 (fun x -> float (x + error()))
let sampleBHigh = sampleB |> Vector.map (fun x -> 200. + x)
let sampleC = Vector.init 50 (fun x -> 100. - float (x + 3 * error()))
let sampleD = Vector.init 50 (fun x -> 100. + float (10 * error()))

open Plotly.NET

//Some axis styling
let myAxis title = Axis.LinearAxis.init(Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
let styleChart x y chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxis y)

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

(**

The [covariance](https://en.wikipedia.org/wiki/Covariance) of two samples describes the relationship of both variables. If one variable 
tends to be high if its pair is high also, the covariance is positive. If on variable is low while its pair is high
the covariance is negative. If there is no (monotone) relationship between both variables, the covariance is zero. 

A positive covariance indicates a positive slope of a regression line, while a negative covariance indicates a negative slope.
If the total population is given the covPopulation without Bessel's correction can be calculated.

$\operatorname{cov}(X, Y) = \operatorname{E}{\big[(X - \operatorname{E}[X])(Y - \operatorname{E}[Y])\big]}$

_Note: The amplitude of covariance does not correlate with the slope, neither it correlates with the spread of the data points from the regression line._

A standardized measure for how well the data lie on the regression line is given by correlation analysis. The pearson correlation coefficient
is defined as 

$\rho_{X,Y}= \frac{\operatorname{cov}(X,Y)}{\sigma_X \sigma_Y}$

**References:**

- Fahrmeir L et al., Statistik - Der Weg zur Datenanalyse, 8. Auflage, doi 10.1007/978-3-662-50372-0

`cov` and `covPopulation` are available as sequence (and other collections) extensions:

*)

let covAB     = Vector.cov sampleA sampleB
let covAC     = Vector.cov sampleA sampleC
let covAD     = Vector.cov sampleA sampleD
let covABHigh = Vector.cov sampleA sampleBHigh

let covPopAB     = Vector.covPopulation sampleA sampleB
let covPopAC     = Vector.covPopulation sampleA sampleC
let covPopAD     = Vector.covPopulation sampleA sampleD
let covPopABHigh = Vector.covPopulation sampleA sampleBHigh

open Correlation
let pearsonAB     = Seq.pearson sampleA sampleB
let pearsonAC     = Seq.pearson sampleA sampleC
let pearsonAD     = Seq.pearson sampleA sampleD
let pearsonABHigh = Seq.pearson sampleA sampleBHigh

(*** condition: ipynb ***)
#if IPYNB
sampleChart
#endif // IPYNB

(***hide***)
sampleChart |> GenericChart.toChartHTML
(***include-it-raw***)

(***hide***)
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
