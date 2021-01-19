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

# Correlation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Correlation.ipynb)

_Summary_: This tutorial demonstrates how to autocorrelate a signal in FSharp.Stats

### Table of contents

 - [Autocorrelation](#Autocorrelation)

## Autocorrelation

[Autocorrelation](https://en.wikipedia.org/wiki/Autocorrelation), also known as serial correlation, is the correlation of a signal with a delayed copy of itself as a function of delay. 
Informally, it is the similarity between observations as a function of the time lag between them. 
The analysis of autocorrelation is a mathematical tool for finding repeating patterns, such as the presence of a periodic signal obscured by noise, or identifying the missing fundamental frequency in a signal implied by its harmonic frequencies.

*)

open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Distributions.Continuous
open FSharp.Stats.Correlation

let lags = [0..100]
let x = [0. .. 100.]

//// Autocorrelation of a gaussian signal
let gaussPDF = Normal.PDF 10. 2.
let yGauss = x |> List.map gaussPDF |> vector

let autoCorrGauss = lags |> List.map (fun lag -> autoCorrelation lag yGauss)

open Plotly.NET

let gaussAC =
    Chart.Point(lags,autoCorrGauss)
    |> Chart.withTraceName "Autocorrelation"
    |> Chart.withTitle "Autocorrelation of a gaussian sine wave"
    |> fun c -> Chart.Stack 1 [Chart.Point(x,yGauss,Name="gaussian");c]

(*** condition: ipynb ***)
#if IPYNB
gaussAC
#endif // IPYNB

(***hide***)
gaussAC |> GenericChart.toChartHTML
(***include-it-raw***)
