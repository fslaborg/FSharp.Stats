(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

open Plotly.NET

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta8"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta8"
#r "nuget: FSharp.Stats"
#endif // IPYNB

(** 

# Correlation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Correlation.ipynb)

_Summary_: This tutorial demonstrates how to autocorrelate a signal in FSharp.Stats

### Table of contents

 - [Sequence correlations](#Sequence correlations)
 - [Matrix correlations](#Matrix correlations)
 - [Autocorrelation](#Autocorrelation)

## Sequence correlations

*)
open FSharp.Stats
open FSharp.Stats.Correlation

let sampleA = [|3.4;2.5;6.5;0.2;-0.1|]
let sampleB = [|3.1;1.5;4.2;1.2;2.0|]

let pearson =   Seq.pearson sampleA sampleB
let pearsonW =  Seq.pearsonWeighted  sampleA sampleB [1.;1.;1.;2.;1.;]
let spearman =  Seq.spearman sampleA sampleB
let kendall =   Seq.kendall sampleA sampleB
let bicor =     Seq.bicor sampleA sampleB

let table = 
    let header = ["<b>Correlation measure</b>";"value"]
    let rows = 
        [
        ["Pearson";                 sprintf "%3f" pearson ]       
        ["Pearson weighted";        sprintf "%3f" pearsonW]
        ["Spearman";                sprintf "%3f" spearman]
        ["Kendall";                 sprintf "%3f" kendall ]
        ["Biweight midcorrelation"; sprintf "%3f" bicor   ]     
        ]
    Chart.Table(header, rows, ColorHeader = "#deebf7", ColorCells = "lightgrey") 

(*** condition: ipynb ***)
#if IPYNB
table
#endif // IPYNB

(***hide***)
table |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Matrix correlations

*)
let m = 
    [
        [0.4;1.2;4.5]
        [1.2;0.5;-0.1]
        [5.0;19.8;2.4]
        [-6.0;-2.;0.0]
    ]
    |> matrix

let pearsonCorrelationMatrix = 
    Correlation.Matrix.rowWiseCorrelationMatrix Correlation.Seq.pearson m


let table2 = 
    //Assign a color to every cell seperately. Matrix must be transposed for correct orientation.
    let cellcolors = 
        //map color from value to hex representation
        let mapColor min max value = 
            let proportion =  int (255. * (value - min) / (max - min))
            Colors.fromRgb (255 - proportion) 255  proportion
            |> Colors.toWebColor
        pearsonCorrelationMatrix
        |> Matrix.toJaggedArray
        |> JaggedArray.map (mapColor -1. 1.)
        |> JaggedArray.transpose

    let values = 
        pearsonCorrelationMatrix 
        |> Matrix.toJaggedArray
        |> JaggedArray.map (sprintf "%.3f")

    Chart.Table(["colindex 0";"colindex 1";"colindex 2";"colindex 3"],values,ColorCells=cellcolors)

(*** condition: ipynb ***)
#if IPYNB
table2
#endif // IPYNB

(***hide***)
table2 |> GenericChart.toChartHTML
(***include-it-raw***)


(**

## Autocorrelation

[Autocorrelation](https://en.wikipedia.org/wiki/Autocorrelation), also known as serial correlation, is the correlation of a signal with a delayed copy of itself as a function of delay. 
Informally, it is the similarity between observations as a function of the time lag between them. 
The analysis of autocorrelation is a mathematical tool for finding repeating patterns, such as the presence of a periodic signal obscured by noise, or identifying the missing fundamental frequency in a signal implied by its harmonic frequencies.

*)


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
