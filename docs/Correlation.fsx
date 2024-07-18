(**
---
title: Correlation
index: 5
category: Documentation
categoryindex: 0
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpAux, 2.0.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: OptimizedPriorityQueue, 5.1.0"
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

#endif // IPYNB

(** 

# Correlation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Correlation.ipynb)
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

_Summary_: This tutorial demonstrates how to calculate correlation coefficients in FSharp.Stats

## Sequence correlations

*)
open Plotly.NET
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
    Chart.Table(header, rows, HeaderFillColor = Color.fromHex "#deebf7", CellsFillColor= Color.fromString "lightgrey") 

(*** condition: ipynb ***)
#if IPYNB
table
#endif // IPYNB

(***hide***)
table |> GenericChart.toChartHTML
(***include-it-raw***)

(**

The [Kendall correlation coefficient](https://en.wikipedia.org/wiki/Kendall_rank_correlation_coefficient) calculated by `Seq.kendall` is the Kendall Tau-b coefficient. Three variants are available: 

- `Seq.kendallTauA`: Kendall's Tau-a. Defined as:

  $$\tau_a = \frac{n_c - n_d}{n(n-1)/2}$$

  where $n_c$ is the number of concordant pairs, $n_d$ is the number of discordant pairs, and $n$ is the sample size. Tau-a does not make adjustments for ties.

- `Seq.kendallTauB`: Kendall's Tau-b (this is the default used by `Seq.kendall`). Defined as:  

  $$\tau_b = \frac{n_c - n_d}{\sqrt{(n_0 - n_1)(n_0 - n_2)}}$$
  
  where $n_0 = n(n-1)/2$, $n_1 = \sum_i t_i(t_i-1)/2$, and $n_2 = \sum_j u_j(u_j-1)/2$. Here $t_i$ is the number of tied values in the $i$th group of ties for the first quantity and $u_j$ is the number of tied values in the $j$th group of ties for the second quantity. Tau-b makes adjustments for ties.

- `Seq.kendallTauC`: Kendall's Tau-c. Defined as:

  $$\tau_c = \frac{2(n_c - n_d)}{n^2(m-1)/m}$$
  
  where $m = \min(r,s)$ and $r$ and $s$ are the number of distinct items in each sequence. Tau-c makes an adjustment for set size in addition to ties.

Here's an example illustrating the differences:

*)

// Sequences with no ties
let seqA = [1. .. 10.0]  
let seqB = seqA |> List.map sin

let noTiesTauA = Seq.kendallTauA seqA seqB 
let noTiesTauB = Seq.kendallTauB seqA seqB
let noTiesTauC = Seq.kendallTauC seqA seqB

// Sequences with ties
let seqC = [1.;2.;2.;3.;4.]
let seqD = [1.;1.;1.;4.;4.]  

let tiesTauA = Seq.kendallTauA seqC seqD 
let tiesTauB = Seq.kendallTauB seqC seqD 
let tiesTauC = Seq.kendallTauC seqC seqD 

let tableKendall = 
    let header = ["<b>Correlation measure</b>";"value"]
    let rows = 
        [
            ["Tau-a (no ties)"; sprintf "%3f" noTiesTauA]
            ["Tau-b (no ties)"; sprintf "%3f" noTiesTauB]
            ["Tau-c (no ties)"; sprintf "%3f" noTiesTauC]
            ["Tau-a (ties)";    sprintf "%3f" tiesTauA]
            ["Tau-b (ties)";    sprintf "%3f" tiesTauB]
            ["Tau-c (ties)";    sprintf "%3f" tiesTauC]
        ]
    Chart.Table(header, rows, HeaderFillColor = Color.fromHex "#deebf7", CellsFillColor= Color.fromString "lightgrey")

(*** condition: ipynb ***)
#if IPYNB
tableKendall
#endif // IPYNB

(***hide***)
tableKendall |> GenericChart.toChartHTML
(***include-it-raw***)

(**

As seen, when there are no ties, all three variants give the same result. But with ties present, Tau-b and Tau-c make adjustments and can give different values from Tau-a. `Seq.kendall` uses Tau-b as it is the most commonly used variant.

*)


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
            Color.fromARGB 1 (255 - proportion) 255  proportion
        pearsonCorrelationMatrix
        |> Matrix.toJaggedArray
        |> JaggedArray.map (mapColor -1. 1.)
        |> JaggedArray.transpose
        |> Array.map Color.fromColors
        |> Color.fromColors

    let values = 
        pearsonCorrelationMatrix 
        |> Matrix.toJaggedArray
        |> JaggedArray.map (sprintf "%.3f")

    Chart.Table(["colindex 0";"colindex 1";"colindex 2";"colindex 3"],values,CellsFillColor=cellcolors)

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
    |> Chart.withTraceInfo "Autocorrelation"
    |> Chart.withTitle "Autocorrelation of a gaussian sine wave"
    |> fun c -> 
        [
            Chart.Point(x,yGauss,Name="gaussian") |> Chart.withTemplate ChartTemplates.lightMirrored
            c |> Chart.withTemplate ChartTemplates.lightMirrored
        ]  
        |> Chart.Grid(2,1)

(*** condition: ipynb ***)
#if IPYNB
gaussAC
#endif // IPYNB

(***hide***)
gaussAC |> GenericChart.toChartHTML
(***include-it-raw***)
