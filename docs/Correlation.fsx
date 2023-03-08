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
#I "../src/FSharp.Stats/bin/Release/netstandard2.0/"
#r "FSharp.Stats.dll"
#r "nuget: Plotly.NET, 4.0.0"

open Plotly.NET
open Plotly.NET.StyleParam
open Plotly.NET.LayoutObjects

//some axis styling
module Chart = 
    let myAxis name = LinearAxis.init(Title=Title.init name,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,ShowGrid=false,ShowLine=true)
    let myAxisRange name (min,max) = LinearAxis.init(Title=Title.init name,Range=Range.MinMax(min,max),Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,ShowGrid=false,ShowLine=true)
    let withAxisTitles x y chart = 
        chart 
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withXAxis (myAxis x) 
        |> Chart.withYAxis (myAxis y)

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: Plotly.NET.Interactive, 4.0.0"
#r "nuget: FSharp.Stats"

open Plotly.NET
#endif // IPYNB

(** 

# Correlation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Correlation.ipynb)

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
    Chart.Table(header, rows, HeaderFillColor = Color.fromHex "#deebf7", CellsFillColor= Color.fromString "lightgrey") 

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
            Chart.Point(x,yGauss,Name="gaussian") |> Chart.withAxisTitles "" ""
            c |> Chart.withAxisTitles "" ""
        ]  
        |> Chart.Grid(2,1)

(*** condition: ipynb ***)
#if IPYNB
gaussAC
#endif // IPYNB

(***hide***)
gaussAC |> GenericChart.toChartHTML
(***include-it-raw***)
