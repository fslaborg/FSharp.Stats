(**
---
title: Fit quality
index: 11
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

open Plotly.NET

(**

# Fit quality

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/GoodnessOfFit.ipynb)
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

_Summary:_ this tutorial shows how to assess fit quality with FSharp.Stats

### Table of contents

 - [Linear regression report](#Linear-regression-report)
 - [Confidence bands](#Confidence-bands)
 - [Prediction bands](#Prediction-bands)
 - [Cook's distance](#Cook-s-distance)

## Linear regression report

Consider this simple linear regression:
*)
open FSharp.Stats
open FSharp.Stats.Fitting
open LinearRegression.OrdinaryLeastSquares
open GoodnessOfFit.OrdinaryLeastSquares.Linear.Univariable
open FSharp.Stats.Distributions

//data sorted by x values
let x = vector [|1. .. 10.|]
let y = vector [|4.;10.;9.;7.;13.;17.;16.;23.;15.;30.|]

///linear regression line fitting function
let coefficients = Linear.Univariable.coefficient x y
let fitFunc = Linear.Univariable.fit coefficients

let fittedValues = x |> Seq.map fitFunc


let chart =
    [
    Chart.Point(x,y) |> Chart.withTraceInfo "raw"
    Chart.Line(fittedValues|> Seq.mapi (fun i y -> x.[i],y)) |> Chart.withTraceInfo "fit"
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
chart
#endif // IPYNB

(***hide***)
chart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
Various quality parameters can be accessed via the `GoodnessOfFit` module:
*)

//In the following some quality/interval/significance values are computed:
let sos         = GoodnessOfFit.calculateSumOfSquares fitFunc x y
let n           = sos.Count
let meanX       = sos.MeanX
let meanY       = sos.MeanY
let slope       = coefficients.[1]
let intercept   = coefficients.[0]
//coefficient of determination
let rSq        = GoodnessOfFit.calculateDeterminationFromValue y fittedValues
//adjusted coefficient of determination; variable=number of coefficints (excluding intercept)
let rSqAdj    = GoodnessOfFit.calculateDeterminationAdj y fittedValues 1
//pearson correlation coefficient
let r           = sqrt rSq
//total sum of squares
let ssTotal     = sos.Total
//regression sum of squares
let ssReg       = sos.Regression
//residual sum of squares
let ssResidual  = sos.Error
//sum of squares xx
let ssxx        = sos.SSxx
//sum of products xy
let ssxy        = sos.SSxy
//standard error of the regression slope
let stdErrSlope = GoodnessOfFit.standardErrorSlope sos 
//standard error of the regression intercept
let stdErrIntercept = GoodnessOfFit.standardErrorIntercept sos 
//standard error of the estimate (S)
let stdErrEstimate  = GoodnessOfFit.standardErrorEstimate sos 
//confidence intervals (df = n-#coefficients; a=5%)
let criticalT   = Testing.TTest.getCriticalTValue (n - 2.) 0.05 Testing.TTest.TwoTailed
let lowerS      = slope - criticalT * stdErrSlope
let upperS      = slope + criticalT * stdErrSlope
let lowerI      = intercept - criticalT * stdErrIntercept
let upperI      = intercept + criticalT * stdErrIntercept
//significance tests
let testSlope   = GoodnessOfFit.ttestSlope slope sos
let testInterc  = GoodnessOfFit.ttestIntercept intercept sos

  
let outputTable = 
    let header = ["<b>ParameterName</b>";"Value";"StandardError (SE Coeff)"]
    let rows = 
        let print f = sprintf "%.3f" f
        [
        ["n";               sprintf "%.0f" n;       "-"] 
        ["meanX";           print meanX;            "-"]
        ["meanY";           print meanY;            "-"]
        ["slope";           print slope;            print stdErrSlope]
        ["intercept" ;      print intercept;        print stdErrIntercept]
        ["<b>Goodness of fit</b>";"";               ""]
        ["SS_total";        print ssTotal;          ""]
        ["SS_regression";   print ssReg;            ""]
        ["SS_residual";     print ssResidual;       ""]
        ["r (pearson cor. coef.";               print r;                ""]
        ["r_squared";       print rSq;             ""]      
        ["r_squared_adj";   print rSqAdj;         ""]
        ["SE Estimate";     print stdErrEstimate;   ""]   
        ["<b>95% Confidence interval</b>";"<b>min</b>";    "<b>max</b>"]
        ["slope";           print lowerS;           print upperS]
        ["intercept";       print lowerI;           print upperI]
        ["<b>significances</b>";"";                 ""]
        ["slope p Value";   print testSlope.PValue; ""]
        ["intercept p Value";print testInterc.PValue;""]
        ]
    Chart.Table(
        header, 
        rows, 
        HeaderFillColor = Color.fromString "#deebf7",
        CellsFillColor  = Color.fromColors [Color.fromString "#deebf7"; Color.fromString "white";Color. fromString "white"],
        CellsMultiAlign  = [StyleParam.HorizontalAlign.Left;StyleParam.HorizontalAlign.Center]
        )
    |> Chart.withTitle "Regression report"
    

(*** condition: ipynb ***)
#if IPYNB
outputTable
#endif // IPYNB

(***hide***)
outputTable |> GenericChart.toChartHTML
(***include-it-raw***)

(**
## Confidence bands

A confidence band shows the uncertainty of an curve estimate. It widens towards the periphery. 

A prediction band shows the uncertainty of a value of a new data point.

In both cases homoscedasticity is assumed.

*)

//data sorted by x values
let xData = vector [|1. .. 10.|]
let yData = vector [|4.;10.;9.;7.;13.;17.;16.;23.;15.;30.|]
//let xData = vector [|1.47;1.50;1.52;1.55;1.57;1.60;1.63;1.65;1.68;1.70;1.73;1.75;1.78;1.80;1.83|]
//let yData = vector [|52.21;53.12;54.48;55.84;57.20;58.57;59.93;61.29;63.11;64.47;66.28;68.10;69.92;72.19;74.46|]
let values = Seq.zip xData yData

///linear regression line fitting function
let coeffs = Linear.Univariable.coefficient xData yData
let fit = Linear.Univariable.fit coeffs

let fitValues = xData |> Seq.map (fun xi -> xi,(fit xi))

///calculate confidence band errors for every x value
let confidence = 
    xData
    |> Vector.map (calculateConfidenceBandError xData yData 0.95)

///lower and upper bounds of the 95% confidence band sorted according to x values
let (lower,upper) = 
    xData 
    |> Vector.toArray
    |> Array.mapi (fun i xi -> (fit xi) - confidence.[i],(fit xi) + confidence.[i]) 
    |> Array.unzip

let rangePlot = 
    [
    Chart.Range (
        xy=fitValues,
        lower=lower,
        upper=upper,
        mode = StyleParam.Mode.Lines,
        LineColor = Color.fromKeyword ColorKeyword.Blue,
        RangeColor = Color.fromKeyword ColorKeyword.LightBlue
        )
        |> Chart.withTraceInfo "CI95"
    Chart.Point (values,MarkerColor=Color.fromString "#000000") |> Chart.withTraceInfo "raw"
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withTitle "Confidence band 95%"

(*** condition: ipynb ***)
#if IPYNB
rangePlot
#endif // IPYNB

(***hide***)
rangePlot |> GenericChart.toChartHTML
(***include-it-raw***)

(**
The confidence band calculation is not limited to the original x values. To get a smooth confidence band, introduce additional x values in small steps.
*)

let newXValues =
    vector [|1. .. 0.5 .. 11.|]

///calculate confidence band errors for every x value
let newConfidence = 
    newXValues
    |> Vector.map (calculateConfidenceBandError xData yData 0.95)

///lower and upper bounds of the 95% confidence band sorted according to x values
let (newLower,newUpper) = 
    newXValues 
    |> Vector.toArray
    |> Array.mapi (fun i xi -> (fit xi) - newConfidence.[i],(fit xi) + newConfidence.[i]) 
    |> Array.unzip

let linePlot =
    [
    Chart.Point(xData,yData) |> Chart.withTraceInfo (sprintf "%.2f+%.4fx" coeffs.[0] coeffs.[1])
    Chart.Line(fitValues) |> Chart.withTraceInfo "linear regression"
    Chart.Line(newXValues,newLower,LineColor= Color.fromString "#C1C1C1") |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceInfo "lower"
    Chart.Line(newXValues,newUpper,LineColor= Color.fromString "#C1C1C1") |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceInfo "upper"
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withTitle "Confidence band 95%"

(*** condition: ipynb ***)
#if IPYNB
rangePlot
#endif // IPYNB

(***hide***)
linePlot |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Prediction bands

*)

let predictionXValues = vector [|1. .. 0.5 .. 15.|]
///calculate preditcion band errors for every x value
let prediction = 
    predictionXValues
    |> Vector.map (calculatePredictionBandError xData yData 0.95)

///lower and upper bounds of the 95% prediction band sorted according to x values
let (pLower,pUpper) = 
    predictionXValues 
    |> Vector.toArray
    |> Array.mapi (fun i xi -> (fit xi) - prediction.[i],(fit xi) + prediction.[i]) 
    |> Array.unzip

let predictionPlot =
    [
    Chart.Point(xData,yData) |> Chart.withTraceInfo (sprintf "%.2f+%.4fx" coeffs.[0] coeffs.[1])
    Chart.Line(fitValues) |> Chart.withTraceInfo "linear regression"
    Chart.Line(predictionXValues,pLower,LineColor= Color.fromString "#C1C1C1") |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceInfo "pLower"
    Chart.Line(predictionXValues,pUpper,LineColor= Color.fromString "#C1C1C1") |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceInfo "pUpper"
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withTitle "Prediction band"


(***hide***)
predictionPlot |> GenericChart.toChartHTML
(***include-it-raw***)


(**
## Cook's distance

Leverage: Leverage describes the potential impact of data points regarding their regression line. Points that show a great dependent-variable-distance to all other points, have a 
higher potential to distort the regression line coefficients (high-leverage points).

Cooks distance (D) is a measure to describe the influence of each data point to the regression line.
If D is low, the influence is low, while a high D indicates an 'influential observation' that is worth taking a closer look.
Cooks distance is a mixture of the residual sum of squares at the particular point and its leverage.

A linear threshold is arbitrarily defined by either 1, 4/n, or 3*mean(D).
Because outliers have a strong influence to D of all other points as well, the thresholds should not be applied without checking the issues by eye.

*)

open LinearRegression.OrdinaryLeastSquares.Linear

let xD = vector [|1. .. 10.|]
let yD = vector [|4.;6.;9.;7.;13.;17.;16.;23.;14.;26.|]

let cooksDistance = Univariable.cooksDistance xD yD

let nD         = float xD.Length
let meanCook   = Seq.mean cooksDistance
let threshold1 = 1.
let threshold2 = 4. / nD
let threshold3 = 3. * meanCook

let cook = 
    [
    Chart.Column (Seq.zip xD cooksDistance) |> Chart.withTraceInfo "cook's distance"
    Chart.Line([0.5,threshold1;10.5,threshold1])|> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceInfo "t=1"
    Chart.Line([0.5,threshold2;10.5,threshold2])|> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceInfo "t=4/n"
    Chart.Line([0.5,threshold3;10.5,threshold3])|> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceInfo "t=3*mean(D)"
    ]
    |> Chart.combine    
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withYAxisStyle "cook's distance"
    |> fun l -> [(Chart.Point(xD,yD) |> Chart.withTraceInfo "raw");l] |> Chart.Grid(2,1)
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withTitle "Cook's distance"
    |> Chart.withSize (650.,650.)

(*** condition: ipynb ***)
#if IPYNB
cook
#endif // IPYNB

(***hide***)
cook |> GenericChart.toChartHTML
(***include-it-raw***)

