(**
---
title: Quantile
index: 19
category: Documentation
categoryindex: 0
---
*)

(*** hide ***)

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

(**

# Quantile

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Quantile.ipynb)

_Summary:_ this tutorial demonstrates how to handle quantiles and QQ-Plots

## Quantiles

Quantiles are values that divide data into equally spaced groups. Percentiles are just quantiles that divide the data in 100 equally sized groups.
The median for example defines the 0.5 quantile or 0.5 percentile. You can calculate the quantile by calculating how many values are less than the value you are interested in.

_Note: There are many possibilities to handle ties or data that cannot be split equally. The default quantile version used in R is `Quantile.mode`._

Let's sample 1000 data points from a normal distribution and calculate some percentiles.
*)

open FSharp.Stats
open FSharp.Stats.Quantile

let rng = Distributions.ContinuousDistribution.normal 3. 1.

let sample = Array.init 1000 (fun _ -> rng.Sample())

let quantile25  = Quantile.mode 0.25 sample
let quantile50  = Quantile.mode 0.50 sample
let quantile75  = Quantile.mode 0.75 sample
let quantile100 = Quantile.mode 1.00 sample


[|quantile25;quantile50;quantile75;quantile100|]
(***include-it-raw***)


(**

These special quantiles are also called quartiles since the divide the data into 4 sections.
Now we can divide the data into the ranges defined by the quantiles and plot them. Here the ranges defines half-open interval:

*)

let range25  = sample |> Array.filter (fun x -> x < quantile25)
let range50  = sample |> Array.filter (fun x -> x > quantile25 && x < quantile50)
let range75  = sample |> Array.filter (fun x -> x > quantile50 && x < quantile75)
let range100 = sample |> Array.filter (fun x -> x > quantile75)

(*** hide ***)
let quartilePlot =
    [|
        Chart.Histogram(range25,"25")   |> Chart.withTemplate ChartTemplates.lightMirrored |> Chart.withXAxisStyle("",MinMax=(0.,6.)) |> Chart.withYAxisStyle("Quartil 25")
        Chart.Histogram(range50,"50")   |> Chart.withTemplate ChartTemplates.lightMirrored |> Chart.withXAxisStyle("",MinMax=(0.,6.)) |> Chart.withYAxisStyle("Quartil 50")
        Chart.Histogram(range75,"75")   |> Chart.withTemplate ChartTemplates.lightMirrored |> Chart.withXAxisStyle("",MinMax=(0.,6.)) |> Chart.withYAxisStyle("Quartil 75")
        Chart.Histogram(range100,"100") |> Chart.withTemplate ChartTemplates.lightMirrored |> Chart.withXAxisStyle("",MinMax=(0.,6.)) |> Chart.withYAxisStyle("Quartil 100")
    |]
    |> Chart.Grid(4,1)

(*** condition: ipynb ***)
#if IPYNB
quartilePlot
#endif // IPYNB

(***hide***)
quartilePlot |> GenericChart.toChartHTML
(***include-it-raw***)


(**

## QQ Plot

QQ plots allow to compare two sample distributions if:

  - the underlying population distribution is unknown or if
  - the relationship between two distributions should be evaluated in greater detail than just their estimated parameters.

When sample is compared to a known distribution, every quantile can be calculated exactly by inverting the CDF. If you compare two samples, there is no uniquely defined CDF, so quantiles have to be interpolated. Additionally
there are various methods for determining Quantiles that differ in handling ties and uneven spacing.

### Comparing two sample distributions

Two sample populations can be compared by QQ-plots where quantiles of the first sample are plotted against quantiles of the second sample. If the sample length is equal, both samples are ordered and plotted as pairs. 

$qq_i = X_i,Y_i$ with X and Y beeing sample sequences of length n and $1 <= i <= n)$.


If samples sizes are unequal the quantiles have to be estimated. Note that this method does not replace a significance test wether the distributions differ statistically.

Lets create four samples of size 300 first:

  - two that are drawn from a normal distribution

  - two that are drawn randomly between 0 and 1

*)


//create samples
let rnd = System.Random()
let norm = Distributions.ContinuousDistribution.normal 0. 1.

///Example 1: Aamples from a standard normal distribution
let normalDistA = Array.init 300 (fun _ -> norm.Sample())
let normalDistB = Array.init 300 (fun _ -> norm.Sample())

///Example 2: Random samples from values between 0 and 1
let evenRandomA = Array.init 300 (fun _ -> rnd.NextDouble())
let evenRandomB = Array.init 300 (fun _ -> rnd.NextDouble())

let exampleDistributions =
    [
        Chart.Histogram(normalDistA,Name="normalDistA") |> Chart.withTemplate ChartTemplates.lightMirrored
        Chart.Histogram(normalDistB,Name="normalDistB") |> Chart.withTemplate ChartTemplates.lightMirrored
        Chart.Histogram(evenRandomA,Name="evenRandomA") |> Chart.withTemplate ChartTemplates.lightMirrored
        Chart.Histogram(evenRandomB,Name="evenRandomB") |> Chart.withTemplate ChartTemplates.lightMirrored
    ]
    |> Chart.Grid(2,2)
    |> Chart.withSize(800.,700.)

(*** condition: ipynb ***)
#if IPYNB
exampleDistributions
#endif // IPYNB

(***hide***)
exampleDistributions |> GenericChart.toChartHTML
(***include-it-raw***)

(**

To compare if two distributions are equal or to identify ranges in which the distributions differ the 100 quantiles from each of the two distributions can be calculated and plotted against each other.
If both distributions are similar, you would expect the quantiles to be identical and therefore are located on the bisector of the QQ-Plot.

Lets calculate the quantiles from _normalDistA_ vs _normalDistB_
*)

// Here a tuple sequence is generated that pairwise contain the same quantiles from normalDistA and normalDistB
let qqData = Signal.QQPlot.fromTwoSamples() normalDistA normalDistB

// Lets check out the first 5 elements in the sequence
Seq.head qqData
(***include-it-raw***)

(**

You can use this tuple sequence and plot it against each other. The diagonal line indicates the bisector where perfect matches would be located.

*)

open FSharp.Stats.Signal
open FSharp.Stats.Signal.QQPlot

//plots QQ plot from two sample populations
let plotFrom2Populations sampleA sampleB =

    //this is the main data plotted as x,y diagram
    let qqData =
        QQPlot.fromTwoSamples() sampleA sampleB
        
    //for a perfect match, all points should be located on the bisector
    let expectedLine = 
        let minimum = min (Quantile.mode 0. sampleA) (Quantile.mode 0. sampleB)
        let maximum = max (Quantile.mode 1. sampleA) (Quantile.mode 1. sampleB)
        [
            minimum,minimum
            maximum,maximum
        ]
        |> Chart.Line
        |> Chart.withTraceName "expected"

    [
        Chart.Point (qqData,Name="QQ")
        expectedLine
    ]
    |> Chart.combine
    |> Chart.withXAxisStyle "Quantiles sample A" 
    |> Chart.withYAxisStyle "Quantiles sample B"
    |> Chart.withTemplate ChartTemplates.lightMirrored

let myQQPlot = plotFrom2Populations normalDistA normalDistB


(*** condition: ipynb ***)
#if IPYNB
myQQPlot
#endif // IPYNB

(***hide***)
myQQPlot |> GenericChart.toChartHTML
(***include-it-raw***)


(**

The both samples were taken from the same normal distribution and therefore they match pretty well.

### Comparing a sample against a normal distribution

You also can plot the quantiles from a sample versus a normal distribution to check if your data is normally distributed.
Your data is z standardized prior to quantile determination to have zero mean and unit variance.

*)

let multipleQQPlots = 
    [
        plotFrom2Populations normalDistA normalDistB |> Chart.withXAxisStyle "normalA" |> Chart.withYAxisStyle "normalB"
        plotFrom2Populations normalDistA evenRandomB |> Chart.withXAxisStyle "normalA" |> Chart.withYAxisStyle "randomB"
        plotFrom2Populations evenRandomA normalDistB |> Chart.withXAxisStyle "randomA" |> Chart.withYAxisStyle "normalB"
        plotFrom2Populations evenRandomA evenRandomB |> Chart.withXAxisStyle "randomA" |> Chart.withYAxisStyle "randomB"
    ]
    |> Chart.Grid(2,2)
    |> Chart.withLegend false
    |> Chart.withSize(800.,700.)

(*** condition: ipynb ***)
#if IPYNB
multipleQQPlots
#endif // IPYNB

(***hide***)
multipleQQPlots |> GenericChart.toChartHTML
(***include-it-raw***)


(**

When QQ-plots are generated for pairwise comparisons, it is obvious, that the random-random and normal-normal samples fit nicely. The cross comparisons between normal and random samples do not match.
Its easy to see that the random smaples are distributed between 0 and 1 while the samples from the normal distributions range from ~-2 to ~2

*)



//The raw qq-plot data of a standard normal distribution and the sample distribution
let qq2Normal sample = QQPlot.fromSampleToGauss(Method=QuantileMethod.Rankit,ZTransform=false) sample


//plots QQ plot from a sample population against a standard normal distribution
let plotFromOneSampleGauss sample =
    
    //this is the main data plotted as x,y diagram
    let qqData = QQPlot.fromSampleToGauss(Method=QuantileMethod.Rankit,ZTransform=false) sample

    let qqChart =
        Chart.Point qqData

    let expectedLine = 
        let minimum = qqData |> Seq.head |> snd
        let maximum = qqData |> Seq.last |> snd
        [
            minimum,minimum
            maximum,maximum
        ]
        |> Chart.Line
        |> Chart.withTraceName "expected"

    [
        qqChart
        expectedLine
    ]
    |> Chart.combine
    |> Chart.withXAxisStyle "Theoretical quantiles" 
    |> Chart.withYAxisStyle "Quantiles gauss"
    |> Chart.withTemplate ChartTemplates.lightMirrored


let myQQPlotOneSampleGauss = plotFromOneSampleGauss normalDistA 

(*** condition: ipynb ***)
#if IPYNB
myQQPlotOneSampleGauss
#endif // IPYNB

(***hide***)
myQQPlotOneSampleGauss |> GenericChart.toChartHTML
(***include-it-raw***)



(**

As seen above the sample perfectly matches the expected quantiles from a normal distribution. This is expected because the sample was generated by sampling from an normal distribution.

*)

let myQQPlotOneSampleRandm = plotFromOneSampleGauss evenRandomA

(*** condition: ipynb ***)
#if IPYNB
myQQPlotOneSampleRandm
#endif // IPYNB

(***hide***)
myQQPlotOneSampleRandm |> GenericChart.toChartHTML
(***include-it-raw***)


(**

As seen above the sample does not matches the expected quantiles from a normal distribution. The sample derives from an random sampling between 0 and 1 and therefore is overrepresented in the tails.

*)