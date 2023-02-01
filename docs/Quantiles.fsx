(**
---
title: Quantile
index: 20
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

### Table of contents

 - [Quantiles](#Quantiles)
 - [QQ plot](#QQ-plot)
   - [Comparing two sample distributions](#Comparing-two-sample-distributions)
   - [Comparing a sample against a distribution](#Comparing-a-sample-against-a-distribution)
     - [Normal distribution](#Normal-distribution)
     - [Uniform Distribution](#Uniform-Distribution)
- [Quantile normalization](#Quantile-normalization)

## Quantiles

Quantiles are values that divide data into equally spaced groups. Percentiles are just quantiles that divide the data in 100 equally sized groups.
The median for example defines the 0.5 quantile or 0.5 percentile. You can calculate the quantile by what proportion of values are less than the value you are interested in.

_Note: There are many possibilities to handle ties or data that cannot be split equally. The default quantile method used here is `Quantile.mode`._

Let's sample 1000 data points from a normal distribution and calculate some percentiles.
*)
open System
open FSharp.Stats
open FSharp.Stats.Signal

let rng = Distributions.ContinuousDistribution.normal 3. 1.

let sample = Array.init 1000 (fun _ -> rng.Sample())

let quantile25  = Quantile.mode 0.25 sample
let quantile50  = Quantile.mode 0.50 sample
let quantile75  = Quantile.mode 0.75 sample
let quantile100 = Quantile.mode 1.00 sample


[|quantile25;quantile50;quantile75;quantile100|]
(***include-it-raw***)


(**

These special quantiles are also called quartiles since they can be used to divide the data into 4 sections.
The ranges that can be defined by the quantiles are plotted below. Here the ranges defines half-open intervals between two quartiles.

*)

let range25  = sample |> Array.filter (fun x -> x < quantile25)
let range50  = sample |> Array.filter (fun x -> x > quantile25 && x < quantile50)
let range75  = sample |> Array.filter (fun x -> x > quantile50 && x < quantile75)
let range100 = sample |> Array.filter (fun x -> x > quantile75)

(*** hide ***)
let quartileRangePlot =
    [|
        Chart.Histogram(range25,"0-25",ShowLegend=false)   |> Chart.withTemplate ChartTemplates.lightMirrored |> Chart.withXAxisStyle("",MinMax=(0.,6.)) |> Chart.withYAxisStyle("Quartil 0-25")
        Chart.Histogram(range50,"25-50",ShowLegend=false)   |> Chart.withTemplate ChartTemplates.lightMirrored |> Chart.withXAxisStyle("",MinMax=(0.,6.)) |> Chart.withYAxisStyle("Quartil 25-50")
        Chart.Histogram(range75,"50-75",ShowLegend=false)   |> Chart.withTemplate ChartTemplates.lightMirrored |> Chart.withXAxisStyle("",MinMax=(0.,6.)) |> Chart.withYAxisStyle("Quartil 50-75")
        Chart.Histogram(range100,"75-100",ShowLegend=false) |> Chart.withTemplate ChartTemplates.lightMirrored |> Chart.withXAxisStyle("",MinMax=(0.,6.)) |> Chart.withYAxisStyle("Quartil 75-100")
    |]
    |> Chart.Grid(4,1)


(*** condition: ipynb ***)
#if IPYNB
quartileRangePlot
#endif // IPYNB

(***hide***)
quartileRangePlot |> GenericChart.toChartHTML
(***include-it-raw***)


(**

## QQ plot

QQ plots allow to compare sample distributions if:

  - the underlying population distribution is unknown or if
  - the relationship between two distributions should be evaluated in greater detail than just their estimated parameters.

When a sample is compared to a known distribution, every quantile can be calculated exactly by inverting their CDF. If you compare two samples, there is no uniquely defined CDF, 
so quantiles have to be interpolated. 

### Comparing two sample distributions

Two sample populations can be compared by QQ-plots where quantiles of the first sample are plotted against quantiles of the second sample. If the sample length is equal, both samples are ordered and plotted as pairs. 

$qq_i = X_i,Y_i$ with $X$ and $Y$ beeing ordered sample sequences of length $n$ and $(1 \le i \le n)$


If samples sizes are unequal the quantiles of the larger data set have to be interpolated from the quantiles of the smaller data set. 

**Lets create four samples of unequal sizes first:**

 - two that are drawn from a normal distribution of mean $3.0$ and standard deviation $0.5$

 - two that are drawn randomly between 0 and 1

*)


//create samples
let rnd = System.Random()
let norm = Distributions.ContinuousDistribution.normal 3.0 0.5

///Example 1: Samples from a normal distribution
let normalDistA = Array.init 300 (fun _ -> norm.Sample())
let normalDistB = Array.init 250 (fun _ -> norm.Sample())

///Example 2: Random samples from values between 0 and 1
let evenRandomA = Array.init 270 (fun _ -> rnd.NextDouble())
let evenRandomB = Array.init 280 (fun _ -> rnd.NextDouble() + 1.)

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

To compare if two distributions are equal or to identify ranges in which the distributions differ, a quantile pair from each of the two distributions can be calculated and plotted against each other.
If both distributions are similar, you would expect the quantiles to be identical and therefore are located on a straight line that additionally is located on the bisector! If the samples are of different length $m$ and $n$ the number 
of quantiles is limited to $min$ $m$ $n$. For every data point of the smaller data set a corresponding quantile of the larger data set is determined.

Lets calculate the quantiles from _normalDistA_ vs _normalDistB_.
*)

// Here a tuple sequence is generated that pairwise contain the same quantiles from normalDistA and normalDistB
let qqData = QQPlot.fromTwoSamples normalDistA normalDistB

// Lets check out the first 5 elements in the sequence
Seq.take 5 qqData
(***include-it-raw***)

(**

You can use this tuple sequence and plot it against each other.

*)

open FSharp.Stats.Signal
open FSharp.Stats.Signal.QQPlot


//plots QQ plot from two sample populations
let plotFrom2Populations sampleA sampleB sampleNameA sampleNameB =

    //here the coordinates are calculated
    let qqCoordinates = QQPlot.fromTwoSamples sampleA sampleB

    Chart.Point (qqCoordinates,Name="QQ")
    |> Chart.withXAxisStyle $"Quantiles {sampleNameA}" 
    |> Chart.withYAxisStyle $"Quantiles {sampleNameB}"
    |> Chart.withTemplate ChartTemplates.lightMirrored

let myQQplot1 = plotFrom2Populations normalDistA normalDistB "sample A" "sample B"


(*** condition: ipynb ***)
#if IPYNB
myQQplot1
#endif // IPYNB

(***hide***)
myQQplot1 |> GenericChart.toChartHTML
(***include-it-raw***)


(**

Both samples were taken from the same distribution (here normal distribution) and therefore they match pretty well.

In the following plot you can see four comparisons of the four distributions defined in the beginning (2x normal + 2x uniform).

*)

let multipleQQPlots = 
    [
        plotFrom2Populations normalDistA normalDistB "normalA" "normalB"
        plotFrom2Populations normalDistA evenRandomB "normalA" "randomB"
        plotFrom2Populations evenRandomA normalDistB "randomA" "normalB"
        plotFrom2Populations evenRandomA evenRandomB "randomA" "randomB"
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

When QQ-plots are generated for pairwise comparisons, it is obvious, that the _random_-_random_ and _normal_-_normal_ samples fit nicely. 

## Attention

Please note that the _random_-_random_ comparison may be misleading! Despite the fact, that the QQ-plot forms a straight line, the underlying distributions differ greatly (_randomA_ ranges from 0 to 1 while _randomB_
ranges from 1 to 2. This is indicated in the QQ plot as the x- and y-axis ranges differ. The formed straight line does _not_ correspond to the bisector.

The cross comparisons between normal and random samples do not match because their quantiles differ.
Its easy to see that the random samples are distributed between 0 and 1 (or 1 and 2) while the samples from the normal distributions range from $1$ to ~$5$.


### Comparing a sample against a distribution

You can plot the quantiles from a sample versus a known distribution to check if your data follows the given distribution. 

_Note that a QQ plot does not replace a significance test wether the distributions differ statistically._

There are various methods to determine quantiles that differ in handling ties and uneven spacing.


```
Quantile determination methods(rank,sampleLength):
  - Blom          -> (rank - 3. / 8.) / (sampleLength + 1. / 4.)
  - Rankit        -> (rank - 1. / 2.) / sampleLength
  - Tukey         -> (rank - 1. / 3.) / (sampleLength + 1. / 3.)
  - VanDerWerden  -> rank / (sampleLength + 1.)
```


#### Normal distribution

The data can be z standardized prior to quantile determination to have zero mean and unit variance. If the data is zTransformed the bisector defines a perfect match.

*)

// The raw qq-plot data of a standard normal distribution and the sample distribution
// defaults: 
//   Method:     QuantileMethod.Rankit
//   ZTransform: false
let qq2Normal sample = QQPlot.toGauss(Method=QuantileMethod.Rankit,ZTransform=true) sample

// plots QQ plot from a sample population against a standard normal distribution. 
// if the data is zTransformed the bisector defines a perfect match.
let plotFromOneSampleGauss sample =
    
    //this is the main data plotted as x,y diagram
    let qqData = QQPlot.toGauss(Method=QuantileMethod.Rankit,ZTransform=true) sample

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
    |> Chart.withXAxisStyle "Theoretical quantiles (normal)" 
    |> Chart.withYAxisStyle "Sample quantiles"
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

// compare the uniform sample against a normal distribution
let my2QQPlotOneSampleGauss = plotFromOneSampleGauss evenRandomA 


(*** condition: ipynb ***)
#if IPYNB
my2QQPlotOneSampleGauss
#endif // IPYNB

(***hide***)
my2QQPlotOneSampleGauss |> GenericChart.toChartHTML
(***include-it-raw***)


(**

As seen above the sample does not match the expected quantiles from a normal distribution. The sample derives from an random sampling between 0 and 1 and therefore is overrepresented in the tails.


#### Uniform Distribution

You also can plot your data against a uniform distribution. Data can be standardized to lie between $0$ and $1$
*)

let uniform = 
    QQPlot.toUniform(Method=QuantileMethod.Rankit,Standardize=false) normalDistA
    |> Chart.Point
    |> Chart.withXAxisStyle "Theoretical quantiles (uniform)" 
    |> Chart.withYAxisStyle "Sample quantiles"
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
uniform
#endif // IPYNB

(***hide***)
uniform |> GenericChart.toChartHTML
(***include-it-raw***)

(**

#### Any specified distribution

You also can plot your data against a distribution you can specify. You have to define the _inverse CDF_ or also called the _Quantile function_.

**LogNormal distribution**

*)

// generate a sample from a lognormal distriution
let sampleFromLogNormal =
    let d = Distributions.ContinuousDistribution.logNormal 0. 1.
    Array.init 500 (fun _ -> d.Sample())

// define the quantile function for the log normal distribution with parameters mu = 0 and sigma = 1
let quantileFunctionLogNormal p = 
    let mu = 0.
    let sigma = 1.
    Math.Exp (mu + Math.Sqrt(2. * (pown sigma 2)) * SpecialFunctions.Errorfunction.inverf(2. * p - 1.))

let logNormalNormalDist = QQPlot.toInvCDF(quantileFunctionLogNormal,Method=QuantileMethod.Rankit) normalDistA

let logNormalLogNormal  = QQPlot.toInvCDF(quantileFunctionLogNormal,Method=QuantileMethod.Rankit) sampleFromLogNormal

let logNormalChart = 
    [
        Chart.Point(logNormalNormalDist,Name="normal sample")
        Chart.Point(logNormalLogNormal,Name="log normal sample")
    ]
    |> Chart.combine
    |> Chart.withXAxisStyle "Theoretical quantiles Log Normal" 
    |> Chart.withYAxisStyle "Sample quantiles"
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
logNormalChart
#endif // IPYNB

(***hide***)
logNormalChart |> GenericChart.toChartHTML
(***include-it-raw***)


(**

The log normal sample fits nicely to the bisector, but the sample from the normal distribution does not fit

# Quantile normalization

For the FSharp.Stats quantile normalization please refer to the [Normalization documentation](https://fslab.org/FSharp.Stats/Normalization.html). 
For clarity, the normalization in this documentation is entirely performed within the snippets.

When you want to compare e.g. intensity measurements of elements between samples, you often have to normalize the samples in order
to be able to perform a valid comparison. Samples may vary in their average intensity just because of the technical nature of the measurement itself, 
thereby distorting the underlying correct/real distribution. It is assumed that global changes across the samples are due to unwanted technical variability and only
a small number of elements are dysregulated (Zhao et al, 2020).

To compensate for this technical variance you can perform a quantile normalization (_Note: `Signal.Normalization.medianOfRatios` could be an alternative_). It is a technique for making two or more
distributions identical in statistical properties and was originally developed for gene expression microarrays. It sees widespread use, constituting a standard part
of analysis pipelines for high-throughput analysis.
You can either quantile normalize data according to given reference distribution (e.g. Gamma or Normal distribution) or create your own reference distribution out of your samples.
For the latter some data is generated that does not share the same intensity range:

*)

let namesA,dataA = Array.init 20 (fun i -> $"A_%02i{i+1}",rnd.NextDouble()      ) |> Array.unzip
let namesB,dataB = Array.init 20 (fun i -> $"B_%02i{i+1}",rnd.NextDouble() + 0.1) |> Array.unzip
let namesC,dataC = Array.init 20 (fun i -> $"C_%02i{i+1}",rnd.NextDouble() / 2.0) |> Array.unzip


let rawDataChart = 
    [
        Chart.Point(dataA |> Array.map (fun value -> "sampleA",value),MultiText=namesA,TextPosition=TextPosition.MiddleRight,ShowLegend=false)
        Chart.Point(dataB |> Array.map (fun value -> "sampleB",value),MultiText=namesB,TextPosition=TextPosition.MiddleRight,ShowLegend=false)
        Chart.Point(dataC |> Array.map (fun value -> "sampleC",value),MultiText=namesC,TextPosition=TextPosition.MiddleLeft,ShowLegend=false)
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withTitle "Raw data"

(*** condition: ipynb ***)
#if IPYNB
rawDataChart
#endif // IPYNB

(***hide***)
rawDataChart |> GenericChart.toChartHTML
(***include-it-raw***)


(**
As you can see, the average intensity varies greatly between samples A, B, and C. If you want to e.g. compare element 11 of each sample, you have to perform a 
sample normalization prior to this comparison. It is called quantile normalization, because the final normalized samples have the **same quantiles** and the **same statistical properties**.

For the normalization the data is ranked, the intensities of each rank x are averaged and this intensity is set as normalized intensity for each sample at rank x.
*)

let rankedA =
    Rank.RankFirst() dataA
    |> Array.zip3 namesA dataA
    |> Array.sortBy (fun (a,b,c) -> c)

let rankedB =
    Rank.RankFirst() dataB
    |> Array.zip3 namesB dataB
    |> Array.sortBy (fun (a,b,c) -> c)

let rankedC =
    Rank.RankFirst() dataC
    |> Array.zip3 namesC dataC
    |> Array.sortBy (fun (a,b,c) -> c)


let rankDataChart = 
    let valA,rankA = rankedA |> Array.map (fun (name,value,rank) -> ("sampleA",value),rank) |> Array.unzip
    let valB,rankB = rankedB |> Array.map (fun (name,value,rank) -> ("sampleB",value),rank) |> Array.unzip
    let valC,rankC = rankedC |> Array.map (fun (name,value,rank) -> ("sampleC",value),rank) |> Array.unzip
    let meanRank11 = [valA.[10];valB.[10];valC.[10]] |> Seq.meanBy snd
    [
        Chart.Point(valA,MultiText=rankA,TextPosition=TextPosition.MiddleRight,ShowLegend=false)
        Chart.Point(valB,MultiText=rankB,TextPosition=TextPosition.MiddleRight,ShowLegend=false)
        Chart.Point(valC,MultiText=rankC,TextPosition=TextPosition.MiddleLeft,ShowLegend=false)
        Chart.Point([valA.[10];valB.[10];valC.[10]],ShowLegend=false) |> Chart.withMarkerStyle(Size=10)
        Chart.Line(["sampleA",meanRank11;"sampleC",meanRank11],LineColor=Color.fromHex "#d62728",LineDash=DrawingStyle.Dash,ShowLegend=false) 
    ]
    |> Chart.combine
    |> Chart.withTitle "Ranks"
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
rankDataChart
#endif // IPYNB

(***hide***)
rankDataChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
As seen above, the ranks assign ascending indices for each data point within a sample. The data point of rank 11 varies a lot between the samples. To compensate this offset the average of the values at each rank
is calculated (red dashed line for rank 11). The values of the data points at each rank are subsequently shifted to their mean (their reference).
*)

///quantile normalization of ranked samples
let normA,normB,normC =
    Array.map3 (fun (nameA,intA,_) (nameB,intB,_) (nameC,intC,_) -> 
        let reference = [intA;intB;intC] |> Seq.mean
        (nameA,reference),(nameB,reference),(nameC,reference)
        ) 
        rankedA rankedB rankedC
    |> Array.unzip3



let normalizedDataChart = 
    let valA,rankA = normA |> Array.map (fun (name,value) -> ("sampleA",value),name) |> Array.unzip
    let valB,rankB = normB |> Array.map (fun (name,value) -> ("sampleB",value),name) |> Array.unzip
    let valC,rankC = normC |> Array.map (fun (name,value) -> ("sampleC",value),name) |> Array.unzip
    [
        Chart.Point(valA,MultiText=rankA,TextPosition=TextPosition.MiddleRight,ShowLegend=false)
        Chart.Point(valB,MultiText=rankB,TextPosition=TextPosition.MiddleRight,ShowLegend=false)
        Chart.Point(valC,MultiText=rankC,TextPosition=TextPosition.MiddleLeft,ShowLegend=false)
        Chart.Point([valA.[10];valB.[10];valC.[10]],ShowLegend=false) |> Chart.withMarkerStyle(Size=10)
    ]
    |> Chart.combine
    |> Chart.withTitle "Normalized data"
    |> Chart.withTemplate ChartTemplates.lightMirrored

(*** condition: ipynb ***)
#if IPYNB
normalizedDataChart
#endif // IPYNB

(***hide***)
normalizedDataChart |> GenericChart.toChartHTML
(***include-it-raw***)


(**
Now let us check why this technique is called quantile normalization. First, we create QQ-Plots of the raw and normalized samples to compare them.

*)


let QQPlotRawAB   = plotFrom2Populations dataA dataB "raw data A" "raw data B"
let QQPlotRawBC   = plotFrom2Populations dataB dataC "raw data B" "raw data C"
let QQPlotQNormAB = plotFrom2Populations (Array.map snd normA) (Array.map snd normB) "qnorm data A" "qnorm data B"
let QQPlotQNormBC = plotFrom2Populations (Array.map snd normB) (Array.map snd normC) "qnorm data B" "qnorm data C"

let qNormPlot = 
    [
        QQPlotRawAB  |> Chart.withTraceName "rawAB"
        QQPlotQNormAB|> Chart.withTraceName "qnormAB"
        QQPlotRawBC  |> Chart.withTraceName "rawBC"
        QQPlotQNormBC|> Chart.withTraceName "qnormBC"
    ]
    |> Chart.Grid(2,2)

(*** condition: ipynb ***)
#if IPYNB
qNormPlot
#endif // IPYNB

(***hide***)
qNormPlot |> GenericChart.toChartHTML
(***include-it-raw***)


(**
It is obvious that the quantiles of the samples are identical after the normalization. Even the raw data QQ-Plots look straight because they all derive from a normal distribution.
But it is easy to see, that the absolute quantile values differ (the quantiles are not located on the bisector). 

### Additional remarks:

  - In this documentation `RankFirst` was used as method for handling ties. If ties are present, the first occurence is assigned to rank x, the next is assigned to rank (x+1).

  - Quantile normalization is susceptible to batch effects when blindly applied to whole data sets! When handling several conditions with multiple replicates each, it may be beneficial to group the samples of each condition and quantile normalize them separately (Zhao et al, 2020).

  - For the standard quantile normalization the number of data point in each sample has to be equal.

  - When strong effects are expected between the samples, it is useful to make special allowances for outliers.

  - "A particular danger in the use of QN is that lay analysts are easily misled by the rather "perfect-looking" post-normalization results" (Zhao et al, 2020)

### References

  - Hicks S, Irzarry R, 2014, When to use Quantile Normalization?, https://doi.org/10.1101/012203
  
  - Zhao, Wong, and Goh, 2020, How to do quantile normalization correctly for gene expression data analyses, doi: 10.1038/s41598-020-72664-6

*)



