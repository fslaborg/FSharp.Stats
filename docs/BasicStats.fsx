(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Newtonsoft.JSON"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta3"
#r "nuget: Plotly.NET.Interactive, 2.0.0-alpha5"
#r "nuget: FSharp.Stats"
#endif // IPYNB

open Plotly.NET
open Plotly.NET.Axis
open Plotly.NET.StyleParam

(**
# Basics

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=BasicStats.ipynb)

_Summary:_ this tutorial gives an overview over how to do some of the basic statistical measurements with FSharp.Stats.

### Table of contents

 - [Central tendency](#Central-tendency)
    - [Mean](#Mean)
    - [Truncated mean](#Truncated-mean)
    - [Median](#Median)
    - [Harmonic mean](#Harmonic-mean)
    - [Geometric mean](#Geometric-mean)
 - [Dispersion](#Dispersion)
    - [Range](#Range)
    - [Variance and Standard Deviation](#Variance-and-standard-deviation)
    - [Coefficient of variation](#Coefficient-of-variation)

## Central tendency

A [central tendency](https://en.wikipedia.org/wiki/Central_tendency) (or measure of central tendency) is a central or typical value for a probability distribution.
It may also be called a center or location of the distribution. Colloquially, measures of central tendency are often called averages.

### Mean

For a data set, the arithmetic [mean](https://en.wikipedia.org/wiki/Mean), also called the expected value or average, 
is the central value of a discrete set of numbers: specifically, 
the sum of the values divided by the number of values:

$\bar{x} = \frac{1}{n}\left (\sum_{i=1}^n{x_i}\right ) = \frac{x_1+x_2+\cdots +x_n}{n}$

`mean` is available as a Sequence (and other collections) extension, as well as `meanBy`, 
which takes an additional converter function:
*)
open FSharp.Stats

let mean1 = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.meanBy float

(***include-value:mean1***)

let mean2 = 
    [10.; 2.; 19.; 24.; 6.; 23.; 47.; 24.; 54.; 77.;]
    |> Seq.mean

(***include-value:mean2***)

(**
### Truncated mean

Computes the truncated (trimmed) mean where a given percentage of the highest and lowest values are discarded. 
In total 2 times the given percentage are discarded:

`meanTruncated` is available as a Sequence (and other collections) extension, as well as `meanTruncatedBy`, 
which takes an additional converter function:
*)

let truncMean1 = 
    [10.; 2.; 19.; 24.; 6.; 23.; 47.; 24.; 54.; 77.;]
    |> Seq.meanTruncated 0.2

(***include-value:truncMean1***)

let truncMean2 = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.meanTruncatedBy float 0.2

(***include-value:truncMean2***)

(**
### Median

The [median](https://en.wikipedia.org/wiki/Median) is a value separating the higher half from the lower half of a data sample, a population, or a probability distribution. 
For a data set, it may be thought of as "the middle" value: if you sort the values of a collection by size, the median is the value in central position. 
Therefore, there are as many bigger values as smaller values than the median in the collection.
If there is an even number of observations, then there is no single middle value; the median is then usually defined to be the mean of the two middle values.

`median` is available as a equence (and other collections) extension:
*)

let median1 = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.median

(***include-value:median1***)

(**
### Harmonic mean

The [harmonic mean](https://en.wikipedia.org/w/index.php?title=Harmonic_mean&action=edit&section=1) can be expressed as the reciprocal of the arithmetic mean of the reciprocals of the given set of observations.
It is typically appropriate for situations when the average of rates is desired.

$H = \frac{n}{\frac1{x_1} + \frac1{x_2} + \cdots + \frac1{x_n}} = \frac{n}{\sum\limits_{i=1}^n \frac1{x_i}} = \left(\frac{\sum\limits_{i=1}^n x_i^{-1}}{n}\right)^{-1}.$

`meanHarmonic` is available as a sequence (and other collections) extension, as well as `meanHarmonicBy`, 
which takes an additional converter function:
*)

let harmonicMean1 = 
    [10.; 2.; 19.; 24.; 6.; 23.; 47.; 24.; 54.; 77.;]
    |> Seq.meanHarmonic

(***include-value:harmonicMean1***)

let harmonicMean2 = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.meanHarmonicBy float

(***include-value:harmonicMean2***)

(**

### Geometric mean

The [geometric mean](https://en.wikipedia.org/wiki/Geometric_mean) indicates the central tendency or typical value of 
a set of numbers by using the product of their values (as opposed to the arithmetic mean which uses their sum). 
The geometric mean is defined as the nth root of the product of n numbers:

$\left(\prod_{i=1}^n x_i\right)^\frac{1}{n} = \sqrt[n]{x_1 x_2 \cdots x_n}$

`meanGeometric` is available as a sequence (and other collections) extension, as well as `meanGeometricBy`, 
which takes an additional converter function:

*)

let geometricMean1 = 
    [10.; 2.; 19.; 24.; 6.; 23.; 47.; 24.; 54.; 77.;]
    |> Seq.meanGeometric

(***include-value:geometricMean1***)

let geometricMean2 = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.meanGeometricBy float 
 
(***include-value:geometricMean2***)

(**
## Dispersion

[Dispersion](https://en.wikipedia.org/wiki/Statistical_dispersion) (also called variability, scatter, or spread) is the extent to which a distribution 
is stretched or squeezed.

### Range

The [range](https://en.wikipedia.org/wiki/Range_(statistics)) of a set of data is the difference between the largest and smallest values.

`range` is available as a sequence (and other collections) extension, as well as `rangeBy`, 
which takes an additional converter function:

_Note:_ instead of returning the absolute difference between max and min value, these functions return an interval with these values as boundaries.
***)

let range1 = 
    [10.; 2.; 19.; 24.; 6.; 23.; 47.; 24.; 54.; 77.;]
    |> Seq.rangeBy float

(***include-value:range1***)

let range2 = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.rangeBy float

(***include-value:range1***)

(**
### Variance and Standard Deviation

The [variance](https://en.wikipedia.org/wiki/Variance)

$s_N^2 = \frac{1}{N} \sum_{i=1}^N \left(x_i - \bar{x}\right)^2$

and the [standard deviation](https://en.wikipedia.org/wiki/Standard_deviation)

$s_N = \sqrt{\frac{1}{N} \sum_{i=1}^N \left(x_i - \bar{x}\right)^2}$

are measures of dispersion the values of a collection have. While the standard deviation has the same unit as the values of the collection the variance has the squared unit. 

`varPopulation` and `stDevPopulation` are available as sequence (and other collections) extensions, as well as `varPopulationBy` and `stDevPopulationBy`, 
which take an additional converter function:

*)

let data = [|1.;3.;5.;4.;2.;8.|]

let varPopulation = Seq.varPopulation data

(***include-value:varPopulation***)

let stdPopulation = Seq.stDevPopulation data

(***include-value:stdPopulation***)


(**
If the full population is **not** given, the calculation lacks in one degree of freedom, so the Bessel corrected version of the calculation has to be used (results in higher values):

$s^2 = \frac{1}{N - 1} \sum_{i=1}^N \left(x_i - \bar{x}\right)^2$ for the unbiased variance estimation, and

$s = \sqrt{\frac{1}{N-1} \sum_{i=1}^N \left(x_i - \bar{x}\right)^2}$ for the corrected standard deviation.

`var` and `stDev` are available as sequence (and other collections) extensions, as well as `varBy` and `stDevBy`, 
which take an additional converter function:
*)

let varSample = Seq.var data

(***include-value:varSample***)

let stdSample = Seq.stDev data

(***include-value:stdSample***)

(**
### Coefficient of variation

The coefficient of variation is the mean-normalized standard deviation:

$\widehat{c_{\rm v}} = \frac{s}{\bar{x}}$

It describes the ratio of the standard deviation to the mean. It assists in comparing measurement variability
with varying amplitudes. Use only if data is measured with a ratio scale (meaningful zero values and meaningful intervals).

`cv` is available as a sequence (and other collections) extension, as well as `cvBy`, 
which takes an additional converter function:

*)
let sample1 =   [1.;4.;2.;6.;5.;3.;2.;]
let sample2 =   [13.;41.;29.;8.;52.;34.;25.;]

let cvSample1 = Seq.cv sample1

(***include-value:cvSample1***)

let cvSample2 = Seq.cv sample2

(***include-value:cvSample2***)
