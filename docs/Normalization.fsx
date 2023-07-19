(**
---
title: Normalization
index: 18
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
open Plotly.NET.StyleParam
open Plotly.NET.LayoutObjects

(**

# Normalization

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Normalization.ipynb)
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

_Summary:_ this tutorial demonstrates multiple ways of data normalization accross several samples

### Table of contents
 - [Introduction](#Introduction)
 - [Median of Ratios](#Median-of-ratios)
 - [Quantile normalization](#Quantile-normalization)

## Introduction

When you want to compare e.g. intensity measurements of elements between samples, you often have to normalize the samples in order
to be able to perform a valid comparison. Samples may vary in their average intensity just because of the technical nature of the measurement itself, 
thereby shifting or distorting the underlying correct/real distribution. For the presented normalization strategies it is assumed that global changes across the samples are due to unwanted technical variability and only
a small number of elements are dysregulated (Zhao et al, 2020).


## Median of ratios

To correct for batch effects or technical measurment differences the median of ratios normalization (MOR) can be applied.
As explained above, it expects that the majority of genes/proteins/elements do not differ between the conditions that should be compared [Love et al. 2014](https://doi.org/10.1186/s13059-014-0550-8).
At first a reference sample is determined by calculating the geometric mean of each of the **n** elements across all **m** samples. The calculation of the geometric mean as 
the n<sup>th</sup> square root of the product of all values (1) seems odd, but when displayed as mean of the log-transformed data (2) it becomes clear, that the geometric 
mean is just an outlier-insensitive measure of a robust average, which is intuitive to do when dealing with biological data or in general data with some extreme outliers.

1. <img src="https://render.githubusercontent.com/render/math?math=geomean = \sqrt[n]{\prod_{}^{}x}">
2. <img src="https://render.githubusercontent.com/render/math?math=geomean = e^{\frac{1}{n}\sum_{}^{}log(x)}">

After a reference sample for each row is determined, for each entry a correction factor to reach the reference is calculated. Each sample now has n associated correction factors.
By taking the median of those individual correction factors, an outlier-insensitive measure of the true correction factor is determined. By choosing the median it is assumed that in theory >=50% of all
measured elements should not change between the measured samples. 

No prior log-transform has to be applied before normalizing the data with this method. If required a log transform can be applied to the normalized values to restore homoscedasticity.

In the following a step-by-step introduction is given with finally applying `Signal.Normalization.medianOfRatios` function to the given data.

**1. Raw intensities for m=3 samples and n=6 elements:**

|elementID|sample1|sample2|sample3|
|--|--|--|--|
|g00|100|130|30|
|g01|80|200|30|
|g02|0|50|0|
|g03|40|50|20|
|g04|50|45|25|
|g05|40|50|15|


**2. Zero intensities cannot be processed during the MOR-normalization.** 
Either imputation, row filtering or data transformation must be performed. In this example a single count is added to each intensity. 
Note that this is not necessarily reasonable for every analysis! The reference sample is determined by calculating the geometric mean of every row.

|elementID|sample1|sample2|sample3|reference|
|--|--|--|--|--|
|g00|101|131|31|74.30|
|g01|81|201|31| 79.62|
|g02|1|51|1|    3.71 |
|g03|41|51|21|  35.28|
|g04|51|56|26|  42.03|
|g05|41|51|16|  32.22|

**3. For every element the correction factor is determined by dividing it by its reference.**

|elementID|sample1|sample2|sample3|reference|corr1|corr2|corr3|
|--|--|--|--|--|--|--|--|
|g00|101|131|31|74.30|1.359|1.763|0.417|
|g01|81|201|31| 79.62|1.017|2.525|0.389|
|g02|1|51|1|    3.71 |0.270|13.752|0.270|
|g03|41|51|21|  35.28|1.162|1.446|0.595|
|g04|51|56|26|  42.03|1.213|1.332|0.619|
|g05|41|51|16|  32.22|1.272|1.583|0.497|

It becomes apparent, that sample1 and sample2 are overrepresented and sample3 is underepresented. g02 and g03 are obviously outliers that differ between the samples. 
These elements should not influence the correction factors. By determining the median of each sample correction column, a outlier insensitive approximation of the true correction 
factor is achieved.


**4. The median of each correction factor column determines the final correction.**

|elementID|sample1|sample2|sample3|reference|corr1|corr2|corr3|
|--|--|--|--|--|--|--|--|
|g00|101|131|31|74.30|1.359|1.763|0.417|
|g01|81|201|31| 79.62|1.017|2.525|0.389|
|g02|1|51|1|    3.71 |0.270|13.752|0.270|
|g03|41|51|21|  35.28|1.162|1.446|0.595|
|g04|51|56|26|  42.03|1.213|1.332|0.619|
|g05|41|51|16|  32.22|1.272|1.583|0.497|
|||||||||
|   |  |  |  |  **Median:**     |**1.188**|**1.673**|**0.457**|

**5. Apply the correction factors to the original (untransformed) data.**

|elementID|sample1|sample2|sample3|reference|corr1|corr2|corr3||normedSample1|normedSample2|normedSample3|
|--|--|--|--|--|--|--|--|--|--|--|--|
|g00|101|131|31|74.30|1.359|1.763|0.417|| 84.19|77.71|65.66 |
|g01|81|201|31| 79.62|1.017|2.525|0.389|| 67.35|119.5|565.66|
|g02|1|51|1|    3.71 |0.270|13.752|0.270|| 0.00|29.89|0.00  |
|g03|41|51|21|  35.28|1.162|1.446|0.595|| 33.68|29.89|43.77 |
|g04|51|56|26|  42.03|1.213|1.332|0.619|| 42.10|32.88|54.72 |
|g05|41|51|16|  32.22|1.272|1.583|0.497|| 33.68|29.89|32.83 |
|||||||||||||
|   |  |  |  | **Median:**  |**1.188**|**1.673**|**0.457**|||||

The normed intensities of the different elements now match a theoretical common intensity level.

*)


open FSharp.Stats


let rawData = 
    [|
    [|100.; 130.; 30.|]
    [| 80.; 200.; 30.|]
    [|  0.;  50.;  0.|]
    [| 40.;  50.; 20.|]
    [| 50.;  45.; 25.|]
    [| 40.;  50.; 15.|]
    |]
    |> matrix

// visualization of the raw data
let rawDataChart = 
    rawData.Transpose
    |> Matrix.toJaggedArray
    |> Array.mapi (fun sampleID sample -> 
        let sampleIntensities = 
            sample 
            |> Array.mapi (fun gID intensity ->
                gID,intensity
            )
        Chart.Column(sampleIntensities,Name = sprintf "raw sample%i" (sampleID+1))
    )
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle "gID"
    |> Chart.withYAxisStyle "raw intensity"
    |> Chart.withTitle "raw data"


(*** condition: ipynb ***)
#if IPYNB
rawDataChart
#endif // IPYNB

(***hide***)
rawDataChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
In the above figure you can see, that the green sample is underrepresented, and the orange is overrepresented when compared to the blue one. 
No the MOR normalization is applied. If no transformation (+1) should be applied, you can use `Signal.Normalization.medianOfRatios`.

*)

/// actual median of ratios normalization. 1 is added to each intensity to get rid of the empty intensities.
let mor = 
    Signal.Normalization.medianOfRatiosBy (fun x -> x + 1.0) rawData

/// Matrix of normalized data in the same order as the input matrix
let morNormedData = 
    mor.NormedData

/// Correction factors to assess the strenght of the applied normalization (1 indicates no normalization)
let corrFactors = 
    let cf1 = Seq.item 0 mor.CorrFactors
    let cf2 = Seq.item 1 mor.CorrFactors
    let cf3 = Seq.item 2 mor.CorrFactors
    sprintf "Correctionfactor for sample1 is %.3f\nCorrectionfactor for sample2 is %.3f\nCorrectionfactor for sample3 is %.3f" cf1 cf2 cf3

(*** condition: ipynb ***)
#if IPYNB
corrFactors
#endif // IPYNB

(***hide***)
corrFactors
(***include-it-raw***)

// visualization of the normed data
let normedDataChart =
    morNormedData.Transpose
    |> Matrix.toJaggedArray
    |> Array.mapi (fun sampleID sample -> 
        let sampleIntensities = 
            sample 
            |> Array.mapi (fun gID intensity ->
                gID,intensity
            )
        Chart.Column(sampleIntensities,Name = sprintf "MOR normed sample%i" (sampleID+1))
    )
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle "gID"
    |> Chart.withYAxisStyle "MOR intensity"

(*** condition: ipynb ***)
#if IPYNB
normedDataChart
#endif // IPYNB

(***hide***)
normedDataChart |> GenericChart.toChartHTML
(***include-it-raw***)


(**

The normed data now shows high similarity of the individual element intensities across samples.

# Quantile normalization

To compensate for the technical variance you also can perform a quantile normalization. It is a technique for making two or more
distributions identical in statistical properties and was originally developed for gene expression microarrays. It sees widespread use, constituting a standard part
of analysis pipelines for high-throughput analysis.
You can either quantile normalize data according to a given reference distribution (e.g. Gamma or Normal distribution) or create your own reference distribution out of your samples.

The same dataset as above is used and a quantile normalization is performed. Since no log transform is applied, there is no necessity to add a constant to each intensity:

**1. Raw intensities for m=3 samples and n=6 elements:**

|elementID|sample1|sample2|sample3|
|--|--|--|--|
|g00|100|130|30|
|g01|80|200|30|
|g02|0|50|0|
|g03|40|50|20|
|g04|50|45|25|
|g05|40|50|15|


**2. Intensities are indexed and sorted in ascending order. The row average intensity is determined.**

|sample1|sample2|sample3| |row average|
|--|--|--|--|--|
|3 -> 0|5 -> 45|3 -> 0    | |15| 
|4 -> 40|3 -> 50|6 -> 15  | |35|
|6 -> 40|4 -> 50|4 -> 20  | |36.7|
|5 -> 50|6 -> 50|5 -> 25  | |41.7|
|2 -> 80|1 -> 130|1 -> 30 | |80|
|1 -> 100|2 -> 200|2 -> 30| |110|


**3. Every intensity is replaced by the row average intensity**

|sample1|sample2|sample3| |row average|
|--|--|--|--|--|
|3;15	|5;15	|3;15   ||15| 
|4;35	|3;35	|6;35   ||35|
|6;36.7	|4;36.7	|4;36.7 ||36.7|
|5;41.7	|6;41.7	|5;41.7 ||41.7|
|2;80	|1;80	|1;80   ||80|
|1;110	|2;110	|2;110  ||110|


**4. Finally the columns are resorted to their original order using the indices. The indices are removed**

|elementID|normedSample1|normedSample2|normedSample3|
|--|--|--|--|
|g00|110  |80   |80   |
|g01|80   |110  |110  |
|g02|15   |35   |15   |
|g03|35   |36.7 |36.7 |
|g04|41.7 |15   |41.7 |
|g05|36.7 |41.7 |35   |



*)

let quantileNorm = 
    Signal.Normalization.quantile rawData

// visualization of the normed data
let normedDataQuantileChart =
    quantileNorm.Transpose
    |> Matrix.toJaggedArray
    |> Array.mapi (fun sampleID sample -> 
        let sampleIntensities = 
            sample 
            |> Array.mapi (fun gID intensity ->
                gID,intensity
            )
        Chart.Column(sampleIntensities,Name = sprintf "quantile normed sample%i" (sampleID+1))
    )
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle "gID"
    |> Chart.withYAxisStyle "qNorm intensity"

(*** condition: ipynb ***)
#if IPYNB
normedDataQuantileChart
#endif // IPYNB

(***hide***)
normedDataQuantileChart |> GenericChart.toChartHTML
(***include-it-raw***)


(**

As seen in the normalized column chart, the final samples all consists of the same values, just assigned to different row indices according to the rank within the sample.
For data with low row count, this normalization is not appropriate because there is a severe disturbance of the intensity values. The more data is incorporated (more rows), the lower
the single value influence will be.

## Notes

You can assess the quality of your normalization by performing a PCA prior and after normalization and compare different normalization strategies. While the 
correction factors for MOR can be investigated, quantile normalization is a non-linear transformation since every value is normalized by its own rank-row mean. 



*)


