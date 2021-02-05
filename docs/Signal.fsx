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

# Signal

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Signal.ipynb)

_Summary:_ this tutorial demonstrates multiple ways of signal processing with FSharp.Stats.

### Table of contents

 - [Outliers](#Outliers)
    - [Tukey's fences](#Tukey-s-fences)
 - [Filtering](#Filtering)
 - [Padding](#Padding)
 - [Wavelet](#Wavelet)
    - [Continuous Wavelet](#Continuous-Wavelet)
    - [Continuous Wavelet 3D](#Continuous-Wavelet-3D)
 - [Fast Fourier transform](#Fast-Fourier-transform)

## Outliers

### Tukey's fences

A common approach for outlier detection is Tukey's fences-method. It determines the interquartile range (IQR) of the 
data and adds a fraction of it to the third quartile (Q3) or substracts it from the first quartile (Q1) respectively. 
A often used fraction of the IQR is k=1.5 for outliers and k=3 for points 'far out'.

In the generation of box plots the same method determines the whiskers and outliers of a sample.

Reference:

  - Tukey, JW. Exploratory data analysis. Addison-Wesely, 1977

*)
open FSharp.Stats
open FSharp.Collections

let sampleO1 = [|45.;42.;45.5;43.;47.;51.;34.;45.;44.;46.;48.;37.;46.;|]

let outlierBordersO1 = Signal.Outliers.tukey 1.5 sampleO1

let lowerBorderO1 = Intervals.getStart outlierBordersO1
// result: 37.16667

let upperBorderO1 = Intervals.getEnd outlierBordersO1
// result: 51.83333

open Plotly.NET

//some axis styling
let myAxis title = Axis.LinearAxis.init(Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
let myAxisRange name range = Axis.LinearAxis.init(Title=name,Range=StyleParam.Range.MinMax(range), Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true)
let styleChart x y chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxis y)
let styleChartRangeY x y mi ma chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxisRange y (mi,ma))

let (inside,outside) =
    sampleO1 
    |> Array.partition (fun x -> Intervals.liesInInterval x outlierBordersO1)

let tukeyOutlierChart =
    [
        Chart.Point(inside |> Seq.map (fun x -> 1,x),"sample")
        Chart.Point(outside |> Seq.map (fun x -> 1,x),"outliers")
    ]
    |> Chart.Combine
    |> Chart.withShapes(
        [
            Shape.init(StyleParam.ShapeType.Line,0.5,1.5,lowerBorderO1,lowerBorderO1,Line=Line.init(Dash=StyleParam.DrawingStyle.Dash,Color="grey"))
            Shape.init(StyleParam.ShapeType.Line,0.5,1.5,upperBorderO1,upperBorderO1,Line=Line.init(Dash=StyleParam.DrawingStyle.Dash,Color="grey"))
        ]
        )
    |> styleChartRangeY "" "" 30. 60.
    |> Chart.withTitle "Tukey's fences outlier borders"
   
(*** condition: ipynb ***)
#if IPYNB
tukeyOutlierChart
#endif // IPYNB

(***hide***)
tukeyOutlierChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Filtering

Savitzgy-Golay description is coming soon.

*)

open FSharp.Stats

// Savitzky-Golay low-pass filtering
let t  = [|-4. ..(8.0/500.).. 4.|]
let dy  = t |> Array.map (fun t -> (-t**2.) + (Distributions.Continuous.Normal.Sample 0. 0.5) )
let dy' = t |> Array.map (fun t -> (-t**2.))

let dysg = Signal.Filtering.savitzkyGolay  31 4 0 1 dy

let savitzgyChart =
    [
        Chart.Point(t, dy, Name="data with noise");
        Chart.Point(t, dy', Name="data without noise");
        Chart.Point(t, dysg, Name="data sg");
    ]
    |> Chart.Combine
    |> styleChart "" ""

(*** condition: ipynb ***)
#if IPYNB
savitzgyChart
#endif // IPYNB

(***hide***)
savitzgyChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Padding

If convolution operations should be performed on a signal trace, it often is necessary to extend (pad) the data with artificial data points.
There are several padding methods:

 - **Zero**: Data points with y-value=zero are introduced. This often is useful when analyzing spectra with sparse data because areas without any data measured are assumed to have zero intensity.

 - **Random**: When the baseline of the measured signal is nonzero like in chromatogramms, it is necessary to insert data points with random y-values taken from the original data set.

 - **Delete**: No datapoints are inserted.

 - **Linear interpolation**: When a linear relationship is assumed in the range betwen two adjacent data points, the padding points should lie on the straight line between those points.


**Three regions can be defined where padding points could be introduced:**

 1. In the beginning and end of the data set artificial data points have to be added to analyse the start- and end-regions of the data. Therefore random data points are chosen from the original data set.
 
 2. If the data is not measured in discrete intervals, the region between two adjacent values have to be padded to ensure sufficient coverage for convolution.
 
 3. If the gap between two adjacent points is too large, another padding method than in case 2. may be chosen.


*)

open FSharp.Stats.Signal

//get raw data
let data = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/data/waveletData.txt")
    |> Array.map (fun x -> 
        let tmp = x.Split([|'\t'|])
        float tmp.[0],float tmp.[1])

///interpolate data point y-values when small gaps are present
let innerPadMethod = Padding.InternalPaddingMethod.LinearInterpolation

///take random data point y-values when huge gaps are between data points
let hugeGapPadMethod = Padding.HugeGapPaddingMethod.Random

///padd the start and end of the signal with random data points
let borderPadMethod = Padding.BorderPaddingMethod.Random

///the maximal distance that is allowed between data points is the minimum spacing divided by 2
let minDistance = 
    (Padding.HelperFunctions.getMinimumSpacing data (-)) / 2.

//maximal allowed gap between datapoints where internalPaddingMethod can be applied.
//if internalPaddingMethod = hugeGapPaddingMethod, then it does not matter which value is chosen
let maxSpacing = 10.

//since were dealing with floats the functions are (-) and (+)
let getDiffFu = Padding.HelperFunctions.Float.getDiffFloat      //(-)
let addXValue = Padding.HelperFunctions.Float.addToXValueFloat  //(+)

//number of datapoints the dataset gets expanded to the left and to the rigth
let borderpadding = 1000

//get the paddedDataSet
let paddedData =
    //if a gap is greater than 10. the HugeGapPaddingMethod is applied
    Padding.pad data minDistance maxSpacing getDiffFu addXValue borderpadding borderPadMethod innerPadMethod hugeGapPadMethod

let paddedDataChart=
    let myYAxis() =
            Axis.LinearAxis.init(
                Title="Temperature",Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showline=true)
    let myXAxis() =
            Axis.LinearAxis.init(
                Title="Time",Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showline=true)
    [
    Chart.Line (paddedData,Name="paddedData")
    Chart.Area (data,Name = "rawData")
    ]
    |> Chart.Combine
    |> Chart.withX_Axis (myXAxis())
    |> Chart.withY_Axis (myYAxis())
    |> Chart.withSize(900.,450.)

(*** condition: ipynb ***)
#if IPYNB
paddedDataChart
#endif // IPYNB

(***hide***)
paddedDataChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
Example for a linear interpolation as huge gap padding method
*)

//get the padded data
let paddedDataLinear =
    //if a gap is greater than 10. the LinearInterpolation padding method is applied
    Padding.pad data minDistance maxSpacing getDiffFu addXValue borderpadding borderPadMethod innerPadMethod Padding.HugeGapPaddingMethod.LinearInterpolation

let axis() = Axis.LinearAxis.init(Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showline=true)
let axisWithTitle title= Axis.LinearAxis.init(Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showline=true)

let paddedDataLinearChart=
    [
    Chart.Line (paddedDataLinear,Name="paddedData")
    Chart.Area (data,Name = "rawData")
    ]
    |> Chart.Combine
    |> Chart.withX_Axis (axisWithTitle "Time")
    |> Chart.withY_Axis (axisWithTitle "Temperature")
    |> Chart.withSize(900.,450.)

(*** condition: ipynb ***)
#if IPYNB
paddedDataLinearChart
#endif // IPYNB

(***hide***)
paddedDataLinearChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Wavelet

### Continuous Wavelet

The Continuous Wavelet Transform (CWT) is a multiresolution analysis method to gain insights into frequency components of a signal with simultaneous 
temporal classification. Wavelet in this context stands for small wave and describes a window function which is convoluted with the original signal at 
every position in time. Many wavelets exist, every one of them is useful for a certain application, thereby 'searching' for specific patterns in the data. 
By increasing the dimensions (scale) of the wavelet function, different frequency patterns are studied.

In contrast to the Fourier transform, that gives a perfect frequency resolution but no time resolution, the CWT is capable of mediating between the two opposing 
properties of time resolution and frequency resolution (Heisenberg's uncertainty principle).

For further information please visit [The Wavelet Tutorial](http://web.iitd.ac.in/~sumeet/WaveletTutorial.pdf).
*)

open FSharp.Stats
open StyleParam

///Array containing wavelets of all scales that should be investigated. The propagated frequency corresponds to 4 * Ricker.Scale
let rickerArray = 
    [|2. .. 10.|] |> Array.map (fun x -> Wavelet.createRicker (x**1.8))

///the data already was padded with 1000 additional datapoints in the beginning and end of the data set (see above). 
///Not it is transformed with the previous defined wavelets.
let transformedData = 
    rickerArray
    |> Array.map (fun wavelet -> ContinuousWavelet.transform paddedData (-) 1000 wavelet)

///combining the raw and transformed data in one chart
let combinedChart =
    //CWT-chart
    let heatmap =
        let colNames = 
            transformedData.[0] |> Array.map fst
        transformedData
        |> JaggedArray.map snd
        |> fun x -> Chart.Heatmap(x,ColNames=colNames,Showscale=false)
        |> Chart.withAxisAnchor(X=1)
        |> Chart.withAxisAnchor(Y=1)

    //Rawchart
    let rawChart = 
        Chart.Area (data,Color = "#1f77b4",Name = "rawData")
        |> Chart.withAxisAnchor(X=2)
        |> Chart.withAxisAnchor(Y=2) 

    //combine the charts and add additional styling
    Chart.Combine([heatmap;rawChart])
    |> Chart.withX_AxisStyle("Time",Side=Side.Bottom,Id=2,Showgrid=false)
    |> Chart.withX_AxisStyle("", Side=Side.Top,Showgrid=false, Id=1,Overlaying=AxisAnchorId.X 2)
    |> Chart.withY_AxisStyle("Temperature", MinMax=(-25.,30.), Side=Side.Left,Id=2)
    |> Chart.withY_AxisStyle(
        "Correlation", MinMax=(0.,19.),Showgrid=false, Side=Side.Right,
        Id=1,Overlaying=AxisAnchorId.Y 2)
    |> Chart.withLegend true
    |> Chart.withX_Axis (axis())
    |> Chart.withY_Axis (axis())
    //|> Chart.withSize(900.,700.)
    

(*** condition: ipynb ***)
#if IPYNB
combinedChart
#endif // IPYNB

(***hide***)
combinedChart |> GenericChart.toChartHTML
(***include-it-raw***)


(**

Because in most cases default parameters are sufficient to transform the data, there are two additional functions to process the raw data with automated padding:
 
1. `ContinuousWavelet.transformDefault`
   
 - padding is chosen in an automated manner based on the used wavelet

 - minDistance: median spacing / 2

 - maxDistance: median spacing * 10

 - internalPadding: LinearInterpolation

 - hugeGapPadding: Random

2. `ContinuousWavelet.transformDefaultZero`

 - padding is chosen in an automated manner based on the used wavelet  

 - minDistance: smallest occuring spacing

 - maxDistance: Infinity

 - internalPadding: Zero

 - hugeGapPadding: Zero (redundant)

*)

//used wavelets
let rickerArrayDefault = 
    [|2. .. 2. .. 10.|] |> Array.map (fun x -> Wavelet.createRicker (x**1.8))

//transforms the data with default parameters (InternalPadding=LinearInterpol;HugeGapPadd=Random)
let defaultTransform =
    rickerArrayDefault
    |> Array.map (ContinuousWavelet.transformDefault data)

//alternative presentation of the wavelet correlation coefficients as line charts
let defaultChart =
    let rawDataChart =
        [|Chart.Area(data,Name= "rawData")|]
    let cwtCharts =
        let scale i = rickerArrayDefault.[i].Scale
        defaultTransform 
        |> Array.mapi (fun i x -> Chart.Line(x,Name=(sprintf "s: %.1f" (scale i))))

    Array.append rawDataChart cwtCharts
    |> Chart.Combine
    |> Chart.withY_Axis (axisWithTitle "Temperature and Correlation")
    |> Chart.withX_Axis (axisWithTitle "Time")
    |> Chart.withTitle "default transform"

(*** condition: ipynb ***)
#if IPYNB
defaultChart
#endif // IPYNB

(***hide***)
defaultChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
- Because random y-values are introduced, small wavelets are going to receive a high correlation in big gaps!
- s = scale
- f = frequency [days]
*)
(*** include-value:defaultChart ***)

//transforms the data with default parameters (InternalPadding=Zero;HugeGapPadd=Zero)
let defaultZeroTransform =
    rickerArrayDefault
    |> Array.map (ContinuousWavelet.transformDefaultZero data)

let defaultZeroChart =
    let rawDataChart =
        [|Chart.Area(data,Name= "rawData")|]
    let cwtCharts =
        let scale i = rickerArrayDefault.[i].Scale
        defaultZeroTransform 
        |> Array.mapi (fun i x -> Chart.Line(x,Name=(sprintf "s: %.1f" (scale i) )))

    Array.append rawDataChart cwtCharts
    |> Chart.Combine
    |> Chart.withY_Axis (axisWithTitle "Temperature and Correlation")
    |> Chart.withX_Axis (axisWithTitle "Time")
    |> Chart.withTitle "default Zero transform"


(**
- Because zeros are introduced, the adjacent signals are going to receive a high correlation!
- In this example the correlation coefficients in general drop to a reduced intensity because a zero values are introduced between every data point (minDistance = minimal spacing / 2.). So here a zero padding makes no sense. The Temperature wont drop to zero between two measurements.
- s = scale
- f = frequency [days]
*)

(*** condition: ipynb ***)
#if IPYNB
defaultZeroChart
#endif // IPYNB

(***hide***)
defaultZeroChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

### Continuous Wavelet 3D

When dealing with three dimensional data a three dimensional wavelet has to be used for signal convolution. Here the Marr wavelet (3D mexican hat wavlet) is used for analysis.
Common cases are:

- (microscopic) images

- micro arrays

*)

open FSharp.Stats.Signal

let data2D =
    let rnd = System.Random()
    Array2D.init 50 50 (fun i j -> 
        if (i,j) = (15,15) then 5.
        elif (i,j) = (35,35) then -5.
        else rnd.NextDouble())

let data2DChart = 
    data2D
    |> JaggedArray.ofArray2D
    |> fun data -> Chart.Heatmap(data,Showscale=false)
    |> Chart.withX_AxisStyle "raw data"

//has to be greater than the padding area of the used wavelet
let padding = 11

let paddedData2D =
    //padding the data points with 50 artificial random points on each side
    Padding.Discrete.ThreeDimensional.pad data2D padding Padding.Discrete.ThreeDimensional.Random

let marrWavelet = 
    Wavelet.createMarr 3.

let transformedData2D =
    ContinuousWavelet.Discrete.ThreeDimensional.transform paddedData2D padding marrWavelet

let chartHeatmap =
    transformedData2D
    |> JaggedArray.ofArray2D
    |> fun data -> Chart.Heatmap(data,Showscale=false)
    |> Chart.withX_AxisStyle "wavelet transformed"

let combined2DChart =
    [data2DChart;chartHeatmap]
    |> Chart.Stack 2

(*** condition: ipynb ***)
#if IPYNB
combined2DChart
#endif // IPYNB

(***hide***)
combined2DChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Fast Fourier transform

The FFT analysis converts a signal from its original domain (often time or space) to a representation in the frequency domain and vice versa.

*)

open FSharp.Stats 

// Fast fourier transform

// Sampling frequency   
let fs = 1000  
// Sampling period      
let tp = 1. / float fs
// Length of signal
let l = 1500;            

// Time vector
let time = Array.init (l-1) (fun x -> float x * tp)       

let pi = System.Math.PI

let signal t = 0.7 * sin (2.*pi*50.*t) + sin (2.*pi*120.*t)
let timeSignal = time |> Array.map signal

let fft = 
    Signal.FFT.inverseInPlace (
        timeSignal 
        |> Array.map (fun v ->  Complex.Create (v, 0.) )) 
    |> Array.map (fun c -> c.RealPart)

let fftChart = 
    [
        Chart.Line(time,timeSignal) |> Chart.withTraceName "signal"
        Chart.Line(time,fft) |> Chart.withTraceName "fft"
    ]
    |> Chart.Combine
    |> Chart.withX_Axis (axis())
    |> Chart.withY_Axis (axis())

(*** condition: ipynb ***)
#if IPYNB
fftChart
#endif // IPYNB

(***hide***)
fftChart |> GenericChart.toChartHTML
(***include-it-raw***)