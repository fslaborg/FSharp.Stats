(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "netstandard.dll"
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"

open FSharp.Plotly
open FSharp.Collections
(**

#Signal


<a name="Padding"></a>

## Padding

If convolution operations should be performed on a signal trace, it often is necessary to extend (pad) the data with artificial data points.
There are several padding methods:

 - **Zero**: Data points with y-value=zero are introduced. This often is useful when analyzing spectra with sparse data because areas without any data measured are assumed to have zero intensity.

 - **Random**: When the baseline of the measured signal is nonzero like in chromatogramms, it is necessary to insert data points with random y-values taken from the original data set.

 - **Delete**: No datapoints are inserted.

 - **Linear interpolation**: When a linear relationship is assumed in the range betwen two adjacent data points, the padding points should lie on the straight line between those points.


**Three regions can be defined where padding points could be introduced:**

 1. In the beginning and end of the data set artificial data points have to be added to analyse the start- and end-regions of the data. 
 
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

///the maximal distance that is allowed between data points is the average spacing
let minDistance = 
    Padding.HelperFunctions.getMedianSpacing data (-)

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
    //if a gap is greater than 1000. the InternalPaddingMethod is applied
    Padding.pad data minDistance maxSpacing getDiffFu addXValue borderpadding innerPadMethod hugeGapPadMethod


let paddedDataChart=
    let myYAxis() =
            Axis.LinearAxis.init(
                Title="Temperature",Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showline=true)
    let myXAxis() =
            Axis.LinearAxis.init(
                Title="Time",Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showline=true)
    [
    Chart.Point (paddedData,Name="paddedData")
    Chart.Point (data,Name = "rawData") |> Chart.withMarkerStyle(4)
    ]
    |> Chart.Combine
    |> Chart.withX_Axis (myXAxis())
    |> Chart.withY_Axis (myYAxis())
    |> Chart.withSize(900.,450.)

(*** include-value:paddedDataChart ***)


(**

<a name="Wavelet"></a>

#Wavelet

<a name="ContinuousWavelet"></a>

##Continuous Wavelet

<a name="FFT"></a>

## Fast Fourier transform

The FFT analysis converts a signal from its original domain (often time or space) to a representation in the frequency domain and vice versa.

*)

//open FSharp.Stats 

//// Fast fourier transform

//// Sampling frequency   
//let fs = 1000  
//// Sampling period      
//let tp = 1. / float fs
//// Length of signal
//let l = 1500;            

//// Time vector
//let t = Array.init (l-1) (fun x -> float x * tp)       

//let pi = System.Math.PI

//let signal t = 0.7 * sin (2.*pi*50.*t) + sin (2.*pi*120.*t)
//let y = t |> Array.map signal

//let y' = 
//    Signal.FFT.inverseInPlace (
//        y 
//        |> Array.map (fun v ->  Complex.Create (v, 0.) )) 
//    |> Array.map (fun c -> c.RealPart)


//[
//    Chart.Line(t,y)
//    Chart.Line(t,y')
//]
//|> Chart.Combine
//|> Chart.Show


//Chart.Line(t,y') |> Chart.Show

//let x = [|0. .. 0.1 .. 10.|]
//let f t = 5. + 2. * cos (2.*pi*t-90.) + 3.* cos (4.*pi*t)
//let y = x |> Array.map fA
//let y' = Signal.FFT.inverseInPlace (
//    y 
//    |> Array.map (fun v ->  Complex.Create (v, 0.) )) 
//    |> Array.map (fun c -> c.RealPart)

//Chart.Line(x,x |> Array.map f) |> Chart.Show

//Chart.Line(x,y') |> Chart.Show


(**
<a name="Baseline"></a>

##Baseline

<a name="Filtering"></a>

##Filtering


Savitzgy-Golay description is coming soon.

*)




open FSharp.Stats

// Savitzky-Golay low-pass filtering
let t  = [|-4. ..(8.0/500.).. 4.|]
let dy  = t |> Array.map (fun t -> (-t**2.) + (Distributions.Continuous.Normal.Sample 0. 0.5) )
let dy' = t |> Array.map (fun t -> (-t**2.))

let dysg = Signal.Filtering.savitzky_golay  31 4 0 1 dy

let savitzgyChart =
    [
        Chart.Point(t, dy, Name="data with noise");
        Chart.Point(t, dy', Name="data without noise");
        Chart.Point(t, dysg, Name="data sg");
    ]
    |> Chart.Combine

(*** include-value:savitzgyChart***)






