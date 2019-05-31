namespace FSharp.Stats.Signal

open System
open FSharp.Stats
open FSharp.Stats.Signal
open Padding
open Wavelet

///Continuous wavelet transform on non discrete data
module ContinuousWavelet =
    
    module HelperFunctions =

        module Time =
            ///getDiff: calculates the time span between the two events as total minutes (float)
            let getDiffMinutes (a: DateTime) (b: DateTime) =
                a - b
                |> fun x -> x.TotalMinutes 

        module Float =
            ///getDiff: calculates the difference of the two events (-)
            let getDiffFloat (a: float) (b: float) =
                a - b

        module Int =
            ///getDiff: calculates the difference of the two events
            let getDiffInt (a: int) (b: int) =
                (float a) - (float b)

    ///calculates the continuous wavelet transform: 
    ///data: data to transform (x_Value,y_Value) [];
    ///getDiff: get the difference in x_Values as float representation (if 'a is float then (-))
    ///borderpadding: define the number of points padded to the beginning and end of the data (has to be the same as used in padding)
    ///wavelet: used wavelet
    let inline transform (paddedData : ('a * float) []) (getDiff: 'a -> 'a -> float) (borderpadding : int) (wavelet: Wavelet.Ricker) =
        let n = paddedData.Length
        let rickerPadd = wavelet.PaddingArea

        //for every point in the range of the original data perform a convolution with a wavelet and calculate
        //the correlation value at that particular time point
        [|borderpadding .. (n-borderpadding-1)|]
        |> Array.map (fun i -> 
            let (currentX,currentY) = paddedData.[i]
            //calculates the product at x = 0, so the current data point
            let transformAtX = wavelet.RickerFun 0. * currentY
            //calculates sum of products on the right side of the current data point
            let rec rightSide iR acc =
                let (nextRightX,nextRightY) = paddedData.[i+iR]
                let diff = getDiff nextRightX currentX 
                if diff > rickerPadd then
                    acc
                else    
                    rightSide (iR + 1) (acc + ((wavelet.RickerFun diff) * nextRightY))
            //calculates sum of products on the left side of the current data point
            let rec leftSide iL acc = 
                let (nextLeftX,nextLeftY) = paddedData.[i+iL]
                let diff = getDiff currentX nextLeftX
                if diff > rickerPadd then 
                    acc
                else 
                    leftSide (iL - 1) (acc + ((wavelet.RickerFun (- diff)) * nextLeftY))
                
            //correlationvalue as sum of the left and rigth segment, as well as at the data point itself
            let correlationValue = 
                (rightSide 1 0.) + (leftSide -1 0.) + transformAtX

            //adjusts the energy of the wavelet function to a same amount 
            let energycorrection x = x / (Math.Sqrt (Math.Abs(wavelet.Scale)))

            currentX,energycorrection correlationValue 
            )

    let transformDefault (rawData : ('a * float) []) (wavelet: Wavelet.Ricker) =
        let n = rawData.Length
        let spacing =
            [|1 .. n - 1|]
            |> Array.map (fun idx -> fst rawData.[idx] - fst rawData.[idx - 1])
            |> Array.sort
            |> fun intervals -> intervals.[int (n / 2) - 1]
        //the minimalDistance allowed is half the median spacing
        let minDistance = spacing / 2.
        //the maximalDistance allowed is 10 times the median spacing
        let maxDistance = spacing * 10.
        let paddingArea = 
            (wavelet.PaddingArea / minDistance) + 1. 
            |> ceil |> int 

        let paddedData = 
            Padding.padd rawData minDistance maxDistance (-) (+) paddingArea Padding.InternalPaddingMethod.LinearInterpolation Padding.HugeGapPaddingMethod.Random
        transform paddedData (-) paddingArea wavelet

    let transformDefaultZero (rawData : ('a * float) []) (wavelet: Wavelet.Ricker) =
        let n = rawData.Length
        let spacing =
            [|1 .. n - 1|]
            |> Array.map (fun idx -> fst rawData.[idx] - fst rawData.[idx - 1])
            |> Array.sort
        //the minimalDistance allowed is half the minimal overall interval
        let minDistance = spacing.[0]
        //there is no maximal distance in a chromatogram
        let maxDistance = infinity
        let paddingArea = 
            (wavelet.PaddingArea / minDistance) + 1.
            |> ceil |> int 
        let paddedData = 
            Padding.padd rawData minDistance maxDistance (-) (+) paddingArea Padding.InternalPaddingMethod.Zero Padding.HugeGapPaddingMethod.Zero
        transform paddedData (-) paddingArea wavelet


////Example
//open FSharp.Plotly
//open FSharp.Stats.Signal
//open FSharp.Stats.Signal.Padding
//open FSharp.Stats.Signal.Wavelet

////Data
//let rnd = System.Random()
//let data = 
//    Array.init 1000 (fun x -> 1000.* rnd.NextDouble(),rnd.NextDouble()) 
//    |> Array.sortBy fst

////Padding
////let avgSpacing = HelperFunctions.getAvgSpacing data (-)
//let avgSpacing = HelperFunctions.getAvgSpacing data HelperFunctions.Float.getDiffFloat
////interpolate data point y-values when small gaps are present
//let innerPadMethod = Padding.InternalPaddingMethod.LinearInterpolation
////take random data point y-values when huge gaps are between data points
//let hugeGapPadMethod = Padding.HugeGapPaddingMethod.Random
////maximal allowed gap between datapoints where internalPaddingMethod can be applied.
////if internalPaddingMethod = hugeGapPaddingMethod, then it does not matter which value is chosen
//let maxSpacing = 1.
////since were dealing with floats the functions are (-) and (+)
//let getDiffFu = HelperFunctions.Float.getDiffFloat      //(-)
//let addXValue = HelperFunctions.Float.addToXValueFloat  //(+)
////number of datapoints the dataset gets expanded to the left and to the rigth. Minimum=biggestRicker.PaddArea
//let borderpadding = 250
////get the paddedDataSet
//let paddedData =
//    //if a gap is greater than 1000. the InternalPaddingMethod is applied
//    Padding.padd data avgSpacing maxSpacing getDiffFu addXValue borderpadding innerPadMethod hugeGapPadMethod

////Continuopus WaveletTransform
////ricker to use
//let rickerList = 
//    [|2. .. 22.|]
//    |> Array.map (fun x -> Wavelet.createRicker (x**1.1))
////get transformed data based on paddedData
//let transformedData =
//    let transformSingle wavelet = ContinuousWavelet.transform paddedData getDiffFu borderpadding wavelet
//    rickerList
//    |> Array.map (fun ricker -> transformSingle ricker)

////Charting
//let chart =
//    let waveletHeatmap =
//        let colNames = 
//            transformedData.[0]
//            |> Array.map fst
//        transformedData
//        |> JaggedArray.map snd
//        |> fun data' -> Chart.Heatmap(data',ColNames=colNames,Showscale=false) 
//        |> Chart.withTraceName("transformed")
//        |> Chart.withX_AxisStyle("",MinMax=(-250.,1250.))
//        |> Chart.withY_AxisStyle("Scale")
//    let rawChart =
//        data
//        |> Chart.Line
//        |> Chart.withTraceName("rawData")
//        |> Chart.withX_AxisStyle("",MinMax=(-250.,1250.))
//    let paddedChart =
//        paddedData
//        |> Chart.Line
//        |> Chart.withTraceName "paddedData"
//        |> Chart.withX_AxisStyle("",MinMax=(-250.,1250.))
//    [rawChart;paddedChart;waveletHeatmap]
//    |> Chart.Stack 1  
//    |> Chart.withSize(1200.,900.)
//chart
//|> Chart.Show
