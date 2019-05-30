namespace FSharp.Stats.Signal

open System
open FSharp.Stats
open FSharp.Stats.Signal

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
    let inline transform (data : ('a * float) []) (getDiff: 'a -> 'a -> float) (borderpadding : int) (wavelet: Wavelet.Ricker) =
        let n = data.Length
        let rickerPadd = wavelet.PaddingArea

        //for every point in the range of the original data perform a convolution with a wavelet and calculate
        //the correlation value at that particular time point
        [|borderpadding .. (n-borderpadding-1)|]
        |> Array.map (fun i -> 
            let (currentX,currentY) = data.[i]
            //calculates the product at x = 0, so the current data point
            let transformAtX = wavelet.RickerFun 0. * currentY
            //calculates sum of products on the right side of the current data point
            let rec rightSide iR acc =
                let (nextRightX,nextRightY) = data.[i+iR]
                let diff = getDiff nextRightX currentX 
                if diff > rickerPadd then
                    acc
                else    
                    rightSide (iR + 1) (acc + ((wavelet.RickerFun diff) * nextRightY))
            //calculates sum of products on the left side of the current data point
            let rec leftSide iL acc = 
                let (nextLeftX,nextLeftY) = data.[i+iL]
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
