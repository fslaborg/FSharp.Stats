namespace FSharp.Stats.Signal

open FSharp.Stats
open System


module Wavelet =

    ///Ricker, or Mexican hat wavelet
    type Ricker = {
        //the scale of the wavelet
        Scale       : float
        //half of the width of the wavelet
        PaddingArea : float
        //x_value of minimum y_value
        MinimumPosX : float
        //function that takes a x_value and gives the corresponding y_value
        RickerFun   : (float -> float)
        }

    //creation function for Ricker
    let createRicker scale =  
        let rickerFun x = 
            let xx = pown x 2
            let ss = pown scale 2
            let fakA = 2. / (sqrt(3. * scale) * 1.331335364)//(Math.PI**0.25)) pre calculated for efficiency
            let fakB = 1. - (xx/ss)
            let fakC = exp (-xx / (2. * ss))
            fakA * fakB * fakC
        let padArea =   (7. * scale) |> ceil
        let minimumPosX = 1.73205 * scale
        {
        Scale       = scale
        PaddingArea = padArea
        MinimumPosX = minimumPosX
        RickerFun   = rickerFun
        }
