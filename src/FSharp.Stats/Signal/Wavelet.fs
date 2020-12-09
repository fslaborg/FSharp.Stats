namespace FSharp.Stats.Signal

open System
open FSharp.Stats
open FSharpAux

module Wavelet =

    ///Ricker, or Mexican hat wavelet
    type Ricker = {
        ///the scale of the wavelet
        Scale       : float
        ///half of the width of the wavelet
        PaddingArea : float
        ///x value of minimum y value
        MinimumPosX : float
        ///function that takes a x value and gives the corresponding y value
        RickerFun   : (float -> float)
        ///ricker function values for discrete wavelet transform 
        RickerValues: float []
        }

    ///creation function for Ricker
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
        let rickerValues = [|-padArea .. padArea|] |> Array.map rickerFun

        {
        Scale        = scale
        PaddingArea  = padArea
        MinimumPosX  = minimumPosX
        RickerFun    = rickerFun
        RickerValues = rickerValues
        }

    ///3D-wavelet
    type Marr =  {
        ///scale of the wavelet
        Scale           : float  
        ///distance to where the wavelet functions cross the xy-axis-plane
        Radius          : float 
        ///half of the width of the wavelet
        PaddingArea     : float      
        ///function that takes a x- and y-value and gives the corresponding z-value
        MarrValues      : float [,]
                        }
    
    ///creation function for Marr
    let createMarr (radius : float) = 
        let functionMarr x (y : float) s = 
            let squareX = pown x 2
            let squareY = pown y 2
            let squareS = pown s 2
            let facA = 1./(Math.PI * squareS)
            let facB = 1.-(squareX + squareY)/(2. * squareS)
            let facC = exp(-(squareX + squareY)/(2. * squareS))
            facA * facB * facC

        let functionValuesMarr scale xyValues = 
            xyValues
            |> Array.map (fun y -> 
                xyValues
                |> Array.map (fun x -> 
                    functionMarr x y scale
                    )
                )
        let scale = 0.7071 * radius
        let paddingArea = ceil (3. * radius + 2.)
        {
        Scale           = 0.7071 * scale
        Radius          = radius   
        PaddingArea     = paddingArea
        MarrValues         = Array2D.ofJaggedArray (functionValuesMarr scale [|- paddingArea .. paddingArea|])
        }
