namespace FSharp.Stats.Testing 

open System

[<Obsolete("Use Signal.Outliers instead.")>]
module Outliers =
    open FSharp.Stats

    type OutlierBorders =
        {Upper : float;
         Lower : float}

    let createOutlierBorder upper lower = 
        {Upper = upper; Lower = lower}

    let tukey (c:float) (d:float []) =
        let thirdQ = Quantile.compute 0.75 d 
        let firstQ = Quantile.compute 0.25 d
        let iqr = System.Math.Abs (thirdQ - firstQ)
        createOutlierBorder (thirdQ + c * iqr) (firstQ - c * iqr)
        
        
