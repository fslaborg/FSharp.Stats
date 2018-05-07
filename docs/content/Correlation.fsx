(** 

#Correlation

*)
#I "../../bin/FSharp.Stats/net461"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
#r "FSharp.Stats.dll"

open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Distributions.Continuous
open FSharp.Plotly
open FSharp.Stats.Correlation


let lags = [0..100]
let x = [0. .. 100.]

//// Autocorrelation of a gaussian signal
let gaussPDF = Normal.PDF 10. 2.
let yGauss = x |> List.map gaussPDF

Chart.Point(x,yGauss)
|> Chart.Show

let autoCorrGauss = lags |> List.map (fun lag -> acfOf lag yGauss)


//// Autocorrelation of a gaussian sine wave


Chart.Point(lags,autoCorrGauss)
|> Chart.Show

let 
    


