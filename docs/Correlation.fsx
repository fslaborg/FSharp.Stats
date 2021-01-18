(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta3"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta3"
#r "nuget: FSharp.Stats"
#endif // IPYNB

open Plotly.NET
open Plotly.NET.Axis
open Plotly.NET.StyleParam

let myAxis title = LinearAxis.init(Title=title,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
let myAxisRange title range = LinearAxis.init(Title=title,Range=Range.MinMax range,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
let styleChart x y chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxis y)
let styleChartRange x y rx ry chart = chart |> Chart.withX_Axis (myAxisRange x rx) |> Chart.withY_Axis (myAxisRange y ry)

(** 

#Correlation

*)

open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Distributions.Continuous
open FSharp.Stats.Correlation


let lags = [0..100]
let x = [0. .. 100.]

//// Autocorrelation of a gaussian signal
let gaussPDF = Normal.PDF 10. 2.
let yGauss = x |> List.map gaussPDF |> vector



let autoCorrGauss = lags |> List.map (fun lag -> autoCorrelation lag yGauss)



(*** hide ***)



let gaussAC =
    Chart.Point(lags,autoCorrGauss)
    |> Chart.withTraceName "Autocorrelation"
    |> Chart.withTitle "Autocorrelation of a gaussian sine wave"
    |> fun c -> Chart.Stack 1 [Chart.Point(x,yGauss,Name="gaussian");c]
    
(***hide***)
gaussAC |> GenericChart.toChartHTML
(***include-it-raw***)
