(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/net47"
#r @"FSharp.Stats.dll"
#r "netstandard.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"

open FSharp.Plotly
open FSharp.Plotly.Axis
open FSharp.Plotly.StyleParam

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
open FSharp.Plotly
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
    
(*** include-value:gaussAC ***)

