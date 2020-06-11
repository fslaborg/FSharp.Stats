(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "netstandard.dll"
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
open System
open FSharp.Plotly
open FSharp.Plotly.Axis
open FSharp.Plotly.StyleParam

let myAxis title = LinearAxis.init(Title=title,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)
let myAxisRange title range = LinearAxis.init(Title=title,Range=Range.MinMax range,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)
let styleChart x y chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxis y)

(**

#Growth curve

##Modelling

For growth curve analysis the cell count data must be considered in log space. The exponential phase (log phase) then becomes linear.
After modelling, all growth parameters (maximal cell count, lag phase duration, and growth rate) can be derived from the model, so there is no
need for manual labelling of the growth phases.

</br>

![Data model](img/growthCurve.png)

</br>

Several growth curve models have been proposed. The classic Verhulst logistic regression is covered in the 'Fitting' documentation.
In the following example the four parameter gompertz function is applied to the growth 
data (Gibson et al., 1988).

*)

open FSharp.Stats
open FSharp.Stats.Fitting.NonLinearRegression

let time = [|0. .. 0.5 .. 8.|]

let cellCount =
    [|
    17000000.;16500000.;11000000.;14000000.;
    27000000.;40000000.;120000000.;300000000.;
    450000000.;1200000000.;2700000000.;
    5000000000.;8000000000.;11700000000.;
    12000000000.;13000000000.;12800000000.
    |]

let cellCountLn = cellCount |> Array.map log


(*** hide ***)

let chartOrig = 
    Chart.Point(time,cellCount)
    |> Chart.withTraceName "original data"
    |> styleChart "" "cells/ml"

let chartLog =
    Chart.Point(time,cellCountLn)
    |> Chart.withTraceName "log count"
    |> styleChart "time (h)" "ln(cells/ml)"
    
let growthChart = 
    [chartOrig;chartLog]|> Chart.Stack 1

(*** include-value:growthChart ***)

(**

The gompertz model is fitted using the Levenberg Marquardt solver. Initial parameters are guessed from the original data.
The expected generation time has to be approximated as initial guess. For parameter interpretation the applied log transform is important.
The log function used to transform the count data has to be provided.

Gompertz parameters:

  - A: lower asymptote

  - B: relative growth rate (approximated by generation time consideration)

  - C: upper asymtote - lower asymptote

  - M: time point of inflection (maximal growth rate)


*)

// The Levenberg Marquardt algorithm identifies the parameters that leads to the best fit 
// of the gompertz models to the count data. The solver must be provided with initial paramters
// that are estimated in the following:
let solverOptions (xData :float []) (yDataLog :float []) expectedGenerationTime (usedLogTransform: float -> float) =
    // lower asymptote
    let a = Seq.min yDataLog
    // upper asymptote - lower asymptote (y range)
    let c = (Seq.max yDataLog) - a
    // relative growth rate
    let b = usedLogTransform 2. * Math.E / (expectedGenerationTime * c)
    // time point of inflection (in gompertz model at f(x)=36% of the y range)
    let m = 
        let yAtInflection = a + c * 0.36
        Seq.zip xData yDataLog
        |> Seq.minBy (fun (xValue,yValue) ->
            Math.Abs (yValue - yAtInflection)
        )
        |> fst
    createSolverOption 0.001 0.001 10000 [|a;b;c;m|]

// By solving the nonlinear fitting problem, the optimal model parameters are determined
let gompertzParams =
    LevenbergMarquardt.estimatedParams
        Table.gompertz
        (solverOptions time cellCountLn 1. log)
        0.1
        10.
        time
        cellCountLn

let fittingFunction = Table.gompertz.GetFunctionValue gompertzParams

(*** hide ***)
let chartLogR =
    Chart.Point(time,cellCountLn)
    |> Chart.withTraceName "log count"
    
let fittedValues =
    [time.[0] .. 0.1 .. Seq.last time]
    |> Seq.map (fun x -> 
        x,fittingFunction x
        ) 
    |> Chart.Line

let fittedChart = 
    [chartLogR;fittedValues]
    |> Chart.Combine
    |> styleChart "time (h)" "ln(cells/ml)"

(*** include-value:fittedChart ***)

(**

##Generation time calculation

The generation time can be calculated by dividing log(2) by the slope of the inflection point. The used log transform must
match the used log transform applied to the count data.

The four parameter Gompertz model allows the determination of generation times from its parameters (Gibson et al., 1988).

*)


let generationtime (parametervector:vector) (logTransform:float -> float) =
    logTransform 2. * Math.E / (parametervector.[1] * parametervector.[2])

let lag (parametervector:vector) =
    (parametervector.[3] - 1.) / parametervector.[1]

let g = sprintf "The generation time is: %.1f min" (60. * (generationtime gompertzParams log))
let l = sprintf "The lag phase duration is %.2f h" (lag gompertzParams)


(*** include-value:g ***)
(*** include-value:l ***)
