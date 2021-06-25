(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta8"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta8"
#r "nuget: FSharp.Stats"
#endif // IPYNB

(**

# Hyper Parameter Tuning

*)

open Plotly.NET
open FSharp.Stats
open FSharp.Stats.HyperParameterTuning
open FSharp.Stats.Optimization
let f _ [p1] = (), -(abs (HyperParameterValue.GetFloat p1))

let hps = [HyperParameter.Create (-2.,2.)]

GridSearch.gridSearchMaximize 100 f () hps
RandomSearch.randomSearchMaximize 100 f () hps

let surrogateF = BayesianOptimization.Surrogates.gaussianProcessSurrogate
let selectorF = BayesianOptimization.Selectors.expectedImprovementSelector
#time
BayesianOptimization.bayesianOptimizationMaximize 20 surrogateF selectorF f () hps
1+1

let eps =  System.Double.Epsilon ** (1.0 / 50.0)

let vectorF (vec : vector) = abs vec.[0] + (0.5 + vec.[1]) ** 2.

let xs = (vector [10.;5.])
//let vectorF vec = f () (vec |> Seq.map (fun v -> HyperParameterValue.Float v) |> Seq.toList) |> snd

let derivative = GradientDescent.grad vectorF
GradientDescent.minimize vectorF derivative xs

let heatmap2d f = 
    let x = List.init 50 (fun x -> (float x - 25.) / 10.)
    let y = List.init 50 (fun x -> (float x - 25.) / 10.)
    let z = 
        x
        |> List.map (fun i ->
            y
            |> List.map (fun j ->
                   f (vector [i;j])
            )
        )
    Chart.Heatmap(z,x,y)

