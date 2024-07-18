(**
---
title: Optimization
index: 23
category: Documentation
categoryindex: 0
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpAux, 2.0.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: OptimizedPriorityQueue, 5.1.0"
#I "../src/FSharp.Stats/bin/Release/netstandard2.0/"
#r "FSharp.Stats.dll"
#r "nuget: Plotly.NET, 4.0.0"

Plotly.NET.Defaults.DefaultDisplayOptions <-
    Plotly.NET.DisplayOptions.init (PlotlyJSReference = Plotly.NET.PlotlyJSReference.NoReference)


(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: Plotly.NET.Interactive, 4.0.0"
#r "nuget: FSharp.Stats"

open Plotly.NET
#endif // IPYNB


(**

# Optimization

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Optimization.ipynb)
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

_Summary:_ This tutorial teaches how to use optimization methods within FSharp.Stats

## Nelder-Mead

The Nelder-Mead method (also downhill simplex method) can be used to find the minimum or maximum of an objective function.
Please check out Mathias' blog post about the [nelder mead algorithm](https://brandewinder.com/2022/03/31/breaking-down-Nelder-Mead/).

## Quadratic function

Task: Identify the minimum of the following function:

$$f(x)=x^2-0.32x-0.13$$


*)

open FSharp.Stats
open FSharp.Stats.Optimization
open System

open Plotly.NET
open Plotly.NET.TraceObjects


let myFunction (xs: vector) = 
    let x = xs.[0]
    x**2. + 0.32*x + 0.13

// initial guess for the optimization
let x0 = vector [| 0.95|]

// default solver options
let nmc = NelderMead.NmConfig.defaultInit()   

// optimization procedure
let optim = 
    //let stopCrit = 
    //    { OptimizationStop.defaultStopCriteria with MinFunctionEpsilon = 1e-24 }
    //NelderMead.minimizeWithStopCriteria nmc x0 myFunction stopCrit
    NelderMead.minimize nmc x0 myFunction
 
// optimization results as x, y, and z coordinate
let xs,ys =
    optim.Vectors.[0..40] |> Array.map (fun x -> x.[0],myFunction x)
    |> Array.unzip

let optimizationPathchart = 
    [
    Chart.Line(x=xs,y=ys,ShowMarkers=true,Name="Optimization path")
    [-1.  .. 0.005 .. 1.] |> List.map (fun x -> x,myFunction (vector [x])) |> Chart.Line |> Chart.withTraceInfo "myFunction"
    Chart.Point([|optim.SolutionVector.[0],optim.Solution|],Name="Solution") |> Chart.withMarkerStyle(Size=20,Symbol=StyleParam.MarkerSymbol.ArrowUp)
    ]
    |> Chart.combine    
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle ("x",ShowGrid=false) 
    |> Chart.withYAxisStyle ("myFunction(x)",ShowGrid=false) 
    |> Chart.withSize (800.,800.)

(*** condition: ipynb ***)
#if IPYNB
optimizationPathchart
#endif // IPYNB

(***hide***)
optimizationPathchart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
### Rosenbrock

In the following chapter the minium of the three-dimensional rosenbrock function is identified. 

$$f(x,y) = (\alpha - x)^2 + \beta(y-x^2)^2$$
When $\alpha = 1$ and $\beta = 100$ the minimum is at $\alpha^2=1$.

Lets define the function, and a starting coordinate for the optimization task.
*)


// Rosenbrock's valley or Rosenbrock's banana function
let rosenbrock (xs: vector) =
    let x, y = xs.[0], xs.[1]
    pown (1.0 - x) 2 + 100.0 * pown (y - pown x 2) 2

// initial guess for the optimization
let x0_rb = vector [| 1.85; -1.65|]

// rosenbrock visualization
let rosenBrockChart = 
    let range = [-2. .. 0.05 .. 2.] 
    range
    |> List.map (fun y -> 
        range
        |> List.map (fun x -> 
            rosenbrock (vector [x;y])
            )
        )
    |> fun z -> 
        Chart.Surface(zData=z,X=range,Y=range,Opacity = 0.5, Contours = Contours.initXyz (Show = true))

let startConditionsChart = 
    [
    Chart.Point3D([x0_rb.[0],x0_rb.[1],rosenbrock x0_rb])
    rosenBrockChart 
    ]
    |> Chart.combine    
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle ("x", Id = StyleParam.SubPlotId.Scene 1,ShowGrid=false)
    |> Chart.withYAxisStyle ("y", Id = StyleParam.SubPlotId.Scene 1,ShowGrid=false)
    |> Chart.withZAxisStyle ("rosenbrock(x,y)",ShowGrid=false)
    |> Chart.withSize (800.,800.)

(*** condition: ipynb ***)
#if IPYNB
startConditionsChart
#endif // IPYNB

(***hide***)
startConditionsChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

Now the functions minimum should be identified using the Nelder-Mead method. Default solver options are used for optimizations.

*)


// default solver options
let nmc_rb = NelderMead.NmConfig.defaultInit()   

// optimization procedure
let optim_rb = NelderMead.minimize nmc_rb x0_rb rosenbrock 


// minimum x and y value
optim_rb.SolutionVector //vector [|0.9999978246; 1.000002057|]

// minimum z value
optim_rb.Solution //4.110573695e-09

// all z values during optimization steps
optim_rb.Values

// all x and y values during optimization steps
optim_rb.Vectors

(**

#### Plotting of the optimization path

The minimum was correctly identified to be located at $(1,1)$. Lets investigate the path the Nelder-Mead method took to converge to this result.

*)

// optimization results as x, y, and z coordinate
let x3d,y3d,z3d =
    optim_rb.Vectors.[0..60] |> Array.map (fun x -> x.[0],x.[1],rosenbrock x)
    |> Array.unzip3

let optimizationPathChart = 
    [
    Chart.Line3D(x=x3d,y=y3d,z=z3d,ShowMarkers=true)
    rosenBrockChart 
    ]
    |> Chart.combine    
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle ("x", Id = StyleParam.SubPlotId.Scene 1,ShowGrid=false) 
    |> Chart.withYAxisStyle ("y", Id = StyleParam.SubPlotId.Scene 1,ShowGrid=false) 
    |> Chart.withZAxisStyle ("rosenbrock(x,y)",ShowGrid=false)
    |> Chart.withSize (800.,800.)

(*** condition: ipynb ***)
#if IPYNB
optimizationPathChart
#endif // IPYNB

(***hide***)
optimizationPathChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

### Auckley function

The Auckley function has many valleys, with one center and global minimum at $(0,0)$.

*)


// Auckley function
let auckley (xs: vector) =
    let x, y = xs.[0], xs.[1]
    -20.*exp(-0.2*sqrt(0.5*(x**2. + y**2))) - 
    exp(0.5*(cos(2. * Math.PI * x) + cos(2. * Math.PI * y))) + 
    Math.E + 20.

// auckley visualization
let auckleyChart = 
    let range = [-2. .. 0.05 .. 2.] 
    range
    |> List.map (fun y -> 
        range
        |> List.map (fun x -> 
            auckley (vector [x;y])
            )
        )
    |> fun z -> 
        Chart.Surface(zData=z,X=range,Y=range,Opacity = 0.5)


// initial guess for the optimization
let x0_auckley = vector [| 1.2; -1.25 |] 

// default solver options
let nmc_auckley = NelderMead.NmConfig.defaultInit()   

// optimization procedure
let optim_auckley = NelderMead.minimize nmc_auckley x0_auckley auckley


// optimization results as x, y, and z coordinate
let x3d_auc,y3d_auc,z3d_auc =
    optim_auckley.Vectors.[0..44] |> Array.map (fun x -> x.[0],x.[1],auckley x)
    |> Array.unzip3

let optimizationPathChart_auckley = 
    [
    Chart.Line3D(x=x3d_auc,y=y3d_auc,z=z3d_auc,ShowMarkers=true)
    auckleyChart 
    ]
    |> Chart.combine    
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle ("x", Id = StyleParam.SubPlotId.Scene 1,ShowGrid=false) 
    |> Chart.withYAxisStyle ("y", Id = StyleParam.SubPlotId.Scene 1,ShowGrid=false) 
    |> Chart.withZAxisStyle ("auckley(x,y)",ShowGrid=false)
    |> Chart.withSize (800.,800.)

(*** condition: ipynb ***)
#if IPYNB
optimizationPathChart_auckley
#endif // IPYNB

(***hide***)
optimizationPathChart_auckley |> GenericChart.toChartHTML
(***include-it-raw***)


(**
The Nelder-Mead method is able to identiy a local minimum, but misses the global minimum

### Beale function


*)


// Beale function function
let beale (xs: vector) =
    let x, y = xs.[0], xs.[1]
    (1.5 - x + x*y)**2. + 
    (2.25 - x + x*y**2)**2. + 
    (2.625 - x + x*y**3)**2.
   

// Beale visualization
let bealeChart = 
    let range = [-4. .. 0.01 .. 4.] 
    range
    |> List.map (fun y -> 
        range
        |> List.map (fun x -> 
            log10 (beale (vector [x;y]))
            )
        )
    |> fun z -> 
        Chart.Surface(zData=z,X=range,Y=range,Opacity = 0.5)


// initial guess for the optimization
let x0_beale_1 = vector [| -3.5; -3.5 |] 
let x0_beale_2 = vector [|  3. ;  2.5 |] 

// default solver options
let nmc_beale = NelderMead.NmConfig.defaultInit()   

// optimization procedure
let optim_beale_1 = NelderMead.minimize nmc_beale x0_beale_1 beale
let optim_beale_2 = NelderMead.minimize nmc_beale x0_beale_2 beale


// optimization results as x, y, and z coordinate
let x3d_bea_1,y3d_bea_1,z3d_bea_1 =
    optim_beale_1.Vectors.[0..34] |> Array.map (fun x -> x.[0],x.[1],beale x |> log10)
    |> Array.unzip3

let x3d_bea_2,y3d_bea_2,z3d_bea_2 =
    optim_beale_2.Vectors.[0..34] |> Array.map (fun x -> x.[0],x.[1],beale x |> log10)
    |> Array.unzip3

let optimizationPathChart_beale = 
    [
    bealeChart 
    Chart.Line3D(x=x3d_bea_1,y=y3d_bea_1,z=z3d_bea_1,ShowMarkers=true,Name="local minimum")
    Chart.Line3D(x=x3d_bea_2,y=y3d_bea_2,z=z3d_bea_2,ShowMarkers=true,Name="global minimum")
    ]
    |> Chart.combine    
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle ("x", Id = StyleParam.SubPlotId.Scene 1,ShowGrid=false)
    |> Chart.withYAxisStyle ("y", Id = StyleParam.SubPlotId.Scene 1,ShowGrid=false)
    |> Chart.withZAxisStyle ("log10(beale(x,y))",ShowGrid=false)
    |> Chart.withSize (800.,800.)

(*** condition: ipynb ***)
#if IPYNB
optimizationPathChart_beale
#endif // IPYNB

(***hide***)
optimizationPathChart_beale |> GenericChart.toChartHTML
(***include-it-raw***)


(**

Depending on the start conditions, the method yield a wrong, and a correct path to identify the global minimum.

*)
