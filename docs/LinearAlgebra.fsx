(**
---
title: Linear Algebra
index: 4
category: Documentation
categoryindex: 0
---
*)

(*** hide ***)

(*** condition: prepare ***)
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
# Linear Algebra

Some algorithms such as SVD, EVD, or QR are implemented as a managed version in F# for a full list check the [API reference](https://fslab.org/FSharp.Stats/reference/fsharp-stats-algebra.html)
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)
*)

open FSharp.Stats
open FSharp.Stats.Algebra

let A = 
    matrix [ [ 1.0;  1.0; -1.0 ]
             [ 1.0; -2.0; -3.0 ]
             [ 2.0;  3.0;  1.0 ] ]

let B = 
    matrix [ [  4.0; ]
             [ -6.0; ]
             [  7.0; ] ]

let svdRes = LinearAlgebra.SVD A

(***include-value:svdRes***)

(**

## Using unmanaged optimized linear algebra functions

Additionally, we provide some bindings for [LAPACK]() routines. This is currently only tested on windows.

**Attention**: These bindings are highly incomplete and will most likely be dropped for something like MKL.NET. [See issue#](https://github.com/fslaborg/FSharp.Stats/issues/91)

the native libraries are contained in the nuget package at the `netlib_LAPACK` path. Include that one instead of the `/../../lib` pth used here.

*)

ServiceLocator.setEnvironmentPathVariable (__SOURCE_DIRECTORY__ + "/../../lib") //"D:/Source/FSharp.Stats/lib"

// initialize the native service provider. This will search on many system paths for the needed binaries.
LinearAlgebra.Service()

let svdResLapack = LinearAlgebra.SVD A


