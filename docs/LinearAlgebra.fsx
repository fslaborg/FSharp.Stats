(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta3"
#r "nuget: Plotly.NET.Interactive, 2.0.0-alpha5"
#r "nuget: FSharp.Stats"
#endif // IPYNB

open Plotly.NET
open Plotly.NET.Axis
open Plotly.NET.StyleParam

(**
# Linear Algebra
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

(**

## Using unmanaged optimized linear algebra functions

**Attention**: These bindings are highly incomplete and will most likely be dropped for something like MKL.NET. [See issue#](https://github.com/fslaborg/FSharp.Stats/issues/91)

the native libraries are contained in the nuget package at the `netlib_LAPACK` path. Include that one instead of the `/../../lib` pth used here.

*)

ServiceLocator.setEnvironmentPathVariable (__SOURCE_DIRECTORY__ + "/../../lib") //"D:/Source/FSharp.Stats/lib"

LinearAlgebra.Service()

let svdRes = LinearAlgebra.SVD A
(***include-value:svdRes***)