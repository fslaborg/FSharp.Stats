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

(** 

#Rank

*)