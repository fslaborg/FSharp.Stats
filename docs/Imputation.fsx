(**
---
title: Imputation
index: 12
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

#Imputation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Imputation.ipynb)

Short documentation how to impute values

*)
open FSharp.Stats
open FSharp.Stats.ML


let a = [3.;2.;3.;4.;5.;]
let b = [1.;2.;3.;nan;5.;]
let c = [nan;2.;3.;4.;nan;]
let d = [5.;2.;6.;4.;5.;]
let e = [0.5;2.;3.;5.;5.;]

let data = [a;b;c;d;e]

(*** hide ***)
let missingDataMatrix = "rawData\r\n" + FSharp.Stats.FSIPrinters.matrix (matrix data)
(*** include-value:missingDataMatrix ***)

(**
<a name="k-Nearest imputation"></a>

##k-Nearest imputation

Missing data imputation based on the k-nearest neighbour algorithm:

*)

// init kNearest MatrixBaseImpute
let kn : Impute.MatrixBaseImputation<float[],float> = Impute.kNearestImpute 2
let imputedData = Impute.imputeBy kn Ops.isNan data

(*** hide ***)
let imputedDataMatrix = "k nearest neighbours imputed data\r\n" + FSharp.Stats.FSIPrinters.matrix (matrix imputedData)
(*** include-value:imputedDataMatrix ***)

(**

<a name="random imputation"></a>

##random imputation

...
*)

// init random VectorBaseImpute
let rnd = Impute.rnd (System.Random())

let rndRowWise = Impute.imputeRowWiseBy rnd Ops.isNan data
let rndColWise = Impute.imputeColWiseBy rnd Ops.isNan data

(*** hide ***)
let rndRowDataMatrix = "rndRowDataMatrix imputed data\r\n" + FSharp.Stats.FSIPrinters.matrix (matrix rndRowWise)
let rndColDataMatrix = "rndColDataMatrix imputed data\r\n" + FSharp.Stats.FSIPrinters.matrix (matrix rndColWise)
(*** include-value:rndRowDataMatrix ***)
(*** include-value:rndColDataMatrix ***)

(**

<a name="normal imputation"></a>

##normal imputation

...

*)

let normalRowWise = Impute.imputeRowWiseBy Impute.normal Ops.isNan data
let normalColWise = Impute.imputeColWiseBy Impute.normal Ops.isNan data


(*** hide ***)
let normalRowDataMatrix = "normalRowDataMatrix imputed data\r\n" + FSharp.Stats.FSIPrinters.matrix (matrix normalRowWise)
let normalColDataMatrix = "normalColDataMatrix imputed data\r\n" + FSharp.Stats.FSIPrinters.matrix (matrix normalColWise)
(*** include-value:normalRowDataMatrix ***)
(*** include-value:normalColDataMatrix ***)

