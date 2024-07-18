(**
---
title: Intervals
index: 21
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
# Intervals


[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Intervals.ipynb)
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

The interval module enables working with closed intervals. A closed interval includes its maximum and minimum.

  - $[1,2], \left\{ x | 1 \le x \le 2 \right\}$ - closed interval; 1 and 2 are _included_
  - $(1,2), \left\{ x | 1 < x < 2 \right\}$ - open interval; 1 and 2 are _excluded_
  - $[1,2), \left\{ x | 1 \le x < 2 \right\}$ - right open interval; 1 is _included_ but 2 is _excluded_
  - $(1,2], \left\{ x | 1 < x \le 2 \right\}$ - left open interval; 1 is _excluded_ but 2 is _included_


**Interval creation**

*)
open FSharp.Stats
open Plotly.NET

let myInterval = Interval.CreateLeftOpen<float> (-3.,2.)

let loi = sprintf "myInterval is: %s" (myInterval.ToString())

(*** include-value:loi***)

(**
When intervals are created from sequences, always closed intervals are generated!
*)
let collection = [3.0; -2.0; 5.0; 1.0; -6.0; 100.0]
let interval = Interval.ofSeq collection

(*** include-value:interval***)

(**

**Visualization of the interval**

*)

open Plotly.NET
open Plotly.NET.LayoutObjects
open Plotly.NET.StyleParam

let interval01 = 
    Chart.Point([])
    |> Chart.withShape (Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart interval,X1=Interval.getEnd interval,Y0=1,Y1=2,FillColor=Color.fromHex "#1f77b4"))
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle ("",MinMax=(-10.,120.))
    |> Chart.withYAxisStyle ("",MinMax=(0.,5.))

(*** condition: ipynb ***)
#if IPYNB
interval01
#endif // IPYNB

(***hide***)
interval01 |> GenericChart.toChartHTML
(***include-it-raw***)

(**

*)

let collectionBy = [("a",3.0); ("b",-2.0); ("c",5.0); ("d",1.0); ("e",-6.0); ("f",100.0)]
let intervalByFst = Interval.ofSeqBy fst collectionBy
let intervalBySnd = Interval.ofSeqBy snd collectionBy

(*** include-value:intervalByFst***)
(*** include-value:intervalBySnd***)

(**

**Interval addition**

Intervals can be added (interval arithmetic definition) by adding the minima and maxima to obtain a new interval

```
i=[a,b]
j=[c,d]

i + j = [a+b,c+d]
```

*)

let i02 = Interval.CreateClosed<float> (6.,8.)
let i03 = Interval.CreateClosed<float> (5.,10.)
let addedInterval = Interval.add i02 i03

(*** hide ***)
let interval02 = 
    let i1 = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart i02,X1=Interval.getEnd i02,Y0=1,Y1=2,FillColor=Color.fromHex "#1f77b4")
    let i2 = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart i03,X1=Interval.getEnd i03,Y0=3,Y1=4,FillColor=Color.fromHex "#ff7f0e")
    let re = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart addedInterval,X1=Interval.getEnd addedInterval,Y0=5,Y1=6,FillColor=Color.fromHex "#2ca02c")
    Chart.Point([])
    |> Chart.withShapes [i1;i2;re]
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle (MinMax=(0.,20.))
    |> Chart.withYAxisStyle (MinMax=(0.,8.))

interval02 |> GenericChart.toChartHTML
(***include-it-raw***)

(**

**Interval subtraction**

Intervals can be subtracted (interval arithmetic definition) by:

```
i=[a,b]
j=[c,d]

i - j = [a-d,b-c]
```

*)

let subInterval = Interval.subtract i02 i03

(*** hide ***)
let interval03 = 
    let i1 = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart i02,X1=Interval.getEnd i02,Y0=1,Y1=2,FillColor=Color.fromHex "#1f77b4")
    let i2 = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart i03,X1=Interval.getEnd i03,Y0=3,Y1=4,FillColor=Color.fromHex "#ff7f0e")
    let re = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart subInterval,X1=Interval.getEnd subInterval,Y0=5,Y1=6,FillColor=Color.fromHex "#2ca02c")
    Chart.Point([])
    |> Chart.withShapes [i1;i2;re]
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withXAxisStyle (MinMax=(-5.,20.))
    |> Chart.withYAxisStyle (MinMax=(0.,8.))

interval03 |> GenericChart.toChartHTML
(***include-it-raw***)


(**

**Interval intersection**

Closed intervals include their margins. If a margin is shared between two intervals, both intervals intersect.

*)

let i04 = Interval.CreateClosed<float> (2.,8.)
let i05 = Interval.CreateClosed<float> (5.,10.)
let intInterval = Interval.intersect i04 i05

(*** hide ***)
let interval04 = 
    let i1 = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart i04,X1=Interval.getEnd i04,Y0=1,Y1=2,FillColor=Color.fromHex "#1f77b4")
    let i2 = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart i05,X1=Interval.getEnd i05,Y0=3,Y1=4,FillColor=Color.fromHex "#ff7f0e")
    let re = Shape.init(ShapeType=ShapeType.Rectangle,X0=Interval.getStart intInterval,X1=Interval.getEnd intInterval,Y0=5,Y1=6,FillColor=Color.fromHex "#2ca02c")
    Chart.Point([])
    |> Chart.withShapes [i1;i2;re]
    |> Chart.withTemplate ChartTemplates.lightMirrored

    |> Chart.withXAxisStyle (MinMax=(0.,12.))
    |> Chart.withYAxisStyle (MinMax=(0.,8.))

interval04 |> GenericChart.toChartHTML
(***include-it-raw***)
