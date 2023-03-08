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
#I "../src/FSharp.Stats/bin/Release/netstandard2.0/"
#r "FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-preview.16"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-preview.16"
#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.16"
#r "nuget: FSharp.Stats"
#endif // IPYNB

open Plotly.NET
open Plotly.NET.StyleParam
open Plotly.NET.LayoutObjects

//some axis styling
module Chart = 
    let myAxis name = LinearAxis.init(Title=Title.init name,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,ShowGrid=false,ShowLine=true)
    let myAxisRange name (min,max) = LinearAxis.init(Title=Title.init name,Range=Range.MinMax(min,max),Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,ShowGrid=false,ShowLine=true)
    let withAxisTitles x y chart = 
        chart 
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withXAxis (myAxis x) 
        |> Chart.withYAxis (myAxis y)

(**
# Intervals


[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Intervals.ipynb)

The interval module enables working with closed intervals. A closed interval includes its maximum and minimum.

  - $[1,2], \left\{ x | 1 \le x \le 2 \right\}$ - closed interval; 1 and 2 are _included_
  - $(1,2), \left\{ x | 1 < x < 2 \right\}$ - open interval; 1 and 2 are _excluded_
  - $[1,2), \left\{ x | 1 \le x < 2 \right\}$ - half open interval; 1 is _included_ but 2 is _excluded_


**Interval creation**

*)
open FSharp.Stats

let collection = [3.0; -2.0; 5.0; 1.0; -6.0; 100.0]
let interval = Intervals.ofSeq collection

(*** include-value:interval***)

(**

**Visualization of the interval**

*)

open Plotly.NET

let interval01 = 
    Chart.Point([])
    |> Chart.withShape (Shape.init(ShapeType.Rectangle,Intervals.getStart interval,Intervals.getEnd interval,1,2,Fillcolor=Color.fromHex "#1f77b4"))
    |> Chart.withAxisTitles "" ""
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
let intervalByFst = Intervals.ofSeqBy fst collectionBy
let intervalBySnd = Intervals.ofSeqBy snd collectionBy

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

let i02 = Intervals.create 6.  8.
let i03 = Intervals.create 5. 10.
let addedInterval = Intervals.add i02 i03

(*** hide ***)
let interval02 = 
    let i1 = Shape.init(ShapeType.Rectangle,Intervals.getStart i02,Intervals.getEnd i02,1,2,Fillcolor=Color.fromHex "#1f77b4")
    let i2 = Shape.init(ShapeType.Rectangle,Intervals.getStart i03,Intervals.getEnd i03,3,4,Fillcolor=Color.fromHex "#ff7f0e")
    let re = Shape.init(ShapeType.Rectangle,Intervals.getStart addedInterval,Intervals.getEnd addedInterval,5,6,Fillcolor=Color.fromHex "#2ca02c")
    Chart.Point([])
    |> Chart.withShapes [i1;i2;re]
    |> Chart.withAxisTitles "" ""
    |> Chart.withXAxisStyle ("",MinMax=(0.,20.))
    |> Chart.withYAxisStyle ("",MinMax=(0.,8.))

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

let subInterval = Intervals.subtract i02 i03

(*** hide ***)
let interval03 = 
    let i1 = Shape.init(ShapeType.Rectangle,Intervals.getStart i02,Intervals.getEnd i02,1,2,Fillcolor=Color.fromHex "#1f77b4")
    let i2 = Shape.init(ShapeType.Rectangle,Intervals.getStart i03,Intervals.getEnd i03,3,4,Fillcolor=Color.fromHex "#ff7f0e")
    let re = Shape.init(ShapeType.Rectangle,Intervals.getStart subInterval,Intervals.getEnd subInterval,5,6,Fillcolor=Color.fromHex "#2ca02c")
    Chart.Point([])
    |> Chart.withShapes [i1;i2;re]
    |> Chart.withAxisTitles "" ""
    |> Chart.withXAxisStyle ("",MinMax=(-5.,20.))
    |> Chart.withYAxisStyle ("",MinMax=(0.,8.))

interval03 |> GenericChart.toChartHTML
(***include-it-raw***)


(**

**Interval intersection**

Closed intervals include their margins. If a margin is shared between two intervals, both intervals intersect.

*)

let i04 = Intervals.create 2.  8.
let i05 = Intervals.create 5. 10.
let intInterval = Intervals.intersect i04 i05

(*** hide ***)
let interval04 = 
    let i1 = Shape.init(ShapeType.Rectangle,Intervals.getStart i04,Intervals.getEnd i04,1,2,Fillcolor=Color.fromHex "#1f77b4")
    let i2 = Shape.init(ShapeType.Rectangle,Intervals.getStart i05,Intervals.getEnd i05,3,4,Fillcolor=Color.fromHex "#ff7f0e")
    let re = Shape.init(ShapeType.Rectangle,Intervals.getStart intInterval.Value,Intervals.getEnd intInterval.Value,5,6,Fillcolor=Color.fromHex "#2ca02c")
    Chart.Point([])
    |> Chart.withShapes [i1;i2;re]
    |> Chart.withAxisTitles "" ""
    |> Chart.withXAxisStyle ("",MinMax=(0.,12.))
    |> Chart.withYAxisStyle ("",MinMax=(0.,8.))

interval04 |> GenericChart.toChartHTML
(***include-it-raw***)
