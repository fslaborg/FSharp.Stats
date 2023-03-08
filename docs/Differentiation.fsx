(**
---
title: Differentiation
index: 15
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
# Numerical Differentiation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?urlpath=/tree/home/jovyan/Integration.ipynb)

Numerical differentiation is used to estimate the derivative of a mathematical function using values of the function and perhaps other knowledge about the function.

### Three-Point Differentiation
FSharp.Stats implements a three point differentiation method. This method takes a set of values and their function values. For a given value xT of the set, one defines three other points which should be considered to calculate the differential at xT.
Here follows a small snippet.

First, we create our data. In this case our function is f(x) = x ^ 3.
*)
open FSharp.Stats

// x data
let xs = Array.init 100 (fun x -> float x / 8.)
// y data
let ys = Array.map (fun x -> x ** 3.) xs

(**
Now we apply the threePointDifferentiation to every point starting with the second and ending with the second last. We can't do it for every point because for every point we need to have three other points in close proximity.
*)

let y's = 
    [|
    for i = 1 to xs.Length - 2 do
        yield xs.[i],Integration.Differentiation.differentiateThreePoint xs ys i (i-1) (i) (i+1)
    |]

(**
We compare the resulting values with the values of the known differential f'(x) = 3x^2, here called g(x)
*)

open Plotly.NET

let comparisonChart = 
    [
    Chart.Point(xs,ys,Name = "f(x)")
    Chart.Point(y's,Name = "f'(x)")
    Chart.Point(Array.map (fun x -> x,(x ** 2.) * 3.) xs,Name = "g(x)")
    ]
    |> Chart.combine
    |> Chart.withAxisTitles "" ""

(*** condition: ipynb ***)
#if IPYNB
comparisonChart
#endif // IPYNB

(***hide***)
comparisonChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
### Two-Point Differentiation

To calculate the approximation for the derivative, a Two-Point Differentiation calculates the difference of f(x) at x and f(x) at x+h and correlates it to h. 
This will give better approximations the smaller h is. 
The function uses two different mathematical approaches to decrease the error, one for h > 1. and one for h < 1.

</br>

![Data model](img/Derivative.svg.png)

</br>
*)

open FSharp.Stats.Integration.Differentiation.TwoPointDifferentiation
open FSharp.Stats.Integration.Differentiation

let testFunction x = x**2. 

let test1 = differentiate 0.5 testFunction 2.
//Result for test1 is: 4.00

let test2 = differentiate 3. testFunction 2.
//Result for test2 is: 7.00

let test3 = differentiate 0.1 testFunction 2.
//Result for test3 is: 4.00

(**
- The correct result for test1 (f(x) = x**2.) is assumed to be 4.
- With a higher h (for test2 h = 3., compared to the 0.5 used in test1) the approximation error increases.
*)

(**
You can try and find an optimal h - value with the "differentiateOptimalHBy" - function. 
This function uses the first h-value it assumes to give good results for the numerical differentiation calculation.
Due to this, possible error due to float precision is avoided.
In the following example this is not really necessary, as values are quite big.
*)
let hArray = [|0.1 .. 0.1 .. 2.|]

let test4 = differentiateOptimalHBy hArray testFunction 2.
//Result for test4 is: 4.00

(**
If you want to use a presuggested hArray then you can use the "differentiateOptimalH" function.
This function uses an array from 0.01 to 5e^-100 in [|0.01; 0.005; 0.001; 0.0005; 0.0001 ..|]-increments as hArray.
*)

let test5 = differentiateOptimalH testFunction 2. 
//Result for test5 is: 4.00
