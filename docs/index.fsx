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
    let withAxisTitles x y chart = 
        chart 
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withXAxis (myAxis x) 
        |> Chart.withYAxis (myAxis y)

(**
# FSharp.Stats

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=index.ipynb)

FSharp.Stats is a multipurpose project for statistical testing, linear algebra, machine learning, fitting and signal processing.

## Installation

**From Nuget.org:**

You can get all FSharp.Stats packages from nuget at https://www.nuget.org/packages/FSharp.Stats/.

**To build the binaries yourself:**

**Windows**:

- Install [.Net Core SDK](https://www.microsoft.com/net/download)
- navigate to project folder
- use the console command `./build.cmd`

**Linux(Ubuntu, using Mono)**:

- Install [.Net Core SDK](https://www.microsoft.com/net/download/linux-package-manager/ubuntu14-04/sdk-current)
- navigate to project folder
- make the script executable with `chmod +x ./build.sh`
- use the console command `./build.sh`

**Documentation**:

- While editing the documentation you can preview the documentation in your browser via `dotnet fsdocs watch --eval`

---

## Example

The following examples show how easy it is to start working with FSharp.Stats.

*)

(**
### Distributions
*)

open FSharp.Stats

// initialize a normal distribution with mean 25 and standard deviation 0.1
let normalDistribution = Distributions.Continuous.normal 25. 0.1

// draw independently 30 times from the given distribution 
let sample = Array.init 30 (fun _ -> normalDistribution.Sample())

(*** include-value:sample ***)

(**
### Basic descriptive statistics
*)

// calculate the mean of the given sample
let mean = Seq.mean sample

(*** include-value:mean ***)

// calculate the bessel corrected sample standard deviation of the given sample
let stDev = Seq.stDev sample

(*** include-value:stDev ***)

// calculate the coefficient of variation of the given sample 
// Attention: CV is valid only if a hypothetical real zero value exists for the data.
let cv = Seq.cv sample

(*** include-value:cv ***)


(**
### Vectors, Matrices and linear algebra
*)

// create a vector 
let vecB = vector [19.;11.;35.]

// create a matrix 
let matA = matrix [[3.;4.;0.];[1.;2.;2.];[5.;0.;5.]]

// solve the linear system of equations
let vecX = FSharp.Stats.Algebra.LinearAlgebra.SolveLinearSystem matA vecB

(*** include-value:vecX ***)

(**
### Interpolation
*)

let xData = vector [|1. .. 10.|]
let yData = vector [|4.;7.;9.;12.;15.;17.;16.;23.;5.;30.|]

// get coefficients of interpolating polynomial
let interpolatingCoefficients = 
    Interpolation.Polynomial.coefficients xData yData

// get fitting function of interpolating polynomial
let interpolFitFunc = 
    Interpolation.Polynomial.fit interpolatingCoefficients

(*** hide ***)
// create line chart of interpolating polynomial
let interpolChart = 
    [1. .. 0.1 .. 10.] 
    |> List.map (fun x -> x,interpolFitFunc x)
    |> fun data -> Chart.Line(data,Name="interpol polynomial")

(**
### Regression
*)

// get coefficients of 3rd order regression polynomial
let regressionCoefficients = 
    Fitting.LinearRegression.OrdinaryLeastSquares.Polynomial.coefficient 3 xData yData
    
// get fitting function of 3rd order regression polynomial
let regressionFitFunc = 
    Fitting.LinearRegression.OrdinaryLeastSquares.Polynomial.fit 3 regressionCoefficients

(*** hide ***)
// create line chart of regression polynomial
let regressionChart = 
    [1. .. 0.1 .. 10.] 
    |> List.map (fun x -> x,regressionFitFunc x)
    |> fun data -> Chart.Line(data,Name="regression polynomial")

let combinedChart =
    let rawChart = Chart.Point(xData,yData)
    [rawChart;interpolChart;regressionChart]
    |> Chart.combine
    |> Chart.withAxisTitles "" ""
(**
The resulting interpolating and regression polynomials are plotted below using [Plotly.NET](https://github.com/plotly/Plotly.NET).

*)   
(***hide***)
combinedChart |> GenericChart.toChartHTML
(***include-it-raw***)
(**


Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fslaborg/FSharp.Stats/tree/developer/docs
  [gh]: https://github.com/fslaborg/FSharp.Stats
  [issues]: https://github.com/fslaborg/FSharp.Stats/issues
  [readme]: https://github.com/fslaborg/FSharp.Stats/blob/developer/README.md
  [license]: https://github.com/fslaborg/FSharp.Stats/blob/developer/LICENSE

*)
