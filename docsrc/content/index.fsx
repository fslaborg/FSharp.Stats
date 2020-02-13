(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "netstandard.dll"
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/Fsharp.Plotly.dll"

(**
FSharp.Stats
======================

FSharp.Stats is supposed to be a multipurpose project for statistical testing, linear algebra, machine learning, optimization, fitting and signal processing.

Installation
------------

FSharp.Stats is currently on the way to its 1.0.0 release. When this process is done, we will provide a nuget package at [nuget.org](https://www.nuget.org/). However, currently the way to get FSharp.Stats running on 
your machine is to either clone the repository and build the binaries yourself or download the prerelease packages from our [nuget branch](https://github.com/CSBiology/FSharp.Stats/tree/nuget).

**Using prerelease packages from the nuget branch:**

If you are using paket, add the following line to your `paket.dependencies` file:

`git https://github.com/CSBiology/FSharp.Stats.git nuget Packages: /`

you can then access the individual packages:

`nuget FSharp.Stats`

`nuget FSharp.Stats.MSF`


**To build the binaries yourself:**

**Windows**:

- Install [.Net Core SDK](https://www.microsoft.com/net/download)
- Install the dotnet tool fake cli by `dotnet tool install fake-cli -g` for global installation or `dotnet tool install fake-cli --tool-path yourtoolpath`
- go to the project folder
- use the console command `fake build`
- to just build the binaries and save time use the console command `fake build -t buildbinaries`

**Linux(Ubuntu, using Mono)**:

- Install [.Net Core SDK](https://www.microsoft.com/net/download/linux-package-manager/ubuntu14-04/sdk-current)
- go to the project folder
- use the console command `dotnet fake build --target Linux`

</br>

---

Example
-------

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

open FSharp.Plotly

let x_Data = vector [|1. .. 10.|]
let y_Data = vector [|4.;7.;9.;12.;15.;17.;16.;23.;5.;30.|]

// get coefficients of interpolating polynomial
let interpolatingCoefficients = 
    Interpolation.Polynomial.coefficients x_Data y_Data

// get fitting function of interpolating polynomial
let interpolFitFunc = 
    Interpolation.Polynomial.fit interpolatingCoefficients

(*** hide ***)
// create line chart of interpolating polynomial
let interpolChart = 
    [1. .. 0.1 .. 10.] 
    |> List.map (fun x -> x,interpolFitFunc x)
    |> fun data -> Chart.Line(data,"interpol polynomial")

(**
### Regression
*)

// get coefficients of 3rd order regression polynomial
let regressionCoefficients = 
    Fitting.LinearRegression.OrdinaryLeastSquares.Polynomial.coefficient 3 x_Data y_Data
    
// get fitting function of 3rd order regression polynomial
let regressionFitFunc = 
    Fitting.LinearRegression.OrdinaryLeastSquares.Polynomial.fit 3 regressionCoefficients

(*** hide ***)
// create line chart of regression polynomial
let regressionChart = 
    [1. .. 0.1 .. 10.] 
    |> List.map (fun x -> x,regressionFitFunc x)
    |> fun data -> Chart.Line(data,"regression polynomial")

let combinedChart =
    let rawChart = Chart.Point(x_Data,y_Data)
    [rawChart;interpolChart;regressionChart]
    |> Chart.Combine
(**
The resulting interpolating and regression polynomials are plotted below using [FSharp.Plotly](https://github.com/muehlhaus/FSharp.Plotly).

*)   
(*** include-value:combinedChart ***)

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

  [content]: https://github.com/CSBiology/FSharp.Stats/tree/master/docsrc/content
  [gh]: https://github.com/CSBiology/FSharp.Stats
  [issues]: https://github.com/CSBiology/FSharp.Stats/issues
  [readme]: https://github.com/CSBiology/FSharp.Stats/blob/master/README.md
  [license]: https://github.com/CSBiology/FSharp.Stats/blob/master/LICENSE.txt
*)
