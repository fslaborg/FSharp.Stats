(**
---
title: Probability distributions
index: 1
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

(**
# Probability Distributions

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Distributions.ipynb)

_Summary:_ this tutorial shows how to use the various types of probability distributions in FSharp.Stats.

### Table of contents

- [Continuous](#Continuous)
    - [Normal distribution](#Normal-distribution)
    - [Multivariate normal distribution](#Multivariate-normal-distribution)
    - [F distribution](#F-distribution)
- [Discrete](#Discrete)
    - [Binomial distribution](#Binomial-distribution)
    - [Hypergerometric distribution](#Hypergerometric-distribution)
    - [Poisson distribution](#Poisson-distribution)
    - [Gamma distribution](#Gamma-distribution)
- [Empirical](#Empirical)
- [Density estimation](#Density-estimation)
- [Distance](#Distance)

FSharp.Stats provides a wide range of probability distributions. Given the
distribution parameters they can be used to investigate their statistical properties
or to sample non-uniform random numbers.

For every distribution the probability density function (PDF) and cumulative probability function (CDF) can be accessed.
By using the PDF you can access the probability for exactly X=k success states. The CDF is used when the cumulative probabilities of X<=k is required.

## Continuous

### Normal distribution

The normal or Gaussian distribution is a very common continuous probability distribution.
Due to the central limit theorem, randomly sampled means of a random and independent distribution tend to approximate a normal distribution
It describes the probability, that under a given mean and 
a given dispersion (standard deviation) an event occurs exactly k times. 

It is defined by two parameters N(mu,tau):

  - mu = mean

  - tau = standard deviation

Example: The distribution of bread weights of a local manufacturer follows a normal distribution with mean 500 g and a standard
deviation of 20 g.

NormA: What is the probability of bread weights to be lower than 470 g?

NormB: What is the probability of bread weights to be higher than 505 g?

NormC: Sample independently 10 values from the normal distribution and calculate their mean.

*)

open FSharp.Stats
open FSharp.Stats.Distributions

// Creates a normal distribution with µ = 500 and tau = 20 
let normal = Continuous.normal 500. 20.

// NormA: What is the probability of bread weights to be equal or lower than 470 g?
let normA = normal.CDF 470.
// Output: 0.06681 = 6.68 %

// NormB: What is the probability of bread weights to be higher than 505 g?
let normB = 1. - (normal.CDF 505.)
// Output: 0.401294 = 40.13 %

// NormC: Sample independently 10 values from the normal distribution and calculate their mean.
let normC = 
    Seq.init 10 (fun _ -> normal.Sample())
    |> Seq.mean

(*** include-value:normC ***)

// Set a seed so that sampling is reproducible
let seed = 1

Random.SetSampleGenerator(Random.RandThreadSafe(seed))   
List.init 3 (fun _ -> normal.Sample())
(*** include-it ***)
Random.SetSampleGenerator(Random.RandThreadSafe(seed))   
List.init 3 (fun _ -> normal.Sample())
(*** include-it ***)

// Get back to unseeded sampling
Random.SetSampleGenerator(Random.RandThreadSafe())   
List.init 3 (fun _ -> normal.Sample())
(*** include-it ***)

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

let plotNormal =
    [400. .. 600.]
    |> List.map (fun x -> x,normal.PDF x)
    |> Chart.Area
    |> Chart.withAxisTitles "" ""
    |> Chart.withTitle "N(500,20) PDF"

(*** condition: ipynb ***)
#if IPYNB
plotNormal
#endif // IPYNB

(***hide***)
plotNormal |> GenericChart.toChartHTML
(***include-it-raw***)

let plotNormalCDF =

    [400. .. 600.]
    |> List.map (fun x -> x,normal.CDF x)
    |> Chart.Area
    |> Chart.withAxisTitles "" ""
    |> Chart.withTitle "N(500,20) CDF"

(*** condition: ipynb ***)
#if IPYNB
plotNormalCDF
#endif // IPYNB

(***hide***)
plotNormalCDF |> GenericChart.toChartHTML
(***include-it-raw***)

(**
### Multivariate normal distribution

Multivariate normal distributions are initialized with a mean vector and a covariance matrix.
*)

let mvn = Continuous.multivariateNormal (vector [-1.;5.]) (matrix [[0.5;1.];[0.25;1.2]])
let axisXRange = [-5. .. 0.2 .. 5.]
let axisYRange = [ 0. .. 0.2 .. 10.]

// probability density function 
let mvnPdfs =
    axisYRange |> List.map (fun y -> 
        axisXRange
        |> List.map (fun x -> 
            mvn.PDF (vector [x;y])
            )
        )

let mvnSamples = 
    Array.init 1000 (fun _ -> mvn.Sample())

let surface = Chart.Surface(mvnPdfs,axisXRange,axisYRange)

let samples = 
    mvnSamples
    |> Array.map (fun t -> t.[0],t.[1])
    |> Array.unzip
    |> fun (x,y) -> Chart.Scatter3D(x,y,Array.init x.Length (fun _ -> Random.rndgen.NextFloat() / 3.),StyleParam.Mode.Markers)

let mvnChart = 
    [surface;samples]
    |> Chart.combine
    |> Chart.withTitle "Bivariate normal distribution with sampling"


(*** condition: ipynb ***)
#if IPYNB
mvnChart
#endif // IPYNB

(***hide***)
mvnChart |> GenericChart.toChartHTML
(***include-it-raw***)
(**

###Students t distribution

*)

let studentTParams = [(0.,1.,1.);(0.,1.,2.);(0.,1.,5.);]
let xStudentT = [-10. ..0.1.. 10.]

let pdfStudentT mu tau dof = 
    xStudentT 
    |> List.map (Continuous.StudentT.PDF mu tau dof)
    |> List.zip xStudentT


let cdfStudentT mu tau dof = 
    xStudentT 
    |> List.map (Continuous.StudentT.CDF  mu tau dof)
    |> List.zip xStudentT


let v =
    studentTParams
    |> List.map (fun (mu,tau,dof) -> Chart.Spline(pdfStudentT mu tau dof,Name=sprintf "mu=%.1f tau=%.1f dof=%.1f" mu tau dof,ShowMarkers=false))
    |> Chart.combine
    |> Chart.withAxisTitles "" ""

(*** condition: ipynb ***)
#if IPYNB
v
#endif // IPYNB

(***hide***)
v |> GenericChart.toChartHTML
(***include-it-raw***)


(**
### F distribution

The F distribution or Fisher distribution, also known as Fisher-Snedecor distribution, is a continuous probability distribution. 
An F-distributed random variable results from the quotient of two Chi-square-distributed random variables each divided by the associated number of degrees of freedom. 
The F-distribution has two independent degrees of freedom(dof) as parameters, and thus forms a two-parameter distribution family.

Generally speaking, the F-tests and the resulting F-Distribution is utilized for comparing multiple levels of independent variables with multiple groups.
In practice, it is most commonly used to compare the variances within a group to the variance between different groups, as seen in the Analysis of varaince.

*)
// The F-Distribution with the numerator degree of freedom  10. and the denominator degree of freedom 25. is build like this
let fDistrobution =
    Continuous.f 10. 25.

(**

*)
(***hide***)
let fParams = [(2.,1.);(5.,2.);(10.,1.);(100.,100.)]
let xF = [0. .. 1. .. 5.]

let pdfF a b = 
    xF 
    |> List.map (Continuous.F.PDF a b)
    |> List.zip xF

let fPDFs =
    fParams
    |> List.map (fun (a,b) -> Chart.Line(pdfF a b,Name=sprintf "dof1=%.1f dof2=%.1f" a b,LineWidth=3.) )
    |> Chart.combine
    |> Chart.withAxisTitles "" ""
    |> Chart.withTitle "Different F-Distributions PDFs, x=[0,5]"

(*** condition: ipynb ***)
#if IPYNB
fPDFs
#endif // IPYNB

(***hide***)
fPDFs |> GenericChart.toChartHTML
(***include-it-raw***)
(**
*)
(***hide***)
let cdfF a b = 
    xF 
    |> List.map (Continuous.F.CDF a b)
    |> List.zip xF

let fCDFs =
    fParams
    |> List.map (fun (a,b) -> Chart.Line(cdfF a b,Name=sprintf "dof1=%.1f dof2=%.1f" a b,LineWidth=3.) )
    |> Chart.combine
    |> Chart.withAxisTitles "" ""
    |> Chart.withTitle "Different F-Distributions CDFs, x=[0,5]"

(*** condition: ipynb ***)
#if IPYNB
fCDFs
#endif // IPYNB

(***hide***)
fCDFs |> GenericChart.toChartHTML
(***include-it-raw***)


(**

## Discrete

### Binomial distribution

The binomial distribution describes the probability, that under a given success probability and 
a given number of draws an event occurs exactly k times (with replacement). 

It is defined by two parameters B(n,p):

  - n = number of draws

  - p = probability of success

Example: The school bus is late with a probability of 0.10. 

BinoA: What is the probability of running late exactly 5 times during a 30 day month?

BinoB: What is the probability of running late for a maximum of 5 times?

BinoC: What is the probability of running late for at least 5 times?

*)
open FSharp.Stats
open FSharp.Stats.Distributions

// Creates a binomial distribution with n=30 and p=0.90 
let binomial = Discrete.binomial 0.1 30

// BinoA: What is the probability of running late exactly 5 times during a 30 day month?
let binoA = binomial.PDF 5
// Output: 0.1023 = 10.23 %

// BinoB: What is the probability of running late for a maximum of 5 times?
let binoB = binomial.CDF 5.
// Output: 0.9268 = 92.68 %

// BinoC: What is the probability of running late for at least 5 times?
let binoC = 1. - (binomial.CDF 4.)
// Output: 0.1755 = 17.55 %

let plotBinomial =
    [0..30]
    |> List.map (fun x -> x,binomial.PDF x)
    |> Chart.Column
    |> Chart.withAxisTitles "" ""
    |> Chart.withTitle "B(30,0.1)"

(*** condition: ipynb ***)
#if IPYNB
v
#endif // IPYNB

(***hide***)
plotBinomial |> GenericChart.toChartHTML
(***include-it-raw***)

(**
### Hypergerometric distribution

The hypergeometric distribution describes the probability, that under a given number of success and failure events and 
a given number of draws an event occurs exactly k times (without replacement). 

It is defined by three parameters Hyp(n,s,f):

  - n = number of draws
  
  - s = number of success events

  - f = number of failure events

Example: You participate in a lottery, where you have to choose 6 numbers out of 49. The lottery queen draws 6 numbers randomly, 
where the order does not matter.

HypA: What is the probability that your 6 numbers are right?

HypB: What is the probability that you have at least 3 right ones?

HypC: What is the probability that you have a maximum of 3 right ones? 
*)

// Creates a hypergeometric distribution with n=6, s=6 and f=49-6=43.
//N=count(succes)+count(failure), K=count(success), n=number of draws
let hyper = Discrete.hypergeometric 49 6 6

// HypA: What is the probability that your 6 numbers are right?
let hypA = hyper.PDF 6
// Output: 7.15-08

// HypB: What is the probability that you have at least 3 right ones?
let hypB = 1. - hyper.CDF 2.
// Output: 0.01864 = 1.86 %

// HypC: What is the probability that you have a maximum of 3 right ones? 
let hypC = hyper.CDF 3.
// Output: 0.99901 = 99.90 %

(***hide***)
let plotHyper =
    [0..6]
    |> List.map (fun x -> x,hyper.PDF x)
    |> Chart.Column
    |> Chart.withAxisTitles "" ""
    |> Chart.withTitle "Hyp(6,6,43)"

(*** condition: ipynb ***)
#if IPYNB
plotHyper
#endif // IPYNB

(***hide***)
plotHyper |> GenericChart.toChartHTML
(***include-it-raw***)

(**
### Poisson distribution

The poisson distribution describes what the probability for a number of events is, occurring in a certain time, area, or volume.

It is defined by just one parameters Po(lambda) where lambda is the average rate.

Example: During a year, a forest is struck by a lightning 5.5 times. 

PoA: What is the probability that the lightning strikes exactly 3 times?

PoB: What is the probability that the lightning strikes less than 2 times?

PoC: What is the probability that the lightning strikes more than 7 times?
*)
// Creates a poisson distribution with lambda=  .
let poisson = Discrete.poisson 5.5

(*** do-not-eval ***)
// PoA: What is the probability that the lightning strikes exactly 3 times?
let poA = poisson.PDF 3
// Output: 0.11332 = 11.33 %

// PoB: What is the probability that the lightning strikes less or equal to 2 times?
let poB = 
    // CDF not implemented yet
    //poisson.CDF 2.
    [0 .. 2] |> List.sumBy poisson.PDF
    // Output: 0.088376 = 8.84 %
    
// PoC: What is the probability that the lightning strikes more than 7 times?
let poC = 1. -  poisson.CDF 6.
// Output: Not implemented yet. Manual addition necessary

(** *)

let plotPo =
    [0..20]
    |> List.map (fun x -> x,poisson.PDF x)
    |> Chart.Column
    |> Chart.withAxisTitles "" ""
    //|> Chart.withSize(600.,400.)
    |> Chart.withTitle "Po(5.5)"

(*** condition: ipynb ***)
#if IPYNB
plotPo
#endif // IPYNB

(***hide***)
plotPo |> GenericChart.toChartHTML
(***include-it-raw***)

(**
### Gamma distribution
*)

let gammaParams = [(1.,2.);(2.,2.);(3.,2.);(5.,1.);(9.0,0.5);(7.5,1.);(0.5,1.);]
let xgamma = [0. ..0.1.. 20.]

let pdfGamma a b = 
    xgamma 
    |> List.map (Continuous.Gamma.PDF a b)
    |> List.zip xgamma

let gammaPlot =
    gammaParams
    |> List.map (fun (a,b) -> Chart.Point(pdfGamma a b,Name=sprintf "a=%.1f b=%.1f" a b) )//,ShowMarkers=false))
    |> Chart.combine
    |> Chart.withAxisTitles "" ""

(*** condition: ipynb ***)
#if IPYNB
gammaPlot
#endif // IPYNB

(***hide***)
gammaPlot |> GenericChart.toChartHTML
(***include-it-raw***)

let cdfGamma a b = 
    xgamma 
    |> List.map (Continuous.Gamma.CDF a b)
    |> List.zip xgamma

let gammaCDFPlot=
    gammaParams
    |> List.map (fun (a,b) -> Chart.Spline(cdfGamma a b,Name=sprintf "a=%.1f b=%.1f" a b) )//,ShowMarkers=false))
    |> Chart.combine
    |> Chart.withAxisTitles "" ""

(*** condition: ipynb ***)
#if IPYNB
gammaCDFPlot
#endif // IPYNB

(***hide***)
gammaCDFPlot |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Empirical

You can create empirically derived distributions and sample randomly from these.
In this example 100,000 samples from a student t distribution 

*)

// Randomly taken samples; in this case from a gaussian normal distribution.
let sampleDistribution = 
    let template = Continuous.normal 5. 2.
    Seq.init 100000 (fun _ -> template.Sample())

//creates an empirical distribution with bandwidth 0.1
let empiricalDistribution = 
    Empirical.create 0.1 sampleDistribution

(***hide***)
let plotEmpirical =    
    empiricalDistribution
    |> Empirical.getZip
    |> Chart.Column
    |> Chart.withAxisTitles "" ""

(*** condition: ipynb ***)
#if IPYNB
plotEmpirical
#endif // IPYNB

(***hide***)
plotEmpirical |> GenericChart.toChartHTML
(***include-it-raw***)

(**
## Density estimation
*)

let nv = Array.init 1000 (fun _ -> Distributions.Continuous.Normal.Sample 5. 2.)

let xy = KernelDensity.estimate KernelDensity.Kernel.gaussian 1.0 nv

let kernelChart = 
    Chart.SplineArea xy
    |> Chart.withAxisTitles "" ""
 
(*** condition: ipynb ***)
#if IPYNB
kernelChart
#endif // IPYNB

(***hide***)
kernelChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Distance

In this example we will calculate the Wasserstein Metric between 3 distributions. One can imagine this metric as the amount of work needed to move the area (pile of dirt) of one distribution to the area of the other. That's why it's also called Earth Movers Distance.

Be aware that this distance measure is dependent on the actual absolute values of the distributions.
*)

let distribution1 = 
    let normal = Continuous.normal 300. 15.
    Array.init 1000 (fun _ -> normal.Sample())
let distribution2 =
    let normal = Continuous.normal 350. 20.
    Array.init 500 (fun _ -> normal.Sample())
let distribution3 =
    let normal = Continuous.normal 500. 20.
    Array.init 1000 (fun _ -> normal.Sample())

let pilesOfDirt =
    [
    Chart.Histogram(distribution1,Name = "Distribution1")
    Chart.Histogram(distribution2,Name = "Distribution2")
    Chart.Histogram(distribution3,Name = "Distribution3")
    ]
    |> Chart.combine

(*** condition: ipynb ***)
#if IPYNB
pilesOfDirt
#endif // IPYNB

(***hide***)
pilesOfDirt |> GenericChart.toChartHTML
(***include-it-raw***)

let distance1and2 = Distance.OneDimensional.wassersteinDistance distribution1 distribution2

(***include-value: distance1and2 ***)

let distance1and3 = Distance.OneDimensional.wassersteinDistance distribution1 distribution3
(***include-value: distance1and3 ***)

(**
As expected the distance between Distribution 1 and 2 is the lowest:
*)

let distributions = 
    [|distribution1;distribution2;distribution3|]

let mapColor min max value = 
    let proportionR = 
        ((255. - 200.) * (value - min) / (max - min))
        |> int
        |> fun x -> 255 - x
    let proportionG = 
        ((255. - 200.) * (value - min) / (max - min))
        |> int
        |> fun x -> 255 - x
    let proportionB = 
        ((255. - 200.) * (value - min) / (max - min))
        |> int
        |> fun x -> 255 - x
    Color.fromARGB 1 proportionR proportionG proportionB

let distancesTable =
    let headerColors = ["white";"#1f77b4";"#ff7f0e";"#2ca02c"] |> List.map Color.fromString |> Color.fromColors
    let distances = 
        distributions
        |> Array.map (fun x ->
            distributions
            |> Array.map (fun y ->
                Distance.OneDimensional.wassersteinDistance x y
                |> (sprintf "%.2f")
            )
        )
        |> Array.transpose 
        |> Array.append [|[|"Distribution1";"Distribution2";"Distribution3"|]|]
        |> Array.transpose 
    let cellColors = 
        distances
        |> Array.mapi (fun i a ->
            a
            |> Array.mapi (fun j v -> 
                if j = 0 then 
                    if i = 0 then Color.fromHex "#1f77b4"
                    elif i = 1 then Color.fromHex "#ff7f0e"
                    else Color.fromHex "#2ca02c"
                else 
                    mapColor 0. 200. (float v)
            )
        )
        |> Array.transpose
        |> Seq.map Color.fromColors
        |> Color.fromColors
 
    Chart.Table(
        ["";"Distribution1";"Distribution2";"Distribution3"],         
        distances,
        HeaderFillColor = headerColors,
        CellsFillColor = cellColors)

(*** condition: ipynb ***)
#if IPYNB
distancesTable
#endif // IPYNB

(***hide***)
distancesTable |> GenericChart.toChartHTML
(***include-it-raw***)
