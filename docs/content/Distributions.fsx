(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly
(**
#Probability Distributions


FSharp.Stats a provides a wide range of probability distributions. Given the
distribution parameters they can be used to investigate their statistical properties
or to sample non-uniform random numbers.

<a name="Bandwidth"></a>

##Bandwidth

<a name="Frequency"></a>

##Frequency

<a name="Empirical"></a>

##Empirical

<a name="Distribution"></a>

##Distribution

<a name="Continuous"></a>

##Continuous

<a name="Discrete"></a>

##Discrete

*)
#r "FSharp.Stats.dll"
open FSharp.Stats
open FSharp.Stats.Distributions


FSharp.Stats.SpecialFunctions.Factorial.factorial 5

// let trapz x y =
//     Seq.zip x y
//     |> Seq.pairwise 
//     |> Seq.fold (fun acc ((x0,y0),(x1,y1)) -> acc + (x1 - x0) * (y0 + y1) / 2.) 0.

// ####################################################
// Student's T-distribution
// https://en.wikipedia.org/wiki/Student%27s_t-distribution

let studentTParams = [(0.,1.,1.);(0.,1.,2.);(0.,1.,5.);]
let xStudentT = [-10. ..0.1.. 10.]

let pdfStudentT mu tau dof = 
    xStudentT 
    |> List.map (Continuous.StudentT.PDF mu tau dof)
    |> List.zip xStudentT

studentTParams
|> List.map (fun (mu,tau,dof) -> Chart.Spline(pdfStudentT mu tau dof,Name=sprintf "mu=%.1f tau=%.1f dof=%.1f" mu tau dof,ShowMarkers=false))
(*** define-output:PdfStudentT ***)
|> Chart.Combine
|> Chart.withX_AxisStyle("x",MinMax=(-4.,4.))
|> Chart.withY_AxisStyle("P(x)",MinMax=(0.,0.4))
|> Chart.withSize (500., 450.)
(*** include-it:PdfStudentT ***)
|> Chart.Show


let cdfStudentT mu tau dof = 
    xStudentT 
    |> List.map (Continuous.StudentT.CDF  mu tau dof)
    |> List.zip xStudentT

studentTParams
|> List.map (fun (mu,tau,dof) -> Chart.Spline(cdfStudentT mu tau dof,Name=sprintf "mu=%.1f tau=%.1f dof=%.1f" mu tau dof,ShowMarkers=false))
(*** define-output:CdfStudentT ***)
|> Chart.Combine
|> Chart.withX_AxisStyle("x",MinMax=(-4.,4.))
|> Chart.withY_AxisStyle("P(x)",MinMax=(0.,1.))
|> Chart.withSize (500., 450.)
(*** include-it:CdfStudentT ***)
|> Chart.Show


let mu,tau,dof = (0.0,1.0,15.0)

(*** define-output:sampleStudentT ***)
[
    Seq.init 100000
        (fun _ -> Continuous.StudentT.Sample mu tau dof)
    |> Empirical.create 0.1
    |> Empirical.getZip
    |> Chart.Column;
    Chart.Spline(pdfStudentT mu tau dof,Name=sprintf "mu=%.1f tau=%.1f dof=%.1f" mu tau dof,ShowMarkers=false)
]
|> Chart.Combine
|> Chart.withSize (500., 450.)
(*** define-output:sampleStudentT ***)
|> Chart.Show



// ####################################################
// Gamma distribution

let gammaParams = [(1.,2.);(2.,2.);(3.,2.);(5.,1.);(9.0,0.5);(7.5,1.);(0.5,1.);] // |> List.map (fun (x,y) -> y,x)
let xgamma = [0. ..0.1.. 20.]

let pdfGamma a b = 
    xgamma 
    |> List.map (Continuous.Gamma.PDF a b)
    |> List.zip xgamma

gammaParams
|> List.map (fun (a,b) -> Chart.Point(pdfGamma a b,Name=sprintf "a=%.1f b=%.1f" a b) )//,ShowMarkers=false))
(*** define-output:PdfGamma ***)
|> Chart.Combine
|> Chart.withX_AxisStyle("x",MinMax=(0.,20.))
|> Chart.withY_AxisStyle("P(x)",MinMax=(0.,0.5))
|> Chart.withSize (500., 450.)
(*** include-it:PdfGamma ***)
|> Chart.Show


let cdfGamma a b = 
    xgamma 
    |> List.map (Continuous.Gamma.CDF a b)
    |> List.zip xgamma

gammaParams
|> List.map (fun (a,b) -> Chart.Spline(cdfGamma a b,Name=sprintf "a=%.1f b=%.1f" a b) )//,ShowMarkers=false))
(*** define-output:CdfGamma ***)
|> Chart.Combine
|> Chart.withX_AxisStyle("x",MinMax=(0.,20.))
|> Chart.withY_AxisStyle("P(x)",MinMax=(0.,1.0))
|> Chart.withSize (500., 450.)
(*** include-it:CdfGamma ***)
|> Chart.Show


// let alpha = 2.0
// let beta  = 3.0
let alpha = 0.5
let beta  = 0.5

(*** define-output:sampleGamma ***)
[
    Seq.init 100000
        (fun _ -> Continuous.Gamma.Sample alpha beta)
    |> Empirical.create 0.5
    |> Empirical.getZip
    |> Chart.Column;
    Chart.Spline(pdfGamma alpha beta,Name=sprintf "a=%.1f b=%.1f" alpha beta)
]
|> Chart.Combine
|> Chart.withSize (500., 450.)
(*** define-output:sampleGamma ***)
|> Chart.Show







// ##############
// Density estimation

let nv = Array.init 1000 (fun _ -> Distributions.Continuous.Normal.Sample 5. 2.)

let xy = KernelDensity.estimate KernelDensity.Kernel.gaussian 1.0 nv

Chart.SplineArea xy
|> Chart.Show




// ####################################################
// Binomial distribution
// https://en.wikipedia.org/wiki/Binomial_distribution

let binomialParams = [(0.5,20);(0.7,20);(0.5,40);]
let xBinomial = [0 .. 1 .. 40]

let pdfBinomial p n = 
    xBinomial 
    |> List.map (Discrete.Binomial.PDF p n)
    |> List.zip xBinomial

binomialParams
|> List.map (fun (p,n) -> Chart.Spline(pdfBinomial p n,Name=sprintf "p=%.1f n=%i" p n,ShowMarkers=false))
(*** define-output:PdfStudentT ***)
|> Chart.Combine
|> Chart.withX_AxisStyle("x")
|> Chart.withY_AxisStyle("P(x)")
|> Chart.withSize (500., 450.)
(*** include-it:PdfStudentT ***)
|> Chart.Show





