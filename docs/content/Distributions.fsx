(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly
(**
Probability Distributions
=========================

FSharp.Stats a provides a wide range of probability distributions. Given the
distribution parameters they can be used to investigate their statistical properties
or to sample non-uniform random numbers.

*)
#r "FSharp.Stats.dll"
open FSharp.Stats
open FSharp.Stats.Distributions


// https://en.wikipedia.org/wiki/Student%27s_t-distribution
let tmp = 
    Seq.init 100
        (fun _ -> Continuous.StudentT.Sample 1. 1. 1.)
    |> Empirical.create 0.1




let studentTParams = [(1.,1.,1.);(1.,1.,2.);(1.,1.,5.);]
let xStudentT = [-4. ..0.1.. 4.]

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


