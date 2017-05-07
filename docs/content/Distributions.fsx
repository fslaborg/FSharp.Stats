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

let Empiricalcreate bandwidth data =            
    let halfBw = bandwidth / 2.0       
    data
    |> Seq.groupBy (fun x -> floor (x / bandwidth)) 
    |> Seq.map (fun (k,values) -> 
        let count = (Seq.length(values)) |> float                                        
        if k < 0. then
            ((k  * bandwidth) + halfBw, count)   
        else
            ((k + 1.) * bandwidth) - halfBw, count)  
    |> Map.ofSeq
    |> Empirical.normalize 1.


let midRect f (x:float) (h:float) = f (x + h/2.)

let trapezium f (x:float) (h:float) = ( (f x) + f (x+h)) / 2.0

let simpson f (x:float) (h:float) = (f x + 4. * f (x + h/2.) + f(x+h))/6.0


// https://en.wikipedia.org/wiki/Student%27s_t-distribution
let tmp = 
    Seq.init 10000
        (fun _ -> Continuous.StudentT.Sample 0. 1. 1.)
    |> Empiricalcreate 0.5
    |> Empirical.getZip

[
    Chart.Column tmp;
]
|> Chart.Combine
|> Chart.withX_AxisStyle("x",MinMax=(-4.,4.))
|> Chart.withY_AxisStyle("P(x)",MinMax=(0.,0.4))
|> Chart.withSize (500., 450.)
|> Chart.Show

let studentTParams = [(0.,1.,1.);(0.,1.,2.);(0.,1.,5.);]
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


