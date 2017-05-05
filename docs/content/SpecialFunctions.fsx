(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly
(**
dfdf
*)
//#r "D:/Source/FSharp.Stats/bin/FSharp.Stats.dll"
#r "FSharp.Stats.dll"
//open FSharp
open FSharp.Stats

let xgamma = [-4. ..0.05.. 4.]
let ygamma = xgamma |> List.map SpecialFunctions.Gamma.gamma

List.zip xgamma ygamma
|> Chart.Spline
|> Chart.withY_AxisStyle("gamma",MinMax=(-4.,4.))
|> Chart.withX_AxisStyle("x")
|> Chart.withSize (500., 450.)
|> Chart.Show

let xgammaLn = [0.01 ..0.1.. 5.]
let ygammaLn = xgammaLn |> List.map SpecialFunctions.Gamma.gammaLn

List.zip xgammaLn ygammaLn
|> Chart.Spline
|> Chart.withY_AxisStyle("log gamma",MinMax=(-4.,4.))
|> Chart.withX_AxisStyle("x")
|> Chart.withSize (500., 450.)
|> Chart.Show


let agammaInc = [0.5;1.;5.;10.;]
let xgammaInc = [0. .. 0.5 .. 20.]
let ygammaInc a = 
    xgammaInc 
    |> List.map (fun x -> SpecialFunctions.Gamma.lowerIncomplete a x) 
    |> List.zip xgammaInc

agammaInc
|> List.map (fun a -> Chart.Spline(ygammaInc a,Name=sprintf "a=%.1f" a,ShowMarkers=false))
|> Chart.Combine
|> Chart.withY_AxisStyle("lower incomplete gamma P(a,x)")
|> Chart.withY_AxisStyle("x")
|> Chart.withSize (500., 450.)
|> Chart.Show



let agammaInc' = [0.5;1.;5.;10.;]
let xgammaInc' = [0. .. 0.1 .. 20.]
let ygammaInc' a = 
    xgammaInc' 
    |> List.map (fun x -> SpecialFunctions.Gamma.upperIncomplete a x) 
    |> List.zip xgammaInc'

agammaInc'
|> List.map (fun a -> Chart.Spline(ygammaInc' a,Name=sprintf "a=%.1f" a,ShowMarkers=false))
|> Chart.Combine
|> Chart.withY_AxisStyle("upper incomplete gamma P(a,x)")
|> Chart.withY_AxisStyle("x")
|> Chart.withSize (500., 450.)
|> Chart.Show

