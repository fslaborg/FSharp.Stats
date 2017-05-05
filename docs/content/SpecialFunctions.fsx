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












open FSharp.Stats.SpecialFunctions



//  incomplete beta function 
// regularized lower incomplete beta function
let betaRegularized a b x =       
    if (a < 0.0) then invalidArg "a" "Argument must not be negative"
    if (b < 0.0) then invalidArg "b" "Argument must not be negative"
    if (x < 0.0 || x > 1.0) then invalidArg "x" "Argument XY interval is inclusive"
    let bt = 
        if (x = 0.0 || x = 1.0) then
            0.0
        else
            exp (Gamma.gammaLn (a + b) - Gamma.gammaLn a - Gamma.gammaLn b + (a*Math.Log(x)) + (b*Math.Log(1.0 - x)))

    let isSymmetryTransformation = ( x >= (a + 1.0)/(a + b + 2.0))

    let symmetryTransformation a b x =
        let qab = a + b
        let qap = a + 1.0
        let qam = a - 1.0
        let c = 1.0
        let d = 
            let tmp =  1.0 - (qab * x / qap)
            if (abs tmp < FPMIN) then 1. / FPMIN else 1. / tmp
        let h = d
        let rec loop m mm d h c =                
            let aa = float m * (b - float m)*x/((qam + mm)*(a + mm))
            let d' = 
                let tmp = 1.0 + (aa*d)
                if (abs tmp < FPMIN) then 1. / FPMIN else 1. / tmp
            let c' = 
                let tmp = 1.0 + (aa/c)
                if (abs tmp < FPMIN) then FPMIN else tmp
            let h' = h * d' * c'
            let aa' = -(a + float m)*(qab + float m)*x/((a + mm)*(qap + mm))
            let d'' = 
                let tmp = 1.0 + (aa' * d')
                if (abs tmp < FPMIN) then 1. / FPMIN else 1. / tmp
            let c'' = 
                let tmp = 1.0 + (aa'/c')
                if (abs tmp < FPMIN) then FPMIN else tmp
                
            let del = d*c
            let h'' = h' * del
                
            if abs (del - 1.0) <= EPS then
                 if isSymmetryTransformation then 1.0 - (bt*h/a) else bt*h/a
            else
                if m < 140 then
                    loop (m+1) (mm+2.) d'' h'' c''
                else 
                        if isSymmetryTransformation then 1.0 - (bt*h/a) else bt*h/a
                
        loop 1 2. d h c 
            

    if isSymmetryTransformation then
        symmetryTransformation b a (1.0-x)
    else
        symmetryTransformation a b x









