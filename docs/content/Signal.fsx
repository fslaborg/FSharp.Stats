(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly
(**

#Signal

<a name="FFT"></a>

## Fast Fourier transform

<a name="Wavelet"></a>

##Wavelet

<a name="ContinuousWavelet"></a>

##Continuous Wavelet

<a name="Baseline"></a>

##Baseline

<a name="Filtering"></a>

##Filtering


*)

#r "FSharp.Stats.dll"
open FSharp.Stats

let fs = 1000            // Sampling frequency                    
let tp = 1. / float fs    // Sampling period       
let l = 1500;            // Length of signal
// Time vector
let t = Array.init (l-1) (fun x -> float x * tp)       

let pi = System.Math.PI

let signal t = 0.7 * sin (2.*pi*50.*t) + sin (2.*pi*120.*t)
let y = t |> Array.map signal
let y' = Signal.FFT.inverse (y |> Array.map (fun v ->  Complex.Create (v, 0.) )) |> Array.map (fun c -> c.RealPart)

[
    Chart.Line(t,y)
    Chart.Line(t,y')
]
|> Chart.Combine
|> Chart.Show


Chart.Line(t,y') |> Chart.Show



let x = [|0. .. 0.1 .. 10.|]
let f t = 5. + 2. * cos (2.*pi*t-90.) + 3.* cos (4.*pi*t)
let y = x |> Array.map f
let y' = Signal.FFT.inverse (y |> Array.map (fun v ->  Complex.Create (v, 0.) )) |> Array.map (fun c -> c.RealPart)

Chart.Line(x,x |> Array.map f) |> Chart.Show

Chart.Line(x,y') |> Chart.Show
