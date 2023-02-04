#I "bin/Debug/netstandard2.0"
#r "FSharp.Stats.dll"
#r "nuget: Plotly.NET"

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open Plotly.NET


let alpha = 9.9 //0.4 
let beta  = 31 //4.2
// https://keisan.casio.com/exec/system/1180573216
let pdfs = [| 0.987113653; 0.635929273; 0.486870787; 0.400046182; 0.341683319;
                0.299071263; 0.266235685; 0.239955525; 0.218322701; 0.200126249;
                0.184555971; 0.171046668; 0.159190450; 0.148684554; 0.139298865;
                0.130854902; 0.123211796; 0.116256647; 0.109897748; 0.104059710;
                0.098679897; 0.093705765; 0.089092854; 0.084803247; 0.080804376;
                0.077068078; 0.073569861; 0.070288299; 0.067204554; 0.064301989;
                0.061565838; 0.058982949; 0.056541557; 0.054231102; 0.052042076;
                0.049965886; 0.047994748; 0.046121587; 0.044339960; 0.042643979;
                0.041028256; 0.039487846; 0.038018205; 0.036615142; 0.035274793;
                0.033993583; 0.032768200; 0.031595571; 0.030472842; 0.029397355;
                0.028366635; 0.027378369; 0.026430398; 0.025520703; 0.024647389;
                0.023808683; 0.023002918; 0.022228528; 0.021484040; 0.020768066;
                0.020079300; 0.019416507; 0.018778524; 0.018164249; 0.017572643;
                0.017002719; 0.016453546; 0.015924240; 0.015413961; 0.014921914;
                0.014447344; 0.013989532; 0.013547795; 0.013121484; 0.012709981;
                0.012312696; 0.011929068; 0.011558563; 0.011200670; 0.010854903;
                0.010520795; 0.010197904; 0.009885805; 0.009584092; 0.009292377;
                0.009010290; 0.008737475; 0.008473592; 0.008218316; 0.007971333;
                0.007732346; 0.007501068; 0.007277223; 0.007060548; 0.006850789;
                0.006647704; 0.006451059; 0.006260630; 0.006076203; 0.005897569; |]

//let xy_pdf = pdfs |> Array.mapi (fun i v -> (float (i + 1) / 10.), Continuous.Gamma.PDF alpha beta (float (i + 1) / 10.))
let samplesHisto = Array.init 999999 (fun _ -> Continuous.Gamma.Sample alpha beta)

let bw       = 1.//FSharp.Stats.Distributions.Bandwidth.forHistogram samplesHisto
let histo    = FSharp.Stats.Distributions.KernelDensity.estimate KernelDensity.Kernel.gaussian bw samplesHisto
let histoPdf = histo |> Seq.map (fun (x,y) -> x,Continuous.Gamma.PDF alpha beta x)
[
    Chart.Column(histo)
    Chart.Point(histoPdf)
]
|> Chart.combine
|> Chart.show


let alpha', beta' = Continuous.Gamma.Fit samplesHisto

let d = Continuous.Gamma.Estimate pdfs
d.Mean












open FSharp.Stats
let rnd = new System.Random(69)

let mDenseInt1 = Matrix.Generic.init 10 10 (fun r c -> $"{r}{c}" )
let mDenseInt2 = Matrix.Generic.init 10 100 (fun r c -> $"{r}{c}" )
let mDenseInt3 = Matrix.Generic.init 100 10 (fun r c -> $"{r}{c}" )
let mDenseInt4 = Matrix.Generic.init 100 100 (fun r c -> $"{r}{c}" )

mDenseInt1.Format(false)
mDenseInt1.Format(true)
mDenseInt2.Format(false)
mDenseInt2.Format(true)
mDenseInt3.Format(false)
mDenseInt3.Format(true)
mDenseInt4.Format(false)
mDenseInt4.Format(true)

let mDense1 = Matrix.init 10 10 (fun i j -> float i * float j * rnd.NextDouble())
let mDense2 = Matrix.init 10 100 (fun i j -> float i * float j * rnd.NextDouble())
let mDense3 = Matrix.init 100 10 (fun i j -> float i * float j * rnd.NextDouble())
let mDense4 = Matrix.init 100 100 (fun i j -> float i * float j * rnd.NextDouble())
let mDenseSpecial = matrix[[nan;100000000.;infinity;1.4];[1.337;-nan;4269420.42;-infinity]]

mDense1.Format(false)
mDense1.Format(true)
mDense2.Format(false)
mDense2.Format(true)
mDense3.Format(false)
mDense3.Format(true)
mDense4.Format(false)
mDense4.Format(true)
mDenseSpecial.Format(false)
mDenseSpecial.Format(true)
