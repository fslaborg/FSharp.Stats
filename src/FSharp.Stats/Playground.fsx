#I "bin/Debug/netstandard2.0"
#r "FSharp.Stats.dll"

open System
open FSharp.Stats
open FSharp.Stats.Distributions


let observations = [| 1275.56; 1239.44; 1237.92; 1237.22; 1237.1; 1238.41; 1238.62; 1237.05;
    1237.19; 1236.51; 1264.6; 1238.19; 1237.39; 1235.79; 1236.53; 1236.8; 1238.06; 
    1236.5; 1235.32; 1236.44; 1236.58; 1236.3; 1237.91; 1238.6; 1238.49; 1239.21; 
    1238.57; 1244.63; 1236.06; 1236.4; 1237.88; 1237.56; 1236.66; 1236.59; 1236.53; 
    1236.32; 1238.29; 1237.79; 1237.86; 1236.42; 1236.23; 1236.37; 1237.18; 1237.63; 
    1245.8; 1238.04; 1238.55; 1238.39; 1236.75; 1237.07; 1250.78; 1238.6; 1238.36; 
    1236.58; 1236.82; 1238.4; 1257.68; 1237.78; 1236.52; 1234.9; 1237.9; 1238.58; 
    1238.12; 1237.89; 1236.54; 1236.55; 1238.37; 1237.29; 1237.64; 1236.8; 1237.73; 
    1236.71; 1238.23; 1237.84; 1236.26; 1237.58; 1238.31; 1238.4; 1237.08; 1236.61; 
    1235.92; 1236.41; 1237.89; 1237.98; 1246.75; 1237.92; 1237.1; 1237.97; 1238.69; 
    1237.05; 1236.96; 1239.44; 1238.49; 1237.88; 1236.01; 1236.57; 1236.44; 1235.76; 
    1237.62; 1238; 1263.14; 1237.66; 1237; 1236; 1261.96; 1238.58; 1237.77; 1237.06; 
    1236.31; 1238.63; 1237.23; 1236.85; 1236.23; 1236.46; 1236.9; 1237.85; 1238; 
    1237.02; 1236.19; 1236.05; 1235.73; 1258.3; 1235.98; 1237.76; 1246.93; 1239.1; 
    1237.72; 1237.67; 1236.79; 1237.61; 1238.41; 1238.29; 1238.11; 1237; 1236.52; 
    1236.6; 1236.31; 1237.77; 1238.58; 1237.88; 1247.35; 1236.14; 1236.83; 1236.15; 
    1237.93; 1238.16; 1237.34; 1236.78; 1238.66; 1237.76; 1237.19; 1236.7; 1236.04; 
    1236.66; 1237.86; 1238.54; 1238.05; 1238.41; 1236.94; 1240.95; 1261.01; 1237.72; 
    1237.91; 1238.2; 1235.68; 1236.89; 1235.12; 1271.31; 1236.97; 1270.76; 1238.52; 
    1238.19; 1238.6; 1237.16; 1236.72; 1236.71; 1237.14; 1238.48; 1237.95; 1237.42; 
    1235.86; 1236.39; 1236.13; 1236.58; 1237.95; 1237.76; 1237.39; 1238.16; 1236.31; 
    1236.41; 1236.12; 1238.7; 1236.48; 1237.84; 1236.38; 1237.95; 1238.48; 1236.51; 
    1236.56 |]
let alpha, beta = Continuous.Gamma.Fit observations
let d = Continuous.Gamma.Estimate observations
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
