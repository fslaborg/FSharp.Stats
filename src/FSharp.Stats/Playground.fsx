#I "bin/Release/netstandard2.0"
#r "FSharp.Stats.dll"

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
