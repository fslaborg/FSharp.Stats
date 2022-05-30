#I "bin/Release/netstandard2.0"
#r "FSharp.Stats.dll"

open FSharp.Stats
let rnd = new System.Random(1)
let m2 = Matrix.Generic.init 50 50 (fun i j -> int $"{i}{j}")
let m = Matrix.init 500 500 (fun i j -> float i * float j + (rnd.NextDouble()))

printfn "%s" (m.Format(true))

