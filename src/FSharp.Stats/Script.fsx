#light
//#r "System.Numerics"
// D:\Source\FSharp.Stats\src\FSharp.Stats\Script.fsx
//#r "../.../packages/System.Numerics.Vectors/ref/net46/System.Numerics.Vectors.dll"
#r "../../bin/System.Numerics.Vectors.dll"
#r "../../bin/FSharp.Stats.dll"

#time

open FSharp.Stats
open FSharp.Stats.Algebra
open FSharp.Stats.Algebra.Vector


let a1 = [|1.f .. 100000.f|]
let a2 = [|1.f .. 100000.f|]

let v1 = Vector.Vector a1
let v2 = Vector.Vector a2

Vector.IsHardwareAccelerated


for i=0 to 10000 do
    Vector.min v1 |> ignore
    //Vector.add v1 v2 |> ignore


for i=0 to 10000 do
    Array.min a1 |> ignore
    //Array.map2 (+) a1 a2 |> ignore



