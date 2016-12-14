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


let a1 = [|1. .. 100000.|]
let a2 = [|1. .. 100000.|]

let v1 = Vector.Vector a1
let v2 = Vector.Vector a2

Vector.IsHardwareAccelerated


for i=0 to 10000 do
    //Vector.min v1 |> ignore
    Vector.add v1 v2 |> ignore


for i=0 to 10000 do
    //Array.min a1 |> ignore
    Array.map2 (+) a1 a2 |> ignore


System.Numerics.Vector([|3.;3.;|],1)

let inline ofArrayAt index (array : 'T[]) =
    let count = System.Numerics.Vector< 'T>.Count
    if index+count <= array.Length then
        System.Numerics.Vector< 'T>(array,index)
    else
        let tmpT = 
            Array.init count 
                (fun i -> 
                    if i+index < array.Length then array.[i+index] 
                    else (Unchecked.defaultof< 'T>) )
        System.Numerics.Vector< 'T>(tmpT,0)

ofArrayAt 3 [|1. .. 4.|]

 