#light
//#r "System.Numerics"
// D:\Source\FSharp.Stats\src\FSharp.Stats\Script.fsx
//#r "../.../packages/System.Numerics.Vectors/ref/net46/System.Numerics.Vectors.dll"
#r "../../bin/System.Numerics.Vectors.dll"
#r "../../bin/FSharp.Stats.dll"

#time



let rec loop (f:'a -> bool) index thr a b =
    let jump = b - a / 2.
    if (abs index-jump) <= thr then
        if f jump then
            Some jump
        else
            None
    else
        if f jump then
            // left
            loop f jump thr a jump
        else
            // right
            loop f jump thr jump b
        
let tf x = x >= 0.6

loop tf 0. 0.01 0. 1. 


//open FSharp.Stats
//open FSharp.Stats.Algebra
//open FSharp.Stats.Algebra.Vector
//
//
//let a1 = [|1. .. 100000.|]
//let a2 = [|1. .. 100000.|]
//
//let v1 = Vector.Vector a1
//let v2 = Vector.Vector a2
//
//Vector.IsHardwareAccelerated
//
//
//for i=0 to 100000 do
//    //Vector.min v1 |> ignore
//    Vector.add' v1 v2 |> ignore
//    //Vector.sum v1  |> ignore
//    //SIMDUtil.map2 (+) a1 a2 |> ignore 
//
//for i=0 to 1000000 do
//    //Array.min a1 |> ignore
//    Array.map2 (+) a1 a2 |> ignore
//    //Array.sum a1  |> ignore
//
//
//System.Numerics.Vector()
//
//let v1' = System.Numerics.Vector(a1)
//let v2' = System.Numerics.Vector(a2)
//
//for i=0 to 1000000 do
//    //System.Numerics.Vector.Add(v1',v2') |> ignore
//    //System.Numerics.Vector.op_Addition(v1',v2') |> ignore
//    SIMDUtil.add a1 a2 |> ignore 

open System.Numerics

let inline ofArrayAt index (array : 'T[]) =
    let count = System.Numerics.Vector< 'T>.Count
    let result = Array.zeroCreate array.Length
    let rec
    
    if index+count <= array.Length then
        System.Numerics.Vector< 'T>(array,index)
    else
        let tmpT = 
            Array.init count 
                (fun i -> 
                    if i+index < array.Length then array.[i+index] 
                    else (Unchecked.defaultof< 'T>) )
        System.Numerics.Vector< 'T>(tmpT,0)

let inline map 
    (mapping : 'T Vector -> 'U Vector) (source : 'T[]) : 'U[] =
        let count = System.Numerics.Vector< 'T>.Count
        let result = Array.zeroCreate source.Length

        if source.Length < count then
            let tmpT = Array.init count (fun i -> if i < source.Length then source.[i] else (Unchecked.defaultof< 'T>) )
            let tmpU = Array.zeroCreate count 
            let cVec = System.Numerics.Vector< 'T>(tmpT,0)
            (mapping cVec).CopyTo(tmpU,0)
            Array.blit tmpU 0 result 0 source.Length
            
        else
            
            let rec loop i =
                if i <= source.Length-count then 
                    let cVec = System.Numerics.Vector< 'T>(source,i)
                    (mapping cVec).CopyTo(result,i)
                    loop (i+count)
                else 
                    let cVec = System.Numerics.Vector< 'T>(source,source.Length-count)
                    (mapping cVec).CopyTo(result,source.Length-count)
            loop 0
        
        result
