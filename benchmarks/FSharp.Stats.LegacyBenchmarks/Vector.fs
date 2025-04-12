namespace FSharp.Stats.LegacyBenchmarks


open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FSharp.Stats

[<MemoryDiagnoser>]
type VectorBenchmarks() =

    let mutable vector1 = vector [||]
    let mutable vector2 = vector [||]

    // Parameterize vector sizes
    [<Params(100, 1000, 10000)>]
    member val Size = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        vector1 <- Vector.init this.Size (fun i -> float i)
        vector2 <- Vector.init this.Size (fun i -> float (i * 2))

    [<Benchmark>]
    member _.Add() =
        let result = Vector.add vector1 vector2
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.multiply() =
        let result = Vector.cptMul vector1 vector2
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.DotProduct() =
        let result = Vector.dot vector1 vector2
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.CrossProduct() =
        let result = Vector.mulRVV vector1.Transpose vector2
        GC.KeepAlive(result) // Prevents the result from being optimized away

    [<Benchmark>]
    member _.Norm() =
        let result = Vector.norm vector1
        GC.KeepAlive(result) // Prevents the result from being optimized away

