namespace FSharp.Stats.LegacyBenchmarks

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FSharp.Stats

[<MemoryDiagnoser>]
type MatrixBenchmarks() =

    let mutable matrixA = Unchecked.defaultof<Matrix<float>>
    let mutable matrixB = Unchecked.defaultof<Matrix<float>>
    let mutable vector = vector [||]

    // Parameterize matrix sizes
    [<Params(10, 500, 1000)>]
    member val Size = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        // Initialize matrices and vector
        matrixA <- Matrix.init this.Size this.Size (fun i j -> float (i + j))
        matrixB <- Matrix.init this.Size this.Size (fun i j -> float (i * j))
        vector <- Vector.init this.Size (fun i -> float i)

    [<Benchmark>]
    member _.Add() =
        let result = Matrix.add matrixA matrixB
        GC.KeepAlive(result) // Prevent dead code elimination

    [<Benchmark>]
    member _.MatMultiply() =
        let result = Matrix.mul matrixA matrixB
        GC.KeepAlive(result) // Prevent dead code elimination

    [<Benchmark>]
    member _.MultiplyVector() =
        let result = Matrix.mulV matrixA vector
        GC.KeepAlive(result) // Prevent dead code elimination

    [<Benchmark>]
    member _.MultiplyRowVector() =
        let result = Matrix.mulRV vector.Transpose matrixA
        GC.KeepAlive(result) // Prevent dead code elimination
