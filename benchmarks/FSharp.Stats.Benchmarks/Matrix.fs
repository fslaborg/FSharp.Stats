namespace FSharp.Stats.Benchmarks

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FSharp.Stats
open FsMath
open FsMath.Algebra

[<MemoryDiagnoser>]
type MatrixBenchmarks() =

    let mutable matrixA = Unchecked.defaultof<Matrix<float>>
    let mutable matrixB = Unchecked.defaultof<Matrix<float>>
    let mutable vector = [||]

    // Parameterize matrix sizes
    [<Params(10, 500, 1000)>]
    member val Size = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        // Initialize matrices and vector
        matrixA <- Matrix.init this.Size this.Size (fun i j -> float (i + j))
        matrixB <- Matrix.init this.Size this.Size (fun i j -> float (i * j))
        vector <- Array.init this.Size (fun i -> float i)

    [<Benchmark>]
    member _.Add() =
        let result = Matrix.add matrixA matrixB
        GC.KeepAlive(result) // Prevent dead code elimination

    [<Benchmark>]
    member _.MatMultiply() =
        let result = Matrix.matmul matrixA matrixB
        GC.KeepAlive(result) // Prevent dead code elimination

    [<Benchmark>]
    member _.MultiplyVector() =
        let result = Matrix.muliplyVector matrixA vector
        GC.KeepAlive(result) // Prevent dead code elimination

    [<Benchmark>]
    member _.MultiplyRowVector() =
        let result = Matrix.multiplyRowVector vector matrixA
        GC.KeepAlive(result) // Prevent dead code elimination
