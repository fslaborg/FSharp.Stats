namespace FSharp.Stats.LegacyBenchmarks

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FSharp.Stats
open FSharp.Stats.Algebra

[<MemoryDiagnoser>]
type TriangularBenchmark() =


    // We'll hold the test data here
    let mutable K : Matrix<float> = Unchecked.defaultof<Matrix<float>>
    let mutable B : Matrix<float> = Unchecked.defaultof<Matrix<float>>

    /// <summary>
    /// We'll vary 'N' in [500, 1000, 2000] for the matrix dimension.
    /// BenchmarkDotNet will run separate benchmarks for each N.
    /// </summary>
    [<Params(10, 500, 1000, 2000)>]
    member val N = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        // Use the parameter N to build NxN matrix K and NxM matrix B
        let n = this.N
        let m = 2 // or some other number of columns for B
        let rnd = Random(1234)

        // Build K
        K <- Matrix.init n n (fun _ _ -> float(rnd.Next(1,10)))

        // Build B
        B <- Matrix.init n m (fun _ _ -> float(rnd.Next(1,5)))


    [<Benchmark>]
    member this.SolveTriangularSystemLower() =
        // The method we want to benchmark
        LinearAlgebra.SolveTriangularLinearSystems K B true

    [<Benchmark>]
    member this.SolveTriangularSystemUpper() =
        // The method we want to benchmark
        LinearAlgebra.SolveTriangularLinearSystems K B false