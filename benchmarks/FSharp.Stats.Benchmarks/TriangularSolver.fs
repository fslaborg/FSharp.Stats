namespace FSharp.Stats.Benchmarks

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FSharp.Stats
open FsMath
open FsMath.Algebra

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
        let dataK = Array.init (n*n) (fun _ -> float(rnd.Next(1,10)))
        K <- Matrix<float>(n, n, dataK)

        // Build B
        let dataB = Array.init (n*m) (fun _ -> float(rnd.Next(1,5)))
        B <- Matrix<float>(n, m, dataB)


    [<Benchmark>]
    member this.SolveTriangularSystemLower() =
        // The method we want to benchmark
        LinearAlgebra.solveTriangularLinearSystems K B true

    [<Benchmark>]
    member this.SolveTriangularSystemUpper() =
        // The method we want to benchmark
        LinearAlgebra.solveTriangularLinearSystems K B false