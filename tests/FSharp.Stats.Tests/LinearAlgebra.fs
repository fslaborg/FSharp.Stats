module LinearAlgebraTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Algebra
open FSharp.Stats.Algebra.LinearAlgebraManaged
open TestExtensions

[<Tests>]
let managedSVDTests =

    let svdManaged A = 
        let s,u,vt  = LinearAlgebraManaged.SVD A
        let sM = 
            let tmp= Matrix.create A.NumRows A.NumCols 0. 
            for i = 0 to s.Length-1 do 
                tmp.[i,i] <- s.[i]
            tmp
        u,sM,vt
    
    let mSmallerN = Matrix.ofJaggedArray [| [|2.;-1.;2.;-1.|];  [|4.;3.;4.;3.|]; [|9.;13.;-13.;9.|]; |]
    let mEqualN = Matrix.ofJaggedArray [| [|2.;-1.|]; [|9.;13.|]; |]

    testList "LinearAlgebra.LinearAlgebraManaged.SVD" [
        testCase "m=n Matrix: Recover from decomposition" <| fun () -> 
            let u,s,vt = svdManaged mEqualN
            let mEqualNRecov = (u * s * vt)
            let m = mEqualN |> Matrix.toJaggedArray |> Array.concat
            let m' = mEqualNRecov |> Matrix.toJaggedArray |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high m m' "Matrices computed by SVD did not yield the initial matrix when multiplied."
        
        testCase "m=n Matrix: u and vt consist of unit vectors, row- and column- wise." <| fun () -> 
            let u,s,vt = svdManaged mEqualN
            let vecNorms = 
                [
                u |> Matrix.mapCols Vector.norm |> RowVector.toArray
                vt|> Matrix.mapCols Vector.norm |> RowVector.toArray
                u |> Matrix.mapRows (fun x -> x.Transpose |> Vector.norm) |> Vector.toArray
                vt|> Matrix.mapRows (fun x -> x.Transpose |> Vector.norm) |> Vector.toArray
                ]
                |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high (Array.create vecNorms.Length 1.) vecNorms "Matrices computed by SVD did not consist of unit vectors, row- and column- wise."
        
        testCase "m=n Matrix: s contains correct singular values." <| fun () -> 
            let s,u,vt = LinearAlgebraManaged.SVD  mEqualN
            TestExtensions.sequenceEqual Accuracy.high ([|15.81461344;2.213142934|]) s "Matrices computed by SVD did not yield correct singular values."
        
        testCase "m<n Matrix: Recover from decomposition" <| fun () -> 
            let u,s,vt = svdManaged mSmallerN
            let mSmallernRecov = (u * s * vt)
            let m = mSmallerN |> Matrix.toJaggedArray |> Array.concat
            let m' = mSmallernRecov |> Matrix.toJaggedArray |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high m m' "Matrices computed by SVD did not yield the initial matrix when multiplied."
        
        testCase "m<n Matrix: u and vt consist of unit vectors, row- and column- wise." <| fun () -> 
            let u,s,vt = svdManaged mSmallerN
            let vecNorms = 
                [
                u |> Matrix.mapCols Vector.norm |> RowVector.toArray
                vt|> Matrix.mapCols Vector.norm |> RowVector.toArray
                u |> Matrix.mapRows (fun x -> x.Transpose |> Vector.norm) |> Vector.toArray
                vt|> Matrix.mapRows (fun x -> x.Transpose |> Vector.norm) |> Vector.toArray
                ]
                |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high (Array.create vecNorms.Length 1.) vecNorms "Matrices computed by SVD did not consist of unit vectors, row- and column- wise."
        
        testCase "m<n Matrix: s contains correct singular values." <| fun () -> 
            let s,u,vt = LinearAlgebraManaged.SVD  mSmallerN
            TestExtensions.sequenceEqual Accuracy.high ([|22.51999394;6.986424855;2.00991059|]) s "Matrices computed by SVD did not yield correct singular values."
            
        testCase "m>n Matrix: Recover from decomposition" <| fun () -> 
            let u,s,vt = svdManaged mSmallerN.Transpose
            let mSmallernRecov = (u * s * vt)
            let m = mSmallerN.Transpose |> Matrix.toJaggedArray |> Array.concat
            let m' = mSmallernRecov |> Matrix.toJaggedArray |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high m m' "Matrices computed by SVD did not yield the initial matrix when multiplied."
    
        testCase "m>n Matrix: u and vt consist of unit vectors, row- and column- wise." <| fun () -> 
            let u,s,vt = svdManaged mSmallerN.Transpose
            let vecNorms = 
                [
                u |> Matrix.mapCols Vector.norm |> RowVector.toArray
                vt|> Matrix.mapCols Vector.norm |> RowVector.toArray
                u |> Matrix.mapRows (fun x -> x.Transpose |> Vector.norm) |> Vector.toArray
                vt|> Matrix.mapRows (fun x -> x.Transpose |> Vector.norm) |> Vector.toArray
                ]
                |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high (Array.create vecNorms.Length 1.) vecNorms "Matrices computed by SVD did not consist of unit vectors, row- and column- wise."
        
        testCase "m>n Matrix: s contains correct singular values." <| fun () -> 
            let s,u,vt = LinearAlgebraManaged.SVD  mSmallerN.Transpose
            TestExtensions.sequenceEqual Accuracy.high ([|22.51999394;6.986424855;2.00991059|]) s "Matrices computed by SVD did not yield correct singular values."
    ]
    

[<Tests>]
let nullspace =
  
    let mSmallerN = Matrix.ofJaggedArray [| [|2.;-1.;2.;-1.|];  [|4.;3.;4.;3.|]; [|9.;13.;-13.;9.|]; |]
    
    testList "LinearAlgebra.nullspace" [
        testCase "accuracy 1e-5" <| fun () -> 
            let ns = LinearAlgebra.nullspace (Accuracy=1e-5) mSmallerN
            let prod = 
                mSmallerN * ns
                |> Matrix.toJaggedSeq
                |> Seq.concat
            let expected = seq {0.;0.;0.;}
            TestExtensions.sequenceEqual Accuracy.veryHigh expected prod  "A * (nullspace A) should be matrix of zeros"
    ]

[<Tests>]
let linearSystems =
    
    let KDiagonal1 =
        [|
            [|1.;0.;0.|]
            [|0.;1.;0.|]
            [|0.;0.;1.|]
        |]
        |> Matrix.ofJaggedArray

    let KUpper1 =
        [|
            [|1.;1.;1.|]
            [|0.;1.;1.|]
            [|0.;0.;1.|]
        |]
        |> Matrix.ofJaggedArray

    let KUpperNeg1 =
        [|
            [|-1.;-1.;-1.|]
            [|0.;-1.;-1.|]
            [|0.;0.;-1.|]
        |]
        |> Matrix.ofJaggedArray

    let KUpperInf =
        [|
            [|infinity;infinity;infinity|]
            [|0.;infinity;infinity|]
            [|0.;0.;infinity|]
        |]
        |> Matrix.ofJaggedArray

    let KUpperNegInf =
        [|
            [|-infinity;-infinity;-infinity|]
            [|0.;-infinity;-infinity|]
            [|0.;0.;-infinity|]
        |]
        |> Matrix.ofJaggedArray

    let KUpperNaN =
        [|
            [|nan;nan;nan|]
            [|0.;nan;nan|]
            [|0.;0.;nan|]
        |]
        |> Matrix.ofJaggedArray

    let KLower1 =
        [|
            [|1.;1.;1.|]
            [|0.;1.;1.|]
            [|0.;0.;1.|]
        |]
        |> Matrix.ofJaggedArray
        |> Matrix.transpose

    let KLowerNeg1 =
        [|
            [|-1.;-1.;-1.|]
            [|0.;-1.;-1.|]
            [|0.;0.;-1.|]
        |]
        |> Matrix.ofJaggedArray
        |> Matrix.transpose

    let KLowerInf =
        [|
            [|infinity;infinity;infinity|]
            [|0.;infinity;infinity|]
            [|0.;0.;infinity|]
        |]
        |> Matrix.ofJaggedArray
        |> Matrix.transpose

    let KLowerNegInf =
        [|
            [|-infinity;-infinity;-infinity|]
            [|0.;-infinity;-infinity|]
            [|0.;0.;-infinity|]
        |]
        |> Matrix.ofJaggedArray
        |> Matrix.transpose

    let KLowerNaN =
        [|
            [|nan;nan;nan|]
            [|0.;nan;nan|]
            [|0.;0.;nan|]
        |]
        |> Matrix.ofJaggedArray
        |> Matrix.transpose

    let B1 =
        [|
            [|1.;1.;1.|]
            [|1.;1.;1.|]
            [|1.;1.;1.|]
        |]
        |> Matrix.ofJaggedArray

    let BNeg1 =
        [|
            [|-1.;-1.;-1.|]
            [|-1.;-1.;-1.|]
            [|-1.;-1.;-1.|]
        |]
        |> Matrix.ofJaggedArray

    let BInf =
        [|
            [|infinity;infinity;infinity|]
            [|infinity;infinity;infinity|]
            [|infinity;infinity;infinity|]
        |]
        |> Matrix.ofJaggedArray

    let BNegInf =
        [|
            [|-infinity;-infinity;-infinity|]
            [|-infinity;-infinity;-infinity|]
            [|-infinity;-infinity;-infinity|]
        |]
        |> Matrix.ofJaggedArray

    let BNaN =
        [|
            [|nan;nan;nan|]
            [|nan;nan;nan|]
            [|nan;nan;nan|]
        |]
        |> Matrix.ofJaggedArray
    testList "Triangular Linear Systems" [
        // Tested vs R package "bdsmatrix: Routines for Block Diagonal Symmetric Matrices" Version 1.3-6
        testList "Upper Triangular Linear Systems" [
    
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 B1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|1.;1.;1.|];
                            [|1.;1.;1.|];
                            [|1.;1.;1.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix of 1"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KUpper1 B1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|0.;0.;0.|];
                            [|0.;0.;0.|];
                            [|1.;1.;1.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix with 1 in last row"
            testCase "3x3 Upper Triangular Matrix (Values = -1) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KUpperNeg1 B1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|0.;0.;0.|];
                            [|0.;0.;0.|];
                            [|-1.;-1.;-1.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix with 1 in last row"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KUpperInf B1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|0.;0.;0.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KUpperNegInf B1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|0.;0.;0.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KUpperNaN B1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 BInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|infinity;infinity;infinity|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            // This test fails. Difference to R Implementation?
            //testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = Inf)" <| fun () ->
            //    SolveTriangularLinearSystems KUpper1 BInf false
            //    |> fun res ->
            //        let concatRes =
            //            res
            //            |> Matrix.toJaggedArray
            //            |> Array.concat
            //        let concatExpected =
            //            [|
            //                [|nan;nan;nan|];
            //                [|nan;nan;nan|];
            //                [|infinity;infinity;infinity|]
            //            |]
            //            |> Array.concat
            //        TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = -1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KUpperNeg1 BInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|-infinity;-infinity;-infinity|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KUpperInf BInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KUpperNegInf BInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KUpperNaN BInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 BNegInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|-infinity;-infinity;-infinity|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with -Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KUpper1 BNegInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|-infinity;-infinity;-infinity|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with -Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KUpperInf BNegInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KUpperNegInf BNegInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KUpperNaN BNegInf false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 BNaN false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KUpper1 BNaN false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KUpperInf BNaN false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KUpperNegInf BNaN false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KUpperNaN BNaN false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 BNeg1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|-1.;-1.;-1.|];
                            [|-1.;-1.;-1.|];
                            [|-1.;-1.;-1.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix of -1"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KUpper1 BNeg1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|0.;0.;0.|];
                            [|0.;0.;0.|];
                            [|-1.;-1.;-1.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix with -1 in last row"
            testCase "3x3 Upper Triangular Matrix (Values = -1) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KUpperNeg1 BNeg1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|0.;0.;0.|];
                            [|0.;0.;0.|];
                            [|1.;1.;1.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix with -1 in last row"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KUpperInf BNeg1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|0.;0.;0.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KUpperNegInf BNeg1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|0.;0.;0.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KUpperNaN BNeg1 false
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
        ]
        // Tested vs R package "bdsmatrix: Routines for Block Diagonal Symmetric Matrices" Version 1.3-6
        testList "Lower Triangular Linear Systems" [
    
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = 1) (lower)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 B1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|1.;1.;1.|];
                            [|1.;1.;1.|];
                            [|1.;1.;1.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix of 1"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KLower1 B1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|1.;1.;1.|];
                            [|0.;0.;0.|];
                            [|0.;0.;0.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix with 1 in first row"
            testCase "3x3 Lower Triangular Matrix (Values = -1) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KLowerNeg1 B1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|-1.;-1.;-1.|];
                            [|0.;0.;0.|];
                            [|0.;0.;0.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix with -1 in first row"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KLowerInf B1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|0.;0.;0.|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KLowerNegInf B1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|0.;0.;0.|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = 1)" <| fun () ->
                SolveTriangularLinearSystems KLowerNaN B1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = Inf) (lower)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 BInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|infinity;infinity;infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in first row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KLower1 BInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|infinity;infinity;infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = -1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KLowerNeg1 BInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|-infinity;-infinity;-infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KLowerInf BInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KLowerNegInf BInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = Inf)" <| fun () ->
                SolveTriangularLinearSystems KLowerNaN BInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 BNegInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|-infinity;-infinity;-infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with -Inf in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KLower1 BNegInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|-infinity;-infinity;-infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with -Inf in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KLowerInf BNegInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KLowerNegInf BNegInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                SolveTriangularLinearSystems KLowerNaN BNegInf true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = NaN) (lower)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 BNaN true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KLower1 BNaN true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KLowerInf BNaN true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KLowerNegInf BNaN true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = NaN)" <| fun () ->
                SolveTriangularLinearSystems KLowerNaN BNaN true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = -1) (lower)" <| fun () ->
                SolveTriangularLinearSystems KDiagonal1 BNeg1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|-1.;-1.;-1.|];
                            [|-1.;-1.;-1.|];
                            [|-1.;-1.;-1.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix of -1"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KLower1 BNeg1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|-1.;-1.;-1.|];
                            [|0.;0.;0.|];
                            [|0.;0.;0.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix with -1 in first row"
            testCase "3x3 Lower Triangular Matrix (Values = -1) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KLowerNeg1 BNeg1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|1.;1.;1.|];
                            [|0.;0.;0.|];
                            [|0.;0.;0.|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqual Accuracy.high concatRes concatExpected "Should be 3x3 Matrix with -1 in first row"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KLowerInf BNeg1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|0.;0.;0.|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KLowerNegInf BNeg1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|0.;0.;0.|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = -1)" <| fun () ->
                SolveTriangularLinearSystems KLowerNaN BNeg1 true
                |> fun res ->
                    let concatRes =
                        res
                        |> Matrix.toJaggedArray
                        |> Array.concat
                    let concatExpected =
                        [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                        |> Array.concat
                    TestExtensions.sequenceEqualRoundedNaN 9 concatRes concatExpected "Should be 3x3 Matrix of NaN"
        ]
    ]
    
