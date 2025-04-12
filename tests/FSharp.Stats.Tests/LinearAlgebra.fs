module LinearAlgebraTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Algebra
open TestExtensions

[<Tests>]
let managedSVDTests =

    let svdManaged A = 
        let s,u,vt  = LinearAlgebra.SVD A
        let sM = 
            let tmp= Matrix.zeroCreate A.NumRows A.NumCols 
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
            let m = mEqualN.toJaggedArray() |> Array.concat
            let m' = mEqualNRecov.toJaggedArray() |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high m m' "Matrices computed by SVD did not yield the initial matrix when multiplied."
        
        testCase "m=n Matrix: u and vt consist of unit vectors, row- and column- wise." <| fun () -> 
            let u,s,vt = svdManaged mEqualN
            let vecNorms = 
                [|
                u |> Matrix.mapiCols (fun _ v -> [|FSharp.Stats.Vector.norm v|])
                vt|> Matrix.mapiCols (fun _ v -> [|FSharp.Stats.Vector.norm v|])
                u |> Matrix.mapiRows (fun _ x -> [|FSharp.Stats.Vector.norm x|]) 
                vt|> Matrix.mapiRows (fun _ x -> [|FSharp.Stats.Vector.norm x|]) 
                |]
                |> Array.map (fun m -> m.Data)
                |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high (Array.create vecNorms.Length 1.) vecNorms "Matrices computed by SVD did not consist of unit vectors, row- and column- wise."
        
        testCase "m=n Matrix: s contains correct singular values." <| fun () -> 
            let s,u,vt = LinearAlgebra.SVD  mEqualN
            TestExtensions.sequenceEqual Accuracy.high ([|15.81461344;2.213142934|]) s "Matrices computed by SVD did not yield correct singular values."
        
        testCase "m<n Matrix: Recover from decomposition" <| fun () -> 
            let u,s,vt = svdManaged mSmallerN
            let mSmallernRecov = (u * s * vt)
            let m = mSmallerN.Data
            let m' = mSmallernRecov.Data
            TestExtensions.sequenceEqual Accuracy.high m m' "Matrices computed by SVD did not yield the initial matrix when multiplied."
        
        testCase "m<n Matrix: u and vt consist of unit vectors, row- and column- wise." <| fun () -> 
            let u,s,vt = svdManaged mSmallerN
            let vecNorms = 
                [|
                u |> Matrix.mapiCols (fun _ v -> [|FSharp.Stats.Vector.norm v|])
                vt|> Matrix.mapiCols (fun _ v -> [|FSharp.Stats.Vector.norm v|])
                u |> Matrix.mapiRows (fun _ x -> [|FSharp.Stats.Vector.norm x|]) 
                vt|> Matrix.mapiRows (fun _ x -> [|FSharp.Stats.Vector.norm x|]) 
                |]
                |> Array.map (fun m -> m.Data)
                |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high (Array.create vecNorms.Length 1.) vecNorms "Matrices computed by SVD did not consist of unit vectors, row- and column- wise."
        
        testCase "m<n Matrix: s contains correct singular values." <| fun () -> 
            let s,u,vt = LinearAlgebra.SVD  mSmallerN
            TestExtensions.sequenceEqual Accuracy.high ([|22.51999394;6.986424855;2.00991059|]) s "Matrices computed by SVD did not yield correct singular values."
            
        testCase "m>n Matrix: Recover from decomposition" <| fun () -> 
            let u,s,vt = svdManaged (mSmallerN.Transpose())
            let mSmallernRecov = (u * s * vt)
            let m = mSmallerN.Transpose().Data
            let m' = mSmallernRecov.Data
            TestExtensions.sequenceEqual Accuracy.high m m' "Matrices computed by SVD did not yield the initial matrix when multiplied."
    
        testCase "m>n Matrix: u and vt consist of unit vectors, row- and column- wise." <| fun () -> 
            let u,s,vt = svdManaged (mSmallerN.Transpose())
            let vecNorms = 
                [|
                u |> Matrix.mapiCols (fun _ v -> [|FSharp.Stats.Vector.norm v|])
                vt|> Matrix.mapiCols (fun _ v -> [|FSharp.Stats.Vector.norm v|])
                u |> Matrix.mapiRows (fun _ x -> [|FSharp.Stats.Vector.norm x|]) 
                vt|> Matrix.mapiRows (fun _ x -> [|FSharp.Stats.Vector.norm x|]) 
                |]
                |> Array.map (fun m -> m.Data)
                |> Array.concat
            TestExtensions.sequenceEqual Accuracy.high (Array.create vecNorms.Length 1.) vecNorms "Matrices computed by SVD did not consist of unit vectors, row- and column- wise."
        
        testCase "m>n Matrix: s contains correct singular values." <| fun () -> 
            let s,u,vt = LinearAlgebra.SVD  (mSmallerN.Transpose())
            TestExtensions.sequenceEqual Accuracy.high ([|22.51999394;6.986424855;2.00991059|]) s "Matrices computed by SVD did not yield correct singular values."
    ]
    

//[<Tests>]
//let nullspace =
  
//    let mSmallerN = Matrix.ofJaggedArray [| [|2.;-1.;2.;-1.|];  [|4.;3.;4.;3.|]; [|9.;13.;-13.;9.|]; |]
    
//    testList "LinearAlgebra.nullspace" [
//        testCase "accuracy 1e-5" <| fun () -> 
//            let ns = LinearAlgebra.nullspace (Accuracy=1e-5) mSmallerN
//            let prod = 
//                mSmallerN * ns
//                |> Matrix.toJaggedSeq
//                |> Seq.concat
//            let expected = seq {0.;0.;0.;}
//            TestExtensions.sequenceEqual Accuracy.veryHigh expected prod  "A * (nullspace A) should be matrix of zeros"
//    ]

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

    let b1 = vector [|1.;1.;1.|]

    testList "Triangular Linear Systems" [
        // Tested vs R package "bdsmatrix: Routines for Block Diagonal Symmetric Matrices" Version 1.3-6
        testList "SolveTriangularLinearSystems (Upper)" [
            testCase "3x3 Upper Triangular Matrix with 3x3 Matrix (realistic example)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems
                    (
                        [|
                            [|1.;2.;3.|];
                            [|0.;1.;1.|];
                            [|0.;0.;2.|]
                        |]
                        |> Matrix.ofJaggedArray
                    )
                    (
                        [|
                            [|8.;4.;2.|];
                            [|4.;2.;1.|];
                            [|2.;1.;0.|]
                        |]
                        |> Matrix.ofJaggedArray
                    )
                    false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|-1.;-0.5;0.|];
                            [|3.;1.5;1.|];
                            [|1.;0.5;0.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix: \n-1.;-0.5;0.\n3.;1.5;1.\n1.;0.5;0."
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 B1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|1.;1.;1.|];
                            [|1.;1.;1.|];
                            [|1.;1.;1.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix of 1"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpper1 B1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|0.;0.;0.|];
                            [|0.;0.;0.|];
                            [|1.;1.;1.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix with 1 in last row"
            testCase "3x3 Upper Triangular Matrix (Values = -1) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNeg1 B1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|0.;0.;0.|];
                            [|0.;0.;0.|];
                            [|-1.;-1.;-1.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix with 1 in last row"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperInf B1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|0.;0.;0.|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNegInf B1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|0.;0.;0.|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNaN B1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|infinity;infinity;infinity|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpper1 BInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|infinity;infinity;infinity|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = -1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNeg1 BInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|-infinity;-infinity;-infinity|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperInf BInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNegInf BInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNaN BInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BNegInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|-infinity;-infinity;-infinity|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with -Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpper1 BNegInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|-infinity;-infinity;-infinity|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with -Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperInf BNegInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNegInf BNegInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNaN BNegInf false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BNaN false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpper1 BNaN false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperInf BNaN false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNegInf BNaN false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNaN BNaN false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BNeg1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|-1.;-1.;-1.|];
                            [|-1.;-1.;-1.|];
                            [|-1.;-1.;-1.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix of -1"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpper1 BNeg1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|0.;0.;0.|];
                            [|0.;0.;0.|];
                            [|-1.;-1.;-1.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix with -1 in last row"
            testCase "3x3 Upper Triangular Matrix (Values = -1) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNeg1 BNeg1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|0.;0.;0.|];
                            [|0.;0.;0.|];
                            [|1.;1.;1.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix with -1 in last row"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperInf BNeg1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|0.;0.;0.|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNegInf BNeg1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|0.;0.;0.|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with 0 in last row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KUpperNaN BNeg1 false
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
        ]
        // Tested vs R package "bdsmatrix: Routines for Block Diagonal Symmetric Matrices" Version 1.3-6
        testList "SolveTriangularLinearSystems (Lower)" [
            testCase "3x3 Lower Triangular Matrix with 3x3 Matrix (realistic example)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems
                    (
                        [|
                            [|1.;0.;0.|];
                            [|2.;1.;0.|];
                            [|3.;1.;2.|]
                        |]
                        |> Matrix.ofJaggedArray
                    )
                    (
                        [|
                            [|8.;4.;2.|];
                            [|4.;2.;1.|];
                            [|2.;1.;0.|]
                        |]
                        |> Matrix.ofJaggedArray
                    )
                    true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|8.;4.;2.|];
                            [|-12.;-6.;-3.|];
                            [|-5.;-2.5;-1.5|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix: \n8.;4.;2.\n-12.;-6.;-3.\n-5.;-2.5;-1.5"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = 1) (lower)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 B1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|1.;1.;1.|];
                            [|1.;1.;1.|];
                            [|1.;1.;1.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix of 1"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLower1 B1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|1.;1.;1.|];
                            [|0.;0.;0.|];
                            [|0.;0.;0.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix with 1 in first row"
            testCase "3x3 Lower Triangular Matrix (Values = -1) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNeg1 B1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|-1.;-1.;-1.|];
                            [|0.;0.;0.|];
                            [|0.;0.;0.|]
                        |]
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix with -1 in first row"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerInf B1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|0.;0.;0.|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNegInf B1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|0.;0.;0.|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNaN B1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = Inf) (lower)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|infinity;infinity;infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in first row and NaN in other rows"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLower1 BInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|infinity;infinity;infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = -1) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNeg1 BInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|-infinity;-infinity;-infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerInf BInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNegInf BInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNaN BInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BNegInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|-infinity;-infinity;-infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with -Inf in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLower1 BNegInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|-infinity;-infinity;-infinity|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with -Inf in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerInf BNegInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNegInf BNegInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = -Inf)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNaN BNegInf true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = NaN) (lower)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BNaN true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLower1 BNaN true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with Inf in last row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerInf BNaN true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNegInf BNaN true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = NaN)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNaN BNaN true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
            testCase "3x3 diagonal Matrix (Values = 1) with 3x3 Matrix (Values = -1) (lower)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BNeg1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|-1.;-1.;-1.|];
                            [|-1.;-1.;-1.|];
                            [|-1.;-1.;-1.|]
                        |]
                         
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix of -1"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLower1 BNeg1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|-1.;-1.;-1.|];
                            [|0.;0.;0.|];
                            [|0.;0.;0.|]
                        |]
                         
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix with -1 in first row"
            testCase "3x3 Lower Triangular Matrix (Values = -1) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNeg1 BNeg1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|1.;1.;1.|];
                            [|0.;0.;0.|];
                            [|0.;0.;0.|]
                        |]
                         
                    TestExtensions.sequenceEqual Accuracy.high res.Data expected.Data "Should be 3x3 Matrix with -1 in first row"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerInf BNeg1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|0.;0.;0.|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNegInf BNeg1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|0.;0.;0.|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix with 0 in first row and NaN in other rows"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with 3x3 Matrix (Values = -1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystems KLowerNaN BNeg1 true
                |> fun res ->
                    let expected =
                        matrix [|
                            [|nan;nan;nan|];
                            [|nan;nan;nan|];
                            [|nan;nan;nan|]
                        |]
                         
                    TestExtensions.sequenceEqualRoundedNaN 9 res.Data expected.Data "Should be 3x3 Matrix of NaN"
        ]
        testList "SolveTriangularLinearSystem (Upper)" [
            testCase "3x3 Upper Triangular Matrix with Vector (realistic example)" <| fun () ->
                 LinearAlgebra.solveTriangularLinearSystem
                     (
                         [|
                             [|1.;2.;3.|];
                             [|0.;1.;1.|];
                             [|0.;0.;2.|]
                         |]
                         |> Matrix.ofJaggedArray
                     )
                     (
                        [|8.;4.;2.|]
                        |> vector
                     )
                     false
                 |> fun res ->
                     let expected =
                        [|-1.;3.;1.|]
                     TestExtensions.sequenceEqual Accuracy.high res expected "Should be Vector of -1.;3.;1."
            testCase "3x3 diagonal Matrix (Values = 1) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KDiagonal1 b1 false
                |> fun res ->
                    let expected = [|1.;1.;1.|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of 1"
            testCase "3x3 Upper Triangular Matrix (Values = 1) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KUpper1 b1 false
                |> fun res ->
                    let expected = [|0.;0.;1.|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of 0 0 1"
            testCase "3x3 Upper Triangular Matrix (Values = -1) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KUpperNeg1 b1 false
                |> fun res ->
                    let expected = [|0.;0.;-1.|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of 0 0 -1"
            testCase "3x3 Upper Triangular Matrix (Values = Inf) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KUpperInf b1 false
                |> fun res ->
                    let expected = [|nan;nan;0|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of NaN NaN 0"
            testCase "3x3 Upper Triangular Matrix (Values = -Inf) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KUpperNegInf b1 false
                |> fun res ->
                    let expected = [|nan;nan;0|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of NaN NaN 0"
            testCase "3x3 Upper Triangular Matrix (Values = NaN) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KUpperNaN b1 false
                |> fun res ->
                    let expected = [|nan;nan;nan|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of NaN"
        ]
        testList "SolveTriangularLinearSystem (Lower)" [
            testCase "3x3 Upper Triangular Matrix with Vector (realistic example)" <| fun () ->
                 LinearAlgebra.solveTriangularLinearSystem
                     (
                         [|
                             [|1.;0.;0.|];
                             [|2.;1.;0.|];
                             [|3.;1.;2.|]
                         |]
                         |> Matrix.ofJaggedArray
                     )
                     (
                        [|8.;4.;2.|]
                        |> vector
                     )
                     true
                 |> fun res ->
                     let expected =
                        [|8.;-12.;-5.|]
                     TestExtensions.sequenceEqual Accuracy.high res expected "Should be Vector of 8.;-12.;-5."
            testCase "3x3 diagonal Matrix (Values = 1) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KDiagonal1 b1 true
                |> fun res ->
                    let expected = [|1.;1.;1.|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of 1"
            testCase "3x3 Lower Triangular Matrix (Values = 1) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KLower1 b1 true
                |> fun res ->
                    let expected = [|1.;0.;0.|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of 1 0 0"
            testCase "3x3 Lower Triangular Matrix (Values = -1) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KLowerNeg1 b1 true
                |> fun res ->
                    let expected = [|-1.;0.;0.|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of -1 0 0"
            testCase "3x3 Lower Triangular Matrix (Values = Inf) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KLowerInf b1 true
                |> fun res ->
                    let expected = [|0.;nan;nan|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of 0 NaN NaN"
            testCase "3x3 Lower Triangular Matrix (Values = -Inf) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KLowerNegInf b1 true
                |> fun res ->
                    let expected = [|0.;nan;nan|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of 0 NaN NaN"
            testCase "3x3 Lower Triangular Matrix (Values = NaN) with Vector (Values = 1)" <| fun () ->
                LinearAlgebra.solveTriangularLinearSystem KLowerNaN b1 true
                |> fun res ->
                    let expected = [|nan;nan;nan|]
                    TestExtensions.sequenceEqualRoundedNaN 9 res expected "Should be Vector of NaN"
        ]
    ]
    
