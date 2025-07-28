module LinearAlgebraTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Algebra
open TestExtensions


[<Tests>]
let householderTests =
  testList "Householder tests" [

    test "Householder reflection zeros subcolumn (3x2 matrix)" {
      let A = Matrix(3, 2, [| 4.0; 1.0;
                             2.0; 3.0;
                             6.0; 5.0 |])
      let R = Matrix.copy A
      let x = Matrix.getCol 0 R |> Vector.sub 0

      let h = Householder.create x
      Householder.applyLeft(h, R, 0)

      // After application, first column should be [beta, 0, 0]
      Expect.floatClose Accuracy.high R.[1,0] 0.0 "R[1,0] should be zero"
      Expect.floatClose Accuracy.high R.[2,0] 0.0 "R[2,0] should be zero"

      // Check tau and beta match known values (approximate)
      let expectedBeta = sqrt (4.0**2 + 2.0**2 + 6.0**2) // ||x||
      Expect.floatClose Accuracy.high (float h.Beta) expectedBeta "Beta should match norm"
      Expect.isTrue (float h.Tau > 0.0 && float h.Tau < 2.0) "Tau should be in (0, 2)"
    }

    test "Householder reflection zeros subcolumn (2x2 matrix)" {
      let A = Matrix(2, 2, [| 3.0; 1.0;
                             4.0; 5.0 |])
      let R = Matrix.copy A
      let x = Matrix.getCol 0 R |> Vector.sub 0

      let h = Householder.create x
      Householder.applyLeft(h, R, 0)

      Expect.floatClose Accuracy.high R.[1,0] 0.0 "R[1,0] should be zero"
      let expectedBeta = sqrt (3.0**2 + 4.0**2)
      Expect.floatClose Accuracy.high (float h.Beta) expectedBeta "Beta should match norm"
    }

    test "Tau and v correctness on a known input" {
      let x = [| 3.0; 4.0 |]
      let h = Householder.create x

      // We expect:
      // - v[0] = 1
      // - v[1] = (4 / v0), since we normalize the vector to have v[0] = 1
      // - tau e (0, 2), beta = ||x||
      Expect.equal h.V.[0] 1.0 "v[0] should be 1.0"
      Expect.isTrue (float h.Tau > 0.0 && float h.Tau < 2.0) "Tau should be reasonable"
      Expect.floatClose Accuracy.high (float h.Beta) 5.0 "Beta should be ||x|| = 5.0"
    }

    test "Degenerate vector returns trivial Householder" {
      let x = [| 0.0; 0.0; 0.0 |]
      let h = Householder.create x
      Expect.equal h.Tau 0.0 "Tau should be 0.0"
      Expect.equal h.Beta 0.0 "Beta should be 0.0"
      Expect.sequenceEqual h.V [| 1.0; 0.0; 0.0 |] "v should be unit vector"
    }

  ]




[<Tests>]
let subScaledRowInPlaceTests =
  testList "subScaledRowInPlace" [

    test "Basic subtraction (no offset)" {
      let dst = [| 10.0; 20.0; 30.0 |]
      let src = [| 1.0; 2.0; 3.0 |]
      LinearAlgebra.subScaledRowInPlace 2.0 0 0 3 dst src
      let expected = [| 8.0; 16.0; 24.0 |]
      Expect.sequenceEqual dst expected "Subtraction incorrect"
    }

    test "With dst and src offset" {
      let dst = [| 0.0; 10.0; 20.0; 30.0; 0.0 |]
      let src = [| 0.0; 1.0; 2.0; 3.0; 0.0 |]
      // Subtract 2×src[1..3] from dst[1..3]
      LinearAlgebra.subScaledRowInPlace 2.0 1 1 3 dst src
      let expected = [| 0.0; 8.0; 16.0; 24.0; 0.0 |]
      Expect.sequenceEqual dst expected "Offset subtraction incorrect"
    }

    test "Single element (scalar fallback)" {
      let dst = [| 42.0 |]
      let src = [| 2.0 |]
      LinearAlgebra.subScaledRowInPlace 3.0 0 0 1 dst src
      let expected = [| 36.0 |]
      Expect.sequenceEqual dst expected "Scalar fallback incorrect"
    }

    test "No modification outside range" {
      let dst = [| 1.0; 2.0; 3.0; 4.0 |]
      let src = [| 0.0; 1.0; 1.0; 0.0 |]
      LinearAlgebra.subScaledRowInPlace 2.0 1 1 2 dst src
      let expected = [| 1.0; 0.0; 1.0; 4.0 |]
      Expect.sequenceEqual dst expected "Boundary elements modified"
    }

    test "basic 3-element row subtraction" {
      let dst = [| 1.0; 2.0; 3.0 |]
      let src = [| 4.0; 5.0; 6.0 |]
      let alpha = 2.0
      let expected = [| -7.0; -8.0; -9.0 |]

      LinearAlgebra.subScaledRowInPlace alpha 0 0 3 dst src

      Expect.sequenceEqual dst expected "Result should be elementwise dst - alpha * src"
    }

    test "offsets respected correctly" {
      let dst = [| 0.0; 1.0; 2.0; 3.0; 4.0 |]
      let src = [| 0.0; 0.0; 1.0; 2.0; 3.0 |]
      let alpha = 2.0
      let expected = [| 0.0; 1.0; 0.0; -1.0; -2.0 |]

      // Only update dst[2..4] = [2.0; 3.0; 4.0] with src[2..4] = [1.0; 2.0; 3.0]
      LinearAlgebra.subScaledRowInPlace alpha 2 2 3 dst src

      Expect.sequenceEqual dst expected "Offsets should target the correct subarrays"
    }

  ]

[<Tests>]
let qrDecompositionTests =
  testList "QR Decomposition (Modified Gram-Schmidt)" [

    test "Decompose identity matrix" {
      let A = Matrix.identity 3
      let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
      let I = Matrix.identity 3
      TestExtensions.floatMatrixClose Accuracy.high Q I "Q should be identity"
      TestExtensions.floatMatrixClose Accuracy.high R I "R should be identity"
    }

    test "Reconstruct original matrix from Q * R" {
      let A = Matrix(3, 2, [| 1.0; 0.0;
                             1.0; 1.0;
                             0.0; 1.0 |])
      let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
      let QR = Matrix.matmul Q R
      TestExtensions.floatMatrixClose Accuracy.high QR A "Q * R should reconstruct A"
    }

    test "Q has orthonormal columns" {
      let A = Matrix(3, 2, [| 1.0; 0.0;
                             1.0; 1.0;
                             0.0; 1.0 |])
      let Q, _ = LinearAlgebra.qrModifiedGramSchmidt A
      let QTQ = Matrix.matmul (Matrix.transpose Q) Q
      let I = Matrix.identity 2
      TestExtensions.floatMatrixClose Accuracy.high QTQ I "Q^TQ should be identity"
    }

    test "R is upper triangular" {
      let A = Matrix(3, 2, [| 1.0; 0.0;
                             1.0; 1.0;
                             0.0; 1.0 |])
      let _, R = LinearAlgebra.qrModifiedGramSchmidt A
      for i in 0 .. R.NumRows - 1 do
        for j in 0 .. i - 1 do
          Expect.floatClose Accuracy.high R.[i, j] 0.0 $"R[{i},{j}] should be zero"
    }

    test "Tall matrix 4x2 decomposition" {
      let A = Matrix(4, 2, [| 1.0; 2.0;
                             3.0; 4.0;
                             5.0; 6.0;
                             7.0; 8.0 |])
      let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
      let QR = Matrix.matmul Q R
      TestExtensions.floatMatrixClose Accuracy.medium QR A "QR ~ A for tall matrix"
    }

    test "Wide matrix throws or truncates" {
      let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                             4.0; 5.0; 6.0 |])
      Expect.throws (fun () -> LinearAlgebra.qrModifiedGramSchmidt A |> ignore)
        "Wide matrices are not supported (m < n)"
    }
  ]

[<Tests>]
let backSubstituteTests =
    testList "Back substitution tests" [

    test "Solve simple upper triangular system" {
        // R = [2 1]
        //     [0 3]
        let r = Matrix(2, 2, [| 2.0; 1.0;
                                0.0; 3.0 |])
        let y = [| 5.0; 6.0 |]
        let x = LinearAlgebra. backSubstitute r y
        // Solve: 3x1 = 6 -> x1 = 2
        // Then: 2x0 + x1 = 5 -> x0 = (5 - 1*2)/2 = 1.5
        let expected = [| 1.5; 2.0 |]
        for i in 0..1 do
        Expect.floatClose Accuracy.high x.[i] expected.[i] $"x[{i}] incorrect"
    }

    test "Identity matrix returns y as solution" {
        let r = Matrix.identity 3
        let y = [| 4.0; -2.0; 0.5 |]
        let x = LinearAlgebra.backSubstitute r y
        Expect.sequenceEqual x y "Identity matrix should return y = x"
    }

    test "Upper triangular with zeros below diagonal" {
        // R = [1 2 3]
        //     [0 4 5]
        //     [0 0 6]
        let r = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                                0.0; 4.0; 5.0;
                                0.0; 0.0; 6.0 |])
        let y = [| 14.0; 23.0; 18.0 |]
        let x = LinearAlgebra.backSubstitute r y
        let expected = [| 1.0; 2.0; 3.0 |]
        for i in 0..2 do
        Expect.floatClose Accuracy.high x.[i] expected.[i] $"x[{i}] incorrect"
    }

    test "Throws on non-square R" {
        let r = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                                0.0; 4.0; 5.0 |])
        let y = [| 1.0; 2.0 |]
        Expect.throws (fun () -> LinearAlgebra.backSubstitute r y |> ignore)
         "Should throw on non-square R"
    }

    test "Throws on mismatched dimensions with y" {
        let r = Matrix.identity 3
        let y = [| 1.0; 2.0 |]
        Expect.throws (fun () -> LinearAlgebra.backSubstitute r y |> ignore)
         "Should throw on mismatched y length"
    }

    test "Throws on zero diagonal entry (division by zero)" {
        let r = Matrix(2, 2, [| 0.0; 1.0;
                                0.0; 2.0 |])
        let y = [| 3.0; 4.0 |]
        Expect.throws (fun () -> LinearAlgebra.backSubstitute r y |> ignore)
         "Should throw on division by zero"
    }
    ]

// solveLinearQR
// solveTriangularLinearSystems


[<Tests>]
let solveTriangularLinearSystemTests =
  testList "solveTriangularLinearSystem tests" [

    test "Solve L x = b (lower triangular)" {
      // L = [1 0 0; 2 1 0; 3 4 1]
      let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                             2.0; 1.0; 0.0;
                             3.0; 4.0; 1.0 |])
      let b = [| 2.0; 5.0; 20.0 |]
      let x = LinearAlgebra.solveTriangularLinearSystem L b true
      // Solve: Lx = b -> x = [2; 1; 5]
      let expected = [| 2.0; 1.0; 10.0 |]
      for i in 0..2 do
        Expect.floatClose Accuracy.high x.[i] expected.[i] $"x[{i}] incorrect"
    }

    test "Solve L^T x = b (upper triangular)" {
      // L = [1 0 0; 2 1 0; 3 4 1]
      // L^T = [1 2 3; 0 1 4; 0 0 1]
      let L = Matrix(3, 3, [| 1.0; 0.0; 0.0;
                             2.0; 1.0; 0.0;
                             3.0; 4.0; 1.0 |])
      
      let b = [| 23.0; 9.0; 1.0 |]
      let LT = Matrix.transpose L
      let x = LinearAlgebra.solveTriangularLinearSystem LT b false
      // Solve L^T x = b -> x = [1; 2; 1]
      let expected = [| 10.0; 5.0; 1.0 |]
      for i in 0..2 do
        Expect.floatClose Accuracy.high x.[i] expected.[i] $"x[{i}] incorrect"
    }

    test "Throws on mismatched dimensions" {
      let L = Matrix.identity 3
      let b = [| 1.0; 2.0 |]
      Expect.throws (fun () -> LinearAlgebra.solveTriangularLinearSystem L b true |> ignore)
        "Should throw if dimensions don't match"
    }

    test "Throws on zero diagonal (division by zero)" {
      let L = Matrix(2, 2, [| 0.0; 0.0;
                             1.0; 1.0 |])
      let b = [| 1.0; 2.0 |]
      Expect.throws (fun () -> LinearAlgebra.solveTriangularLinearSystem L b true |> ignore)
        "Should throw on division by zero"
    }
  ]

[<Tests>]
let qrTests =
  testList "QR decomposition via Householder" [

    test "Q * R reconstructs A" {
      let A = Matrix(3, 2, [| 1.0; 1.0;
                             1.0; 2.0;
                             1.0; 3.0 |])
      let Q, R = LinearAlgebra.qrDecompose A
      let QR = Matrix.matmul Q R
      TestExtensions.floatMatrixClose Accuracy.high QR A "QR ~ A"
    }

    test "Q is orthogonal: Q^TQ = I" {
      let A = Matrix(3, 2, [| 1.0; 0.0;
                             1.0; 1.0;
                             1.0; 2.0 |])
      let Q, _ = LinearAlgebra.qrDecompose A
      let QtQ = Matrix.matmul (Matrix.transpose Q) Q
      let I = Matrix.identity Q.NumCols
      TestExtensions.floatMatrixClose Accuracy.high QtQ I "Q^TQ ~ I"
    }

    test "R is upper triangular" {
      let A = Matrix(3, 2, [| 1.0; 0.0;
                             2.0; 1.0;
                             3.0; 2.0 |])
      let _, R = LinearAlgebra.qrDecompose A
      for i = 0 to R.NumRows - 1 do
        for j = 0 to i - 1 do
          Expect.floatClose Accuracy.veryHigh R.[i,j] 0.0 $"R[{i},{j}] should be zero"
    }

    test "QR of square matrix" {
      let A = Matrix(3, 3, [| 1.0; 2.0; 3.0;
                             4.0; 5.0; 6.0;
                             7.0; 8.0; 9.0 |])
      let Q, R = LinearAlgebra.qrDecompose A
      let QR = Q * R
      TestExtensions.floatMatrixClose Accuracy.medium QR A "QR ~ A"
    }

    test "QR of small square matrix" {
        let A = Matrix(2, 2, [| 2.0; 1.0;
                                1.0; 3.0 |])
        let Q, R = LinearAlgebra.qrDecompose A
        let QR = Q * R
        TestExtensions.floatMatrixClose Accuracy.medium QR A "QR ~ A"
    }

  ]

[<Tests>]
let leastSquaresQRTests =
  testList "Least squares (QR-based)" [

    test "Overdetermined 3x2 system" {
      let A = Matrix(3, 2, [| 1.0; 1.0;
                             1.0; 2.0;
                             1.0; 3.0 |])
      let b = [| 6.0; 0.0; 0.0 |]
      let x = LinearAlgebra.leastSquares A b
      let expected = [| 8.0; -3.0 |]
      for i in 0..1 do
        Expect.floatClose Accuracy.medium x.[i] expected.[i] $"x[{i}] incorrect"
    }

    test "Underdetermined 2x3 system (minimum norm)" {
      let A = Matrix(2, 3, [| 1.0; 2.0; 0.0;
                             0.0; 1.0; 1.0 |])
      let b = [| 3.0; 2.0 |]
      let x = LinearAlgebra.leastSquares A b

      // There are infinite solutions, but least norm solution is:
      // min ||x|| such that Ax = b
      // numpy gives: [0.33333333, 1.33333333, 0.66666667]
      let expected = [| 0.33333333; 1.33333333; 0.66666667 |]
      for i in 0..2 do
        Expect.floatClose Accuracy.medium x.[i] expected.[i] $"x[{i}] incorrect"
    }

    test "Square system returns exact solution" {
      let A = Matrix(2, 2, [| 2.0; 1.0;
                             1.0; 3.0 |])
      let b = [| 5.0; 10.0 |]
      let x = LinearAlgebra.leastSquares A b
      let expected = [| 1.0; 3.0 |]
      for i in 0..1 do
        Expect.floatClose Accuracy.high x.[i] expected.[i] $"x[{i}] incorrect"
    }

    test "Residual Ax ~ projection of b" {
      let A = Matrix(3, 2, [| 1.0; 1.0;
                             1.0; 2.0;
                             1.0; 3.0 |])
      let b = [| 6.0; 0.0; 0.0 |]
      let x = LinearAlgebra.leastSquares A b
      let Ax = Matrix.muliplyVector A x
      let expected = [| 5.0; 2.0; -1.0 |]
      for i in 0..2 do
        Expect.floatClose Accuracy.medium Ax.[i] expected.[i] $"Ax[{i}] incorrect"
    }
  ]


[<Tests>]
let choleskyTests =
  testList "Cholesky decomposition tests" [

    test "Decompose identity matrix" {
      let I = Matrix.identity 3
      let L = LinearAlgebra.cholesky I
      TestExtensions.floatMatrixClose Accuracy.high L I "L should be identity for identity input"
    }

    test "Decompose known positive-definite matrix" {
      // A = [4 2; 2 3]  -> L = [2 0; 1 1.4142]
      let A = Matrix(2, 2, [| 4.0; 2.0;
                             2.0; 3.0 |])
      let L = LinearAlgebra.cholesky A
      let expected = Matrix(2, 2, [| 2.0; 0.0;
                                    1.0; sqrt 2.0 |])
      for i in 0..1 do
        for j in 0..1 do
          Expect.floatClose Accuracy.medium L.[i,j] expected.[i,j] $"L[{i},{j}] incorrect"
    }

    test "Reconstruct A from L * L^T" {
      let A = Matrix(3, 3, [| 25.0; 15.0; -5.0;
                             15.0; 18.0;  0.0;
                             -5.0; 0.0; 11.0 |])
      let L = LinearAlgebra.cholesky A
      let LT = Matrix.transpose L
      let reconstructed = Matrix.matmul L LT
      TestExtensions.floatMatrixClose Accuracy.high reconstructed A "Reconstructed A <> original"
    }
    test "Throws on non-positive-definite matrix" {
      // Not positive-definite: eigenvalues are [1, -1]
      let A = Matrix(2, 2, [| 0.0; 1.0;
                             1.0; 0.0 |])
      Expect.throws (fun () -> LinearAlgebra.cholesky A |> ignore)
        "Should throw on non-positive-definite matrix"
    }

    test "Throws on non-square matrix" {
      let A = Matrix(2, 3, [| 1.0; 2.0; 3.0;
                             2.0; 5.0; 6.0 |])
      Expect.throws (fun () -> LinearAlgebra.cholesky A |> ignore)
        "Should throw on non-square matrix"
    }

    test "Zero matrix throws (not positive-definite)" {
      let A : Matrix<float> = Matrix.zeroCreate 3 3
      Expect.throws (fun () -> LinearAlgebra.cholesky A |> ignore)
        "Should throw on zero matrix"
    }

  ]

[<Tests>]
let leastSquaresTests =
  testList "Least Squares via Cholesky" [

    test "Solve overdetermined 3x2 system" {
      // A = [1 1; 1 2; 1 3], b = [6; 0; 0]
      // Should find least-squares fit: beta = [8.0; -3.0]
      let A = Matrix(3, 2, [| 1.0; 1.0;
                             1.0; 2.0;
                             1.0; 3.0 |])
      let b = [| 6.0; 0.0; 0.0 |]
      let beta = LinearAlgebra.leastSquaresCholesky A b
      let expected = [| 8.0; -3.0 |]
      for i in 0..1 do
        Expect.floatClose Accuracy.medium beta.[i] expected.[i] $"beta[{i}] incorrect"
    }

    test "Reconstruction A * beta ~ projection of b" {
      // Same setup as above
      let A = Matrix(3, 2, [| 1.0; 1.0;
                             1.0; 2.0;
                             1.0; 3.0 |])
      let b = [| 6.0; 0.0; 0.0 |]
      let beta = LinearAlgebra.leastSquaresCholesky A b
      let predicted = Matrix.muliplyVector A beta
      // Expected projection of b onto Col(A): A * (A' b)
      let expected = [| 5.0; 2.0; -1.0 |]
      for i in 0..2 do
        Expect.floatClose Accuracy.medium predicted.[i] expected.[i] $"predicted[{i}] incorrect"
    }

    test "Throws on dimension mismatch A and b" {
      let A = Matrix.identity 3
      let b = [| 1.0; 2.0 |]
      Expect.throws (fun () -> LinearAlgebra.leastSquaresCholesky A b |> ignore)
        "Should throw if A.NumRows <> b.Length"
    }

    test "Exact solution when A is square and full-rank" {
      let A = Matrix(2, 2, [| 2.0; 1.0;
                             1.0; 3.0 |])
      let b = [| 5.0; 10.0 |]
      let beta = LinearAlgebra.leastSquaresCholesky A b
      let expected = [| 1.0; 3.0 |]
      for i in 0..1 do
        Expect.floatClose Accuracy.high beta.[i] expected.[i] $"beta[{i}] incorrect"
    }

  ]



// hatMatrix
// leverageBy
// leverage
// luDecompose 
// solveLinearSystems (LU)
// solveLinearSystem (LU)









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
    
