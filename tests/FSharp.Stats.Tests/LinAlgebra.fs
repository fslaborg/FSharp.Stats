module LinAlgebraTests

open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Algebra

[<Tests>]
let linearAlgebraTests =
    testList "LinearAlgebra Tests" [

        // ----------------------------------------------------------------------
        // Basic tests
        // ----------------------------------------------------------------------

        testCase "subScaledRowInPlace: subtract scaled portion of source row from destination row" <| fun _ ->
            let dst = [|10.0; 20.0; 30.0|]
            let src = [|1.0 ;  2.0 ;  3.0 |]
            LinearAlgebra.subScaledRowInPlace 2.0 0 0 3 dst src
            let expected = [|8.0; 16.0; 24.0|]
            Expect.equal dst expected "dst should be updated correctly"

        testCase "householderTransform: returns a Householder vector for column i" <| fun _ ->
            let matData = [| 1.0;  2.0;  3.0
                             4.0;  5.0;  6.0
                             7.0;  8.0;  9.0 |]
            let A = Matrix(3, 3, matData)            
            let v = LinearAlgebra.householderTransform A 1
            Expect.equal v.Length 3 "Householder vector should have 3 elements"
            Expect.isFalse (Double.IsNaN v.[1]) "v.[1] should not be NaN"

        testCase "qrModifiedGramSchmidt: Q*R reconstructs A (2x2 example)" <| fun _ ->
            let A = Matrix(2, 2, [|1.0; 2.0; 3.0; 4.0|])
            let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
            // Multiply Q*R => 2x2 => compare with A
            let APrime = Array.zeroCreate 4
            for i in 0..1 do
                for j in 0..1 do
                    let mutable sum = 0.0
                    for k in 0..1 do
                        sum <- sum + Q.[i,k] * R.[k,j]
                    APrime.[i*2 + j] <- sum
            for idx in 0..3 do
                Expect.floatClose Accuracy.high  A.Data.[idx] APrime.[idx] 
                    $"A' and A differ at idx={idx}"

        testCase "backSubstitute: solves R*x=y for x, R upper-triangular" <| fun _ ->
            // R = [2,3; 0,4], y = [8,12] => x= [-0.5,3]
            let R = Matrix(2, 2, [|2.0;3.0; 0.0;4.0|])
            let y = [|8.0;12.0|]
            let x = LinearAlgebra.backSubstitute R y
            let expected = [|-0.5;3.0|]
            Expect.floatClose Accuracy.high  expected.[0] x.[0] "x0 mismatch"
            Expect.floatClose Accuracy.high  expected.[1] x.[1] "x1 mismatch"

        testCase "solveLinearQR: solves A*x=b (2x2 system)" <| fun _ ->
            // A= [1,2; 3,4], b= [5,11] => x= [1,2]
            let A = Matrix(2,2,[|1.;2.;3.;4.|])
            let b = [|5.; 11.|]
            let x = LinearAlgebra.solveLinearQR A b
            let expected = [|1.;2.|]
            Expect.floatClose Accuracy.high  expected.[0] x.[0] "x[0] mismatch"
            Expect.floatClose Accuracy.high  expected.[1] x.[1] "x[1] mismatch"


        // ======================================================================
        // EDGE CASES: Infinity, -Infinity, NaN
        // ======================================================================
        testList "Edge Cases with Infinity, -Infinity, NaN" [

            testCase "subScaledRowInPlace with scaleVal = +∞" <| fun _ ->
                // Let dst[0] = +∞, and scaleVal= +∞. Then dst[0] = ∞ - ∞*something => usually NaN
                // Also check second entry for leftover infinite results.
                let dst = [| Double.PositiveInfinity; 10.0 |]
                let src = [| 2.0; 1.0 |]
                LinearAlgebra.subScaledRowInPlace Double.PositiveInfinity 0 0 2 dst src
                // The math:
                //   dst[0] <- ∞ - (∞ * 2) => ∞ - ∞ => NaN
                //   dst[1] <- 10 - (∞ * 1) => 10 - ∞ => -∞
                Expect.isTrue (Double.IsNaN dst.[0])  "Expected NaN in dst.[0]"
                Expect.isTrue (Double.IsNegativeInfinity dst.[1]) "Expected -∞ in dst.[1]"

            testCase "householderTransform with NaN in matrix" <| fun _ ->
                // Insert a NaN into the matrix
                let matData = [| 1.0; Double.NaN;  3.0
                                 4.0; 5.0;         6.0 |] 
                // 2x3 matrix
                let A = Matrix(2, 3, matData)
                let v = LinearAlgebra.householderTransform A 1
                // It's likely that the result will incorporate NaN somewhere (since row #1 has NaN).
                // We'll confirm that it yields at least one NaN.
                let hasNaN = v |> Array.exists Double.IsNaN
                Expect.isTrue hasNaN "Expected Householder vector to contain NaN"

            testCase "backSubstitute with 0.0 on R-diagonal => Infinity result" <| fun _ ->
                // R = [ [2., 1.],
                //       [0., 0.] ] => zero on diagonal => x.[1] => y.[1] / 0 => ∞ or NaN
                let rData = [|2.;1.; 0.;0.|]
                let R = Matrix(2,2,rData)
                let y = [|5.0; 3.0|]
                // This might yield x.[1] = 3.0 / 0 => +∞ (or possibly throw).
                // If your code doesn't throw, let's see if it yields Infinity:
                let x = LinearAlgebra.backSubstitute R y
                // We'll check x.[1]
                Expect.isTrue (Double.IsInfinity x.[1] || Double.IsNaN x.[1])
                              "Expected Infinity or NaN for x.[1]"

            testCase "solveLinearQR with b containing ∞" <| fun _ ->
                // 2x2 matrix A => [1,2; 3,4], b => [∞, 1]
                // The solution might be ∞ or NaN, or the factorization might produce weird results.
                let A = Matrix(2,2,[|1.;2.;3.;4.|])
                let b = [|Double.PositiveInfinity; 1.0|]
                // We'll see if it returns a vector with ∞ or NaN, or possibly throws.
                // We'll not test for a "correct" solution in the usual sense, only that it yields a result.
                let x = LinearAlgebra.solveLinearQR A b
                // We expect at least one ∞ or NaN in the solution:
                let hasSpecial = x |> Array.exists (fun v -> Double.IsNaN(v) || Double.IsInfinity(v))
                Expect.isTrue hasSpecial "Expected ∞ or NaN in the solution"
        ]

        testList "Extended LinearAlgebra Tests" [

           // =====================================================================
            // solveLinearQR
            // =====================================================================
            testCase "solveLinearQR: throws if b's length doesn't match A's row count" <| fun _ ->
                // A is 2x2, but b has length=3 => mismatch
                let A = Matrix(2, 2, [|1.0;2.0; 3.0;4.0|])
                let b = [|1.0; 2.0; 3.0|] // length=3
                Expect.throwsT<ArgumentException> (fun () ->
                    let _ = LinearAlgebra.solveLinearQR A b
                    ()
                ) "Should throw if A.NumRows != b.Length"

            testCase "solveLinearQR: solves small 3x3 system" <| fun _ ->
                // We'll define a 3x3 with an easy integer solution.
                //
                //   A*x = b
                //   A = [ [1.,2.,0.]
                //         [2.,1.,1.]
                //         [0.,1.,2.] ]
                //
                // We'll pick x => [1., 2., 3.] => let's compute b => A*x by hand:
                // row0 => 1*1 +2*2 +0*3 = 5
                // row1 => 2*1 +1*2 +1*3 = 2 +2 +3=7
                // row2 => 0*1 +1*2 +2*3 = 2 +6=8
                // => b= [5,7,8]
                let Adata = [|
                    1.0; 2.0; 0.0
                    2.0; 1.0; 1.0
                    0.0; 1.0; 2.0 |] // row-major flatten
                let A = Matrix(3, 3, Adata)
                let b = [|5.0; 7.0; 8.0|]

                let x = LinearAlgebra.solveLinearQR A b
                // Expect x= [1,2,3]
                let expected = [|1.0; 2.0; 3.0|]
                for i in 0..2 do
                    Expect.floatClose Accuracy.high expected.[i] x.[i] $"x[{i}] mismatch"

            // =====================================================================
            // solveTriangularLinearSystems
            // =====================================================================
            testCase "solveTriangularLinearSystems: throws if shape mismatch" <| fun _ ->
                // K must be square NxN, B must have Nx? shape
                // We'll define a 2x2 K, and B as 3x2 => mismatch
                let K = Matrix(2, 2, [|2.0;0.0; 1.0;2.0|]) // just a 2x2
                let B = Matrix(3, 2, [|1.0;2.0; 3.0;4.0; 5.0;6.0|]) // 3x2
                Expect.throwsT<ArgumentException> (fun () ->
                    let _ = LinearAlgebra.solveTriangularLinearSystems K B true
                    ()
                ) "Should throw on dimension mismatch"

            testCase "solveTriangularLinearSystems: lower-triangular forward substitution (2x2, 2 columns)" <| fun _ ->
                // K= L= [ [2,0],
                //         [1,2] ]
                // K is 2x2, B is 2x2
                // We'll pick B so that K*X = B has a known solution X.
                //
                // Suppose X => [ [1., 10.],
                //                [2.,  4.] ]
                // Then K*X => B
                // B row0 => row0*K => [2*1 +0*2, 2*10 +0*4] => [2,20]
                // B row1 => row1*K => [1*1 +2*2, 1*10 +2*4] => [1+4=5, 10+8=18]
                // Wait, careful with row-major. Actually, let's do it systematically:
                //   If K is NxN, X is NxM => B is NxM
                //   B[i,*] = sum_{j} (K[i,j] * X[j,*])  (like matrix multiply)
                //
                // For K= [ [2,0], [1,2] ], X= [ [1,10],[2,4] ]
                // Flatten X => row0= (1,10), row1= (2,4)
                // B[0,*] => K[0,0]*X[0,*] + K[0,1]*X[1,*]
                //         => 2*(1,10) + 0*(2,4) => (2,20)
                // B[1,*] => K[1,0]*X[0,*] + K[1,1]*X[1,*]
                //         => 1*(1,10)+2*(2,4) => (1+4, 10+8)= (5,18)
                // => B= [ (2,20),(5,18) ]
                let Kdata = [| 2.0; 0.0
                               1.0; 2.0 |]
                let X_expected_data = [| 1.0; 10.0
                                         2.0; 4.0  |]
                let B_data = [| 2.0; 20.0
                                5.0; 18.0 |]
                let Kmat = Matrix(2,2,Kdata)
                let Bmat = Matrix(2,2,B_data)
                // We want to solve K*X = B for X. isLower=true => forward substitution
                let Xsol = LinearAlgebra.solveTriangularLinearSystems Kmat Bmat true
                // Check that Xsol matches X_expected
                for i in 0..3 do
                    Expect.floatClose Accuracy.high X_expected_data.[i] Xsol.Data.[i]  $"X mismatch at idx={i}"

            testCase "solveTriangularLinearSystems: upper-triangular backward substitution (2x2, 2 columns)" <| fun _ ->
                // Let K= U= [ [2,3], [0,4] ]
                // We'll define X => [ [1,10],[2,4] ]
                // Then B= K*X => 2x2
                // Let's compute B carefully:
                // B row0 => K[0,0]*X[0,*] + K[0,1]*X[1,*] => 2*(1,10)+3*(2,4) => (2+6,20+12)= (8,32)
                // B row1 => K[1,0]*X[0,*] + K[1,1]*X[1,*] => 0*(1,10)+4*(2,4) => (8,16)
                // => B= [ (8,32),(8,16) ]
                let Kdata = [|2.0;3.0; 0.0;4.0|] // 2x2 upper
                let X_expected_data = [| 1.0; 10.0
                                         2.0; 4.0  |] // shape=2x2
                let B_data = [| 8.0; 32.0
                                8.0; 16.0 |]
                let Kmat = Matrix(2,2,Kdata)
                let Bmat = Matrix(2,2,B_data)
                let Xsol = LinearAlgebra.solveTriangularLinearSystems Kmat Bmat false // isLower=false => backward sub
                for i in 0..3 do
                    Expect.floatClose Accuracy.high X_expected_data.[i] Xsol.Data.[i] $"X mismatch at idx={i}"

            // =====================================================================
            // solveTriangularLinearSystem
            // =====================================================================
            testCase "solveTriangularLinearSystem: dimension mismatch => throws" <| fun _ ->
                // K=2x2, v= length=3 => mismatch
                let K = Matrix(2,2,[|2.0;0.0; 1.0;2.0|])
                let v = [|1.0;2.0;3.0|]
                Expect.throwsT<ArgumentException> (fun () ->
                    let _ = LinearAlgebra.solveTriangularLinearSystem K v true
                    ()
                ) "Should throw dimension mismatch"

            testCase "solveTriangularLinearSystem: lower triangular forward substitution (2x2 single system)" <| fun _ ->
                // K= L= [ [2,0],[1,2] ]
                // We'll define a single x => [1,2], compute b => K*x, then see if we get x back.
                //
                // For x => [1.,2.],
                // B row0 => 2*1 +0*2=2
                // B row1 => 1*1 +2*2=1+4=5
                // => b= [2,5]
                let Kdata = [|2.0;0.0; 1.0;2.0|]
                let Kmat = Matrix(2,2,Kdata)
                let xExpected = [|1.0;2.0|]
                let b = [|2.0;5.0|]
                // Solve K*x=b
                let xSol = LinearAlgebra.solveTriangularLinearSystem Kmat b true
                Expect.floatClose Accuracy.high xExpected.[0] xSol.[0] "x[0] mismatch"
                Expect.floatClose Accuracy.high xExpected.[1] xSol.[1] "x[1] mismatch"

            testCase "solveTriangularLinearSystem: upper triangular backward substitution (2x2 single system)" <| fun _ ->
                // K= U= [ [2,3],[0,4] ]
                // Let x => [1,2]. Then b => K*x => 
                // row0 => 2*1 +3*2=2+6=8
                // row1 => 0*1 +4*2=8
                // => b= [8,8]
                let Kdata = [|2.0;3.0; 0.0;4.0|]
                let Kmat = Matrix(2,2,Kdata)
                let xExpected = [|1.0;2.0|]
                let b = [|8.0;8.0|]
                let xSol = LinearAlgebra.solveTriangularLinearSystem Kmat b false
                Expect.floatClose Accuracy.high xExpected.[0] xSol.[0] "x[0] mismatch"
                Expect.floatClose Accuracy.high xExpected.[1] xSol.[1] "x[1] mismatch"

        ]

        testList "Additional Linear Algebra Tests" [

            // =====================================================================
            // 1) qrDecompose
            // =====================================================================
            testCase "qrDecompose: Q*R ~ A for a small 3x2" <| fun _ ->
                // A=3x2 => [ [1.,2.],[3.,4.],[5.,6.] ]
                let Adata = [|1.;2.; 3.;4.; 5.;6.|]
                let A = Matrix(3, 2, Adata)

                let Q, R = LinearAlgebra.qrDecompose A
                // Q is 3x3 in the code above since we use the identity(3) initially,
                // but only the first 2 columns or some partial region might be relevant.
                // We'll do a truncated multiply to compare or do a naive Q*R if R is 3x2.

                // Let's do a direct approach: A' = Q * R (both are 3x2 effectively, if R is 3x2).
                // If R is the same shape as A, it's 3x2 => multiply 3x3 * 3x2 isn't well-defined.
                // But the code as posted doesn't necessarily do the typical Householder approach
                // that modifies R to upper triangular shape. It's more of a placeholder.
                // We'll do a "Compare shape" approach, then do a partial multiply in any case.

                let APrimeData = Array.zeroCreate<float> (3*2)
                // We'll assume R is 3x2. Then Q is 3x3 => Q(3x3)*R(3x2)= A'(3x2).
                for i in 0..2 do
                    for j in 0..1 do
                        let mutable sum = 0.0
                        for k in 0..2 do
                            sum <- sum + Q.[i,k] * R.[k,j]
                        APrimeData.[i*2 + j] <- sum

                // Compare A' with A
                for idx in 0..5 do
                    Expect.floatClose Accuracy.high Adata.[idx] APrimeData.[idx]
                        $"A' vs A mismatch at idx={idx}"

            // =====================================================================
            // 2) leastSquares
            // =====================================================================
            testCase "leastSquares: overdetermined system 3x2" <| fun _ ->
                // We'll create a design matrix A=3x2, b=3
                //   A= [ [1.,2.],
                //         [2.,1.],
                //         [3.,4.] ]
                // b= [ [8.],[10.],[19.] ]
                // We want to solve min ||A x - b||^2 for x in R^2.
                // We'll pick a small example and do a naive check or compare with a known solution if possible.

                let Adata = [|1.;2.; 2.;1.; 3.;4.|]
                let A = Matrix(3, 2, Adata)
                let b = [|8.; 10.; 19.|]

                let x = LinearAlgebra.leastSquares A b
                // We only do a round-trip check: A*x ~ b in least-squares sense.
                // We'll compute A*x (3x2 * 2 => 3) and see if that is close to b.
                let Ax = 
                    let result = Array.zeroCreate 3
                    for i in 0..2 do
                        // row i => i*2, dot with x
                        let rowOffset = i*2
                        result.[i] <- Adata.[rowOffset]*x.[0] + Adata.[rowOffset+1]*x.[1]
                    result

                // Compare Ax to b
                for i in 0..2 do
                    // Because it's least squares, we might not get an exact match if system is not exactly consistent.
                    // But let's see if it's close.
                    Expect.floatClose Accuracy.high b.[i] Ax.[i] $"A*x mismatch at row {i}"

            // =====================================================================
            // 3) cholesky
            // =====================================================================
            testCase "cholesky: factor a positive-definite 3x3" <| fun _ ->
                // We'll define A => 3x3 symmetric positive definite.
                // E.g. A= [ [4, 12, -16],
                //           [12,37, -43],
                //           [-16, -43, 98] ]
                // Classic example => L => [ [2,0,0], [6,1,0], [-8,5,3] ]
                let Adata = [|
                    4.0;  12.0; -16.0;
                    12.0; 37.0; -43.0;
                   -16.0; -43.0;  98.0; |]
                let A = Matrix(3,3,Adata)
                let L = LinearAlgebra.cholesky A
                // Compare with known L
                let Lexpected = [| 2.0; 0.0; 0.0;
                                   6.0; 1.0; 0.0;
                                  -8.0; 5.0; 3.0; |]
                for i in 0..8 do
                    Expect.floatClose Accuracy.high Lexpected.[i] L.Data.[i] $"L mismatch at idx {i}"

            testCase "cholesky: throws if matrix is not square" <| fun _ ->
                let nonsquare = Matrix(2,3, [|1.0;2.0;3.0;4.0;5.0;6.0|])
                Expect.throwsT<ArgumentException> (fun () ->
                    let _ = LinearAlgebra.cholesky nonsquare
                    ()
                ) "Should throw if not square"

            // =====================================================================
            // 4) leastSquaresCholesky
            // =====================================================================
            testCase "leastSquaresCholesky: small example" <| fun _ ->
                // We'll do the same design matrix from earlier, which is 3x2, and b= length=3.
                let Adata = [|1.;2.; 2.;1.; 3.;4.|]
                let A = Matrix(3, 2, Adata)
                let b = [|8.; 10.; 19.|]

                let x = LinearAlgebra.leastSquaresCholesky A b
                // We'll do the same "A*x ~ b" check as above
                let Ax = 
                    let result = Array.zeroCreate 3
                    for i in 0..2 do
                        let rowOffset = i*2
                        result.[i] <- Adata.[rowOffset]*x.[0] + Adata.[rowOffset+1]*x.[1]
                    result
                for i in 0..2 do
                    Expect.floatClose Accuracy.high b.[i] Ax.[i] $"A*x mismatch at row {i}"

            // =====================================================================
            // 5a) hatMatrix
            // =====================================================================
            testCase "hatMatrix: check if H = Q1 Q1^T for a small design matrix" <| fun _ ->
                // We'll do design=3x2 => same as above
                let Adata = [|1.;2.; 2.;1.; 3.;4.|]
                let A = Matrix(3, 2, Adata)
                let H = LinearAlgebra.hatMatrix A
                // We expect H to be 3x3. We can do a partial check:
                //   H ~ Q1 Q1^T, where Q1 is from the Householder-based QR in the code.
                // We'll check shape => should be 3x3, plus we can check if H^2=H (idempotent property).
                Expect.equal (H.NumRows, H.NumCols) (3,3) "Hat matrix is 3x3"

                // Quick idempotence test: H^2 ~ H
                let H2data = Array.zeroCreate<float> (3*3)
                for i in 0..2 do
                    for j in 0..2 do
                        let mutable sum = 0.0
                        for k in 0..2 do
                            sum <- sum + H.[i,k] * H.[k,j]
                        H2data.[i*3 + j] <- sum

                for idx in 0..8 do
                    Expect.floatClose Accuracy.high H.Data.[idx] H2data.[idx] $"Hat matrix not idempotent at idx={idx}"

            // =====================================================================
            // 5b) leverageBy and leverage
            // =====================================================================
            testCase "leverageBy: diagonal of H" <| fun _ ->
                // We'll reuse the hat matrix from above, check if leverageBy returns the diagonal
                let Adata = [|1.;2.; 2.;1.; 3.;4.|]
                let A = Matrix(3, 2, Adata)
                let H = LinearAlgebra.hatMatrix A
                let diag = LinearAlgebra.leverageBy H
                // Compare with Matrix.getDiagonal
                let diag2 = Matrix.getDiagonal H
                Expect.equal diag diag2 "Expected same diagonal from leverageBy"

            testCase "leverage: direct Q approach" <| fun _ ->
                // We'll see if leverage designMatrix ~ diagonal of H
                let Adata = [|1.;2.; 2.;1.; 3.;4.|]
                let A = Matrix(3, 2, Adata)
                // direct approach => Q1 from qrDecompose
                let directLever = LinearAlgebra.leverage A
                // The same as leverageBy (hatMatrix's diagonal)
                let H = LinearAlgebra.hatMatrix A
                let diagH = Matrix.getDiagonal H
                Expect.equal directLever diagH "Leverage mismatch"

            // =====================================================================
            // 6) luDecompose
            // =====================================================================
            testCase "luDecompose: pivot array, L and U multiply back to P*A" <| fun _ ->
                // A= 3x3 => we'll pick something that requires row swaps
                // A= [ [0,2,1],[3,4,5],[1,2,3] ]
                let Adata = [|0.;2.;1.; 3.;4.;5.; 1.;2.;3.|]
                let A = Matrix(3,3,Adata)
                let (P, L, U) = LinearAlgebra.luDecompose A
                // Check dimension => L, U => 3x3, P => perm array
                Expect.equal (L.NumRows, L.NumCols) (3,3) "L shape"
                Expect.equal (U.NumRows, U.NumCols) (3,3) "U shape"

                // We'll see if P*A= L*U
                // 1) build PA => permute rows of A by P => do a naive approach or if you have a function
                let PA = Matrix.permuteRowsBy P A
                // 2) multiply L,U => LU
                let LUdata = Array.zeroCreate<float> (3*3)
                for i in 0..2 do
                    for j in 0..2 do
                        let mutable sum = 0.0
                        for k in 0..2 do
                            sum <- sum + L.[i,k]*U.[k,j]
                        LUdata.[i*3 + j] <- sum

                // Compare LU vs PA
                for idx in 0..8 do
                    Expect.floatClose Accuracy.high PA.Data.[idx] LUdata.[idx] $"LU mismatch at idx={idx}"

            // =====================================================================
            // 7) solveLinearSystems
            // =====================================================================
            testCase "solveLinearSystems: A is 2x2, B=2x2" <| fun _ ->
                // We'll define A => [ [2,3],[1,2] ],
                // B => 2x2 => pick X => 2x2 => solve => see if we get X
                // X => [ [1,10],[2,4] ] => B= A*X => let's do the multiply:
                // row0 => (2,3)*X => 2*(1,10)+3*(2,4)= (2+6,20+12)= (8,32)
                // row1 => (1,2)*X => 1*(1,10)+2*(2,4)= (1+4,10+8)= (5,18)
                // => B => [ (8,32),(5,18) ]
                let Adata = [|2.;3.; 1.;2.|]
                let Bdata = [|8.;32.; 5.;18.|]
                let A = Matrix(2,2,Adata)
                let B = Matrix(2,2,Bdata)
                let Xsol = LinearAlgebra.solveLinearSystems A B
                let Xexpected = [|1.;10.; 2.;4.|]
                for i in 0..3 do
                    Expect.floatClose Accuracy.high Xexpected.[i] Xsol.Data.[i] $"X mismatch at idx={i}"

            // =====================================================================
            // 8) solveLinearSystem
            // =====================================================================
            testCase "solveLinearSystem: 3x3 single system" <| fun _ ->
                // A => [ [0,2,1],[3,4,5],[1,2,3] ] from above, b => let's pick x => [1,2,3], compute b => A*x
                // row0 => 0*1 +2*2 +1*3= 4+3=7
                // row1 => 3*1 +4*2 +5*3= 3+8+15=26
                // row2 => 1*1 +2*2 +3*3= 1+4+9=14
                let Adata = [|0.;2.;1.; 3.;4.;5.; 1.;2.;3.|]
                let A = Matrix(3,3,Adata)
                let b = [|7.;26.;14.|]
                let xSol = LinearAlgebra.solveLinearSystem A b
                let expected = [|1.;2.;3.|]
                for i in 0..2 do
                    Expect.floatClose Accuracy.high expected.[i] xSol.[i] $"x mismatch at {i}"

            // =====================================================================
            // 9) inverse
            // =====================================================================
            testCase "inverse: 2x2 inverse" <| fun _ ->
                // A => [ [1,2],[3,4] ], inverse => [ [-2,1],[1.5, -0.5] ]
                // because 1*4 -2*3= (4-6)= -2 => A^-1 => 1/det * [ [4,-2],[-3,1] ] => 1/-2 => [ [-2,1],[1.5,-0.5] ]
                let Adata = [|1.;2.; 3.;4.|]
                let A = Matrix(2,2,Adata)
                let Ainverse = LinearAlgebra.inverse A
                // Compare with known
                let expected = [| -2.0; 1.0; 1.5; -0.5 |]
                for i in 0..3 do
                    Expect.floatClose Accuracy.high expected.[i] Ainverse.Data.[i] $"Inverse mismatch at idx={i}"

            testCase "inverse: dimension mismatch => throws" <| fun _ ->
                let nonsquare = Matrix(2,3,[|1.;2.;3.;4.;5.;6.|])
                Expect.throwsT<ArgumentException> (fun () ->
                    let _ = LinearAlgebra.inverse nonsquare
                    ()
                ) "Should throw if not square"
        ]




    ]
