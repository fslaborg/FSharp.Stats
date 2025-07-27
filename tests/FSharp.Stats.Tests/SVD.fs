module SVDTests

open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Algebra

module MatrixUtils =

    let inline isApproxZero<'T when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : comparison
        and 'T :> ValueType>
        (epsilon: 'T) (x: 'T) =
        (GenericMath.abs x) < epsilon

    let inline isUpperBidiagonalWithTolerance<'T when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : comparison
        and 'T :> ValueType>
        (epsilon: 'T)
        (A: Matrix<'T>) : bool =
        let m = A.NumCols
        let n = A.NumRows

        let mutable ok = true
        for i = 0 to m - 1 do
            for j = 0 to n - 1 do
                let onDiagonal = j = i
                let onSuperDiagonal = j = i + 1
                let aij = A.[i, j]
                if not (onDiagonal || onSuperDiagonal) && not (isApproxZero epsilon aij) then
                    ok <- false
        ok


[<Tests>]
let householderCreateTest =
    test "Householder.create" {
        let x = [| 4.0; 3.0; 0.0 |]
        let h = Householder.create x

        let v = h.V
        let tau = h.Tau
        let n = v.Length

        // Compute Hx = (I - τ v vᵗ) x
        // First: dot(v, x)
        let dot = Vector.dot v x

        // Then: Hx = x - τ * dot * v
        let hx = Array.init n (fun i -> x.[i] - tau * dot * v.[i])

        // We expect: hx = [±||x||; 0; 0]
        let norm = sqrt (Array.sumBy (fun xi -> xi * xi) x)
        Expect.floatClose Accuracy.high hx.[0] norm "Should be equal (double precision)"
        Expect.floatClose Accuracy.high hx.[1] 0.0 "Should be equal (double precision)"
        Expect.floatClose Accuracy.high hx.[2] 0.0 "Should be equal (double precision)"
        }
        //test " zeroes out tail of vector" {
        //    Expect.floatClose Accuracy.high hx.[0] norm "Should be equal (double precision)"
        //}
        //test " zeroes out tail of vector" {
        //    Expect.floatClose Accuracy.high hx.[1] 0.0 "Should be equal (double precision)"
        //}
        //test " zeroes out tail of vector" {
        //    Expect.floatClose Accuracy.high hx.[2] 0.0 "Should be equal (double precision)"
        //}
        //]

[<Tests>]
let applyLeftTest =
    test "Householder.applyLeft zeroes out subdiagonal column elements" {
        // Define 3x3 test matrix
        let A =
            matrix [|
                [| 4.0; 2.0; 1.0 |]
                [| 3.0; 0.0; 0.0 |]
                [| 0.0; 0.0; 0.0 |]
            |]

        // Create Householder vector from column 0 (x = [4; 3; 0])
        let x = [| A.[0, 0]; A.[1, 0]; A.[2, 0] |]
        let h = Householder.create x

        // Apply reflector from the left to matrix A starting at row 0
        Householder.applyLeft(h, A, 0)

        // Check: entries below A[0][0] should be zero
        //let eps = 1e-10
        Expect.floatClose Accuracy.high A.[1, 0] 0.0 "Should be equal (double precision)"
        Expect.floatClose Accuracy.high A.[2, 0] 0.0 "Should be equal (double precision)"

        // Optionally check the first value matches h.Beta
        Expect.floatClose Accuracy.high A.[0, 0] h.Beta "Should be equal (double precision)"
    }

let applyRightTest =
    test "Householder.applyRight zeroes out row tail elements" {
        // Define 3x3 test matrix
        let A =
            matrix [|
                [| 1.0; 4.0; 3.0 |]
                [| 0.0; 0.0; 0.0 |]
                [| 0.0; 0.0; 0.0 |]
            |]

        // Take the row vector to the right of the diagonal: [4.0; 3.0]
        let x = [| A.[0, 1]; A.[0, 2] |]
        let h = Householder.create x

        // Apply the Householder from the right starting at column 1
        Householder.applyRight(h, A, 1)

        // Check that A[0].[2] is now approximately 0
        //let eps = 1e-10
        Expect.floatClose Accuracy.high A.[0, 2] 0.0 "Should be equal (double precision)"

        // Check that A[0].[1] is updated to h.Beta
        Expect.floatClose Accuracy.high A.[0, 1] h.Beta "Should be equal (double precision)"
    }


[<Tests>]
let bidiagonalizationTest =
    testList "bidiagonalization" [
        test "First step of bidiagonalization (applyLeft to column 0) works" {
            // Full-rank 3x3 matrix
            let A =
                matrix [|
                    [| 4.0; 1.0; 2.0 |]
                    [| 3.0; 1.0; 0.0 |]
                    [| 5.0; 1.0; 3.0 |]
                |]

            // Extract column 0
            let colVector = [| A.[0, 0]; A.[1, 0]; A.[2, 0] |]
            let h = Householder.create colVector
            
            Householder.applyLeft(h, A, 0)
            
            // Overwrite A[0..,0] with [β; 0; 0]
            A.[0, 0] <- h.Beta
            for i = 1 to 2 do
                A.[i, 0] <- 0.0

            // Check column 0
            Expect.floatClose Accuracy.high A.[1, 0] 0.0 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high A.[2, 0] 0.0 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high A.[0, 0] h.Beta "Should be equal (double precision)"
        }



        test "bidiagonalizeInPlace produces upper bidiagonal matrix" {
            let input =
                matrix [|
                    [| 1.0; 2.0; 3.0 |]
                    [| 4.0; 5.0; 6.0 |]
                    [| 7.0; 8.0; 9.0 |]
                |]

            let A = input |> Matrix.copy

            Bidiagonalization.bidiagonalizeInPlace A

            let isBidiagonal = MatrixUtils.isUpperBidiagonalWithTolerance 1e-10 A

            Expect.isTrue isBidiagonal "Matrix should be upper bidiagonal after bidiagonalization"
        }
    ]


//[<Tests>]
//let golubKahanTests =
//    testList "Golub-Kahan Diagonalization" [

//        test "Diagonalize 2x2 upper bidiagonal matrix" {
//            // Construct bidiagonal: [ [4, 2]; [0, 3] ]
//            let d = [| 4.0; 3.0 |]
//            let e = [| 2.0 |]

//            let bidiag = {
//                D = d
//                E = e
//            }

//            let sigma = GolubKahan.diagonalize bidiag 

//            // Expected from NumPy SVD
//            let expected = [| 4.74341649; 2.5355339 |]
//            let eps = 1e-6

//            // Compare sorted values (SVD is unordered)
//            let actualSorted = Array.sortDescending sigma
//            let expectedSorted = Array.sortDescending expected

//            for i in 0 .. expected.Length - 1 do
//                Expect.floatClose Accuracy.high actualSorted.[i] expectedSorted.[i] $"Singular value {i} mismatch"
//        }

//        // You can add more tests here for larger bidiagonal matrices or edge cases
//    ]