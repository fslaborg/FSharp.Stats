module MatrixTests

open System
open FSharp.Stats
open Expecto


[<Tests>]
let matrixTests =
    testList "Matrix Tests" [

        // ----------------------------------------------------------------------
        // Basic construction 
        // ----------------------------------------------------------------------
        testCase "ofJaggedArray creates matrix from jagged array" <| fun _ ->
            let jagged = 
                [| [|0.0; 0.0|]
                   [|1.0; 3.0|]
                   [|2.0; 4.0|] |]
            let mat = Matrix.ofJaggedArray jagged  // We specifically test 'ofJaggedArray' here
            Expect.equal mat.NumRows 3 "Matrix should have 3 rows"
            Expect.equal mat.NumCols 2 "Matrix should have 2 columns"
            Expect.equal mat.Data [|0.0; 0.0; 1.0; 3.0; 2.0; 4.0|]
                "Flattened data should match row-major flattening"

        testCase "ofArray2D creates matrix from 2D array" <| fun _ ->
            let arr2d = 
                array2D [ [ 1; 2 ]
                          [ 3; 4 ]
                          [ 5; 6 ] ]
            let mat = Matrix.ofArray2D arr2d
            Expect.equal mat.NumRows 3 "Matrix should have 3 rows"
            Expect.equal mat.NumCols 2 "Matrix should have 2 columns"
            Expect.equal mat.Data [|1; 2; 3; 4; 5; 6|] "Flattened data should match row-major order"

        testCase "init creates a matrix using a function of row,col" <| fun _ ->
            let mat = Matrix.init 2 3 (fun r c -> float (r + c))
            // For r=0, c=0..2 => [0;1;2]
            // For r=1, c=0..2 => [1;2;3]
            // Flatten => [0;1;2; 1;2;3]
            let expected = [|0.0; 1.0; 2.0; 1.0; 2.0; 3.0|]
            Expect.equal mat.NumRows 2 "Rows = 2"
            Expect.equal mat.NumCols 3 "Cols = 3"
            Expect.equal mat.Data expected "Data should match the function r+c"

        testCase "ofJaggedArray throws on non-rectangular jagged input" <| fun _ ->
            let jagged = 
                [| [|1.0; 2.0|]
                   [|3.0|] |]  // 2nd row has length=1, first row has length=2
            Expect.throws (fun () -> Matrix.ofJaggedArray jagged |> ignore)
                "Should throw due to inconsistent row lengths"

        // ----------------------------------------------------------------------
        // Access & Slicing
        // ----------------------------------------------------------------------
        testCase "indexer get/set within bounds" <| fun _ ->
            let mat = matrix [| [|1.0; 2.0|]
                                [|3.0; 4.0|] |]
            Expect.equal mat.[0,0] 1.0 "Element (0,0) is 1.0"
            mat.[1,1] <- 99.0
            Expect.equal mat.[1,1] 99.0 "Element (1,1) updated to 99.0"

        testCase "indexer throws on out-of-range access" <| fun _ ->
            let mat = matrix [| [|1.0; 2.0|]
                                [|3.0; 4.0|] |]
            Expect.throwsT<ArgumentException> (fun () -> let _ = mat.[999, 999] in ())
                "Indexer out of range should throw"

        testCase "GetSlice works with row and col start..end" <| fun _ ->
            // 3x3 matrix
            let mat = matrix [|
                [|1.0; 2.0; 3.0|]
                [|4.0; 5.0; 6.0|]
                [|7.0; 8.0; 9.0|]
            |]
            // slice submatrix: rows [0..1], cols [1..2]
            let subMat = mat.[0..1, 1..2]
            // This submatrix should be:
            //   [ [2.0; 3.0]
            //     [5.0; 6.0] ]
            Expect.equal subMat.NumRows 2 "2 rows"
            Expect.equal subMat.NumCols 2 "2 cols"
            Expect.equal subMat.Data [|2.0; 3.0; 5.0; 6.0|] "Flattened submatrix data"

        testCase "GetSlice throws on invalid slice range" <| fun _ ->
            let mat = matrix [| [|1.;2.|]
                                [|3.;4.|] |]
            Expect.throwsT<ArgumentException> (fun () -> mat.[0..99, *] |> ignore)
                "Should throw due to invalid row end"

        // ----------------------------------------------------------------------
        // Arithmetic (element-wise) 
        // ----------------------------------------------------------------------
        testCase "add: element-wise addition of same-dimension matrices" <| fun _ ->
            let m1 = matrix [| [|1.; 2.|]
                               [|3.; 4.|] |]
            let m2 = matrix [| [|10.;20.|]
                               [|30.;40.|] |]
            let result = Matrix.add m1 m2
            let expected = [| (1.+10.); (2.+20.); (3.+30.); (4.+40.) |]  // => [11;22;33;44]
            Expect.equal result.Data expected "Element-wise addition result"

        testCase "subtract: dimension mismatch throws" <| fun _ ->
            let m1 = matrix [| [|1.;2.|] |]      // 1x2
            let m2 = matrix [| [|1.;2.;3.|] |] // 1x3
            Expect.throwsT<ArgumentException> (fun () -> Matrix.subtract m1 m2 |> ignore)
                "Subtract must throw if shapes differ"

        testCase "multiply & divide: element-wise" <| fun _ ->
            let m1 = matrix [| [|2.;4.|]
                               [|6.;8.|] |]
            let m2 = matrix [| [|1.;2.|]
                               [|3.;4.|] |]

            // multiply => [ [2.*1. , 4.*2.] ; [6.*3., 8.*4.] ] => [ [2.,8.]; [18.,32.] ]
            let mulResult = Matrix.multiply m1 m2
            Expect.equal mulResult.Data [|2.; 8.; 18.; 32.|] "Element-wise multiply"

            // divide => [ [2./1., 4./2.]; [6./3., 8./4.] ] => [ [2.,2.]; [2.,2.] ]
            let divResult = Matrix.divide m1 m2
            Expect.equal divResult.Data [|2.;2.;2.;2.|] "Element-wise divide"


        // ----------------------------------------------------------------------
        // Standard matrix multiplication
        // ----------------------------------------------------------------------
        testCase "matmul: 2x3 times 3x2 => 2x2 result" <| fun _ ->
            // A=2x3, B=3x2
            let A = matrix [| [|1.0; 2.0; 3.0|]
                              [|4.0; 5.0; 6.0|] |]
            let B = matrix [| [|7.0;  10.0|]
                              [|8.0;  11.0|]
                              [|9.0;  12.0|] |]

            // Expected:
            // C[0,0] = 1*7 + 2*8 + 3*9   = 50
            // C[0,1] = 1*10+2*11+3*12  = 68
            // C[1,0] = 4*7 + 5*8 + 6*9  = 122
            // C[1,1] = 4*10+5*11+6*12 = 167
            let C = Matrix.matmul A B
            Expect.equal C.NumRows 2 "Should have 2 rows"
            Expect.equal C.NumCols 2 "Should have 2 cols"
            Expect.equal C.Data [|50.0; 68.0; 122.0; 167.0|] "Check standard matmul result"


        // ----------------------------------------------------------------------
        // Scalar Operations
        // ----------------------------------------------------------------------
        testCase "addScalar, subtractScalar, multiplyScalar, divideScalar" <| fun _ ->
            let m = matrix [| [|1.;2.|]
                              [|3.;4.|] |]
            let addRes = Matrix.addScalar m 10.0
            Expect.equal addRes.Data [|11.;12.;13.;14.|] "Add 10.0 to all"

            let subRes = Matrix.subtractScalar m 1.0
            Expect.equal subRes.Data [|0.;1.;2.;3.|] "Subtract 1.0 from all"

            let mulRes = Matrix.multiplyScalar m 2.0
            Expect.equal mulRes.Data [|2.;4.;6.;8.|] "Multiply all by 2.0"

            let divRes = Matrix.divideScalar m 2.0
            Expect.equal divRes.Data [|0.5;1.;1.5;2.|] "Divide all by 2.0"

        // ----------------------------------------------------------------------
        // Matrix-Vector Multiply
        // ----------------------------------------------------------------------
        testCase "m * v => standard matrix-vector product" <| fun _ ->
            // 2x3 times vector of length 3 => vector of length 2
            // mat = [ [1.,2.,3.]
            //         [4.,5.,6.] ]
            let mat = matrix [| [|1.;2.;3.|]
                                [|4.;5.;6.|] |]
            let v = [| 10.; 20.; 30. |]
            // result = [
            //   row0 dot v => (1.*10. + 2.*20. + 3.*30.) = 140.
            //   row1 dot v => (4.*10. + 5.*20. + 6.*30.) = 320.
            // ]
            let result = mat * v
            Expect.equal result [|140.; 320.|] "Matrix-vector product"

        testCase "v * m => row-vector times matrix => vector" <| fun _ ->
            // 1x2 row vector times a 2x2 matrix => 1x2 vector
            let v = [|2.; 3.|]  // length=2
            let mat = matrix [| [|10.; 100.|]
                                [|20.; 200.|] |]
            // v*m => [ (2.*10. + 3.*20.),  (2.*100. + 3.*200.) ]
            //       => [ 2.*10. + 3.*20. , 2.*100. + 3.*200. ]
            //       => [ 20.+60., 200.+600. ] => [80., 800.]
            let result = v * mat
            Expect.equal result [|80.; 800.|] "Row-vector times matrix"

        testCase "m * v => dimension mismatch throws" <| fun _ ->
            let mat = matrix [| [|1.;2.|]
                                [|3.;4.|] |] // 2x2
            let v = [|1.;2.;3.|] // length=3
            Expect.throwsT<ArgumentException> (fun () -> let _ = mat * v in ())
                "Matrix(2x2)*Vector(3) => dimension mismatch should throw"

        // ----------------------------------------------------------------------
        // Transpose
        // ----------------------------------------------------------------------
        testCase "Transpose: changes shape and flips row<->col" <| fun _ ->
            let mat = matrix [| [|1.;2.;3.|]
                                [|4.;5.;6.|] |] // 2x3
            let t = mat.Transpose()
            Expect.equal t.NumRows 3 "Should have 3 rows"
            Expect.equal t.NumCols 2 "Should have 2 cols"
            Expect.equal t.Data [|1.;4.;2.;5.;3.;6.|] "Flattened transpose"

        // ----------------------------------------------------------------------
        // Identity, Diagonal, and Zero/Ones
        // ----------------------------------------------------------------------
        testCase "identity n => NxN identity matrix" <| fun _ ->
            let eye = Matrix.identity<float> 3
            // => [1,0,0; 0,1,0; 0,0,1]
            let expected = [|1.;0.;0.; 0.;1.;0.; 0.;0.;1.|]
            Expect.equal eye.Data expected "3x3 identity"

        testCase "diagonal => builds NxN from diag vector" <| fun _ ->
            let diagVec = [|10.; 20.; 30.|]
            let mat = Matrix.diagonal diagVec
            // => [ [10, 0, 0],
            //      [ 0,20, 0],
            //      [ 0, 0,30] ]
            Expect.equal mat.NumRows 3 "3x3"
            Expect.equal mat.NumCols 3 "3x3"
            let expected = [|10.;0.;0.; 0.;20.;0.; 0.;0.;30.|]
            Expect.equal mat.Data expected "Diagonal in main diagonal positions"

        testCase "ones => NxM matrix of all 1's" <| fun _ ->
            let mat = Matrix.ones<float> 2 3
            Expect.equal mat.Data [|1.;1.;1.;1.;1.;1.|] "2x3 of all 1."

        testCase "zeroCreate => NxM matrix of all 0's" <| fun _ ->
            let mat = Matrix.zeroCreate<float> 2 3
            Expect.equal mat.Data [|0.;0.;0.;0.;0.;0.|] "2x3 of all zero."

        // ----------------------------------------------------------------------
        // getRow / getCol 
        // ----------------------------------------------------------------------
        testCase "getRow gets row i as a vector" <| fun _ ->
            let mat = matrix [| [|1.;2.|]
                                [|3.;4.|] |]
            let row0 = Matrix.getRow 0 mat
            let row1 = Matrix.getRow 1 mat
            Expect.equal row0 [|1.;2.|] "Row 0"
            Expect.equal row1 [|3.;4.|] "Row 1"

        testCase "getCol gets column j as a vector" <| fun _ ->
            let mat = matrix [| [|1.;2.;3.|]
                                [|4.;5.;6.|] |]
            let col0 = Matrix.getCol 0 mat
            let col2 = Matrix.getCol 2 mat
            Expect.equal col0 [|1.;4.|] "Column 0"
            Expect.equal col2 [|3.;6.|] "Column 2"

        // ----------------------------------------------------------------------
        // Equality (IEquatable)
        // ----------------------------------------------------------------------
        testCase "Equals returns true for same shape+elements" <| fun _ ->
            let m1 = matrix [| [|1.;2.|]
                               [|3.;4.|] |]
            // 'Matrix.create' constructs from a flattened array
            let m2 = Matrix.create<float> 2 2 [|1.;2.;3.;4.|]
            Expect.isTrue (m1.Equals m2) "Matrices should be equal"

        testCase "Equals returns false for dimension mismatch" <| fun _ ->
            let m1 = matrix [| [|1.;2.|] |]    // 1x2
            let m2 = matrix [| [|1.;2.;3.|] |] // 1x3
            Expect.isFalse (m1.Equals m2) "Should be different shapes => not equal"

        testCase "Equals returns false for data difference" <| fun _ ->
            let m1 = matrix [| [|1.;2.|] |] 
            let m2 = matrix [| [|1.;9.|] |] 
            Expect.isFalse (m1.Equals m2) "Same shape, different data => not equal"

        // ----------------------------------------------------------------------
        // Misc: toArray2D, toJaggedArray
        // ----------------------------------------------------------------------
        testCase "toArray2D transforms a matrix into 2D array" <| fun _ ->
            let mat = matrix [| [|1.;2.|]
                                [|3.;4.|]
                                [|5.;6.|] |]
            let arr2d = mat.toArray2D()
            Expect.equal (arr2d.GetLength(0)) 3 "3 rows in arr2d"
            Expect.equal (arr2d.GetLength(1)) 2 "2 cols in arr2d"
            Expect.equal arr2d.[2,1] 6.0 "Check element"

        testCase "toJaggedArray transforms a matrix into jagged array" <| fun _ ->
            let mat = matrix [| [|7.;8.|]
                                [|9.;10.|] |]
            let jagged = mat.toJaggedArray()
            Expect.equal jagged.Length 2 "2 rows in jagged"
            Expect.equal jagged.[1] [|9.;10.|] "Second row matches"

        // ----------------------------------------------------------------------
        // Slicing & Setting Row
        // ----------------------------------------------------------------------
        testCase "SetRow updates row i" <| fun _ ->
            let mat = matrix [| [|1.;2.|]
                                [|3.;4.|] |]
            mat.SetRow(0, [|10.;20.|])
            Expect.equal mat.Data [|10.;20.;3.;4.|] "Row 0 replaced"

        // (Optional) More tests can be added for addRowVector, addColVector, etc.
    ]



