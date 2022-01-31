module MatrixTests
open Expecto

open FSharp.Stats
open FSharp.Stats.Matrix

let private testRowVecA =
    let values = [|1.;4.|]
    RowVector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testRowVecB =
    let values = [|0.;3.;6.|]
    RowVector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testRowVecC =
    let values = [|0.;3.;4.|]
    RowVector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testRowVecD =
    let values = [|0.;0.;0.|]
    RowVector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testVectorA =
    let values = [|0.;3.;6.|]
    Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testVectorB =
    let values = [|0.;0.;0.|]
    Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testVector1LowerDiag =
    let values = [|0.;5.|]
    Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testVector1UpperDiag =
    let values = [|1.;4.|]
    Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

let private testDiagonalMatrixA : Matrix<float> =
    let values =
        Array2D.init
            3
            3
            (fun i j ->
                if i = j then
                    testVectorA.[i]
                else 0.
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))


let private identity3Int : Matrix<int> =
    let values =
        Array2D.init
            3
            3
            (fun i j ->
                if i = j then 1 else 0
            )
    Matrix.DenseRepr
        (DenseMatrix<int>(Some (Instances.Int32Numerics :> INumeric<int>),values))
        

let private identityFloat3 =  
    let values =
        Array2D.init
            3
            3
            (fun i j ->
                if i = j then 1. else 0.
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))


let private testValuesArrRows =
    [|
        [|0.;1.;2.|]
        [|0.;3.;4.|]
        [|0.;5.;6.|]
    |]

let private testValuesArrCols =
    [|
        [|0.;0.;0.|]
        [|1.;3.;5.|]
        [|2.;4.;6.|]
    |]

let private testValues2x3 =
    [|
        [|0.;0.;0.|]
        [|1.;3.;5.|]
    |]

let private testValues2x3Transposed =
    [|
        [|0.;1.|]
        [|0.;3.|]
        [|0.;5.|]
    |]

let private testValues3x2 =
    [|
        [|0.;0.|]
        [|1.;3.|]
        [|2.;4.|]
    |]

let private testSquareMatrixA : Matrix<float> =
    let values =
        Array2D.init
            3
            3
            (fun i j ->
                testValuesArrRows.[i].[j]
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

let private testSquareMatrixB : Matrix<float> =
    let values =
        Array2D.init
            3
            3
            (fun i j ->
                testValuesArrCols.[i].[j]
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

            
let private test2x3Matrix : Matrix<float> =
    let values =
        Array2D.init
            2
            3
            (fun i j ->
                testValues2x3.[i].[j]
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

            
            
let private test2x3MatrixTransposed : Matrix<float> =
    let values =
        Array2D.init
            3
            2
            (fun i j ->
                testValues2x3Transposed.[i].[j]
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))


let private test3x2MatrixB : Matrix<float> =
    let values =
        Array2D.init
            3
            2
            (fun i j ->
                testValues3x2.[i].[j]
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

let private testConstDiagMatrix : Matrix<float> =
    let values =
        Array2D.init
            3
            3
            (fun i j ->
                if i = j then 3. else 0.
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

let private testConstMatrix : Matrix<float> =
    let values =
        Array2D.init
            3
            3
            (fun i j ->
                3.
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

let private testScalarMatrix : Matrix<float> =
    let values =
        Array2D.init
            1
            1
            (fun i j ->
                3.
            )
    Matrix.DenseRepr
        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

[<Tests>]
let genericImplementationTests = 
    testList "Matrix.GenericImplementation" [
        //TO-DO: cover generic implementation here, using another numeric then float
        testCase "" <| fun () ->
            ()
    ]
    
[<Tests>]
let floatImplementationSparseTests =
    testList "Matrix.FloatImplementation.Sparse" [
        //TO-DO: adapt all dense tests for sparse matrices
        testCase "" <| fun () ->
            ()
    ]

[<Tests>]
let floatImplementationDenseTests =
    testList "Matrix.FloatImplementation.Dense" [
        //Tests for acessing and setting values in the underlying array2D
        testList "Acessors" [

            testCase "Get value" <| fun () ->
                let actual = Matrix.get testSquareMatrixA 0 1
                Expect.equal actual 1. "Matrix.get returned wrong value"

            testCase "Getting value out of range should fail" <| fun () ->
                Expect.throws (fun () -> Matrix.get testSquareMatrixA 0 7 |> ignore) "Getting value out of range should fail"

            testCase "Set value" <| fun () ->

                let actual =
                    Matrix.copy testSquareMatrixA
                Matrix.set actual 0 0 1337.

                let expected = 
                    [
                        [1337.;1.;2.]
                        [0.;3.;4.]
                        [0.;5.;6.]
                    ]
                    |> matrix

                Expect.equal actual expected "Matrix.set mutated the wrong value"

            testCase "Setting value out of range should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.set (Matrix.copy testSquareMatrixA) 0 7 3. |> ignore) "Getting value out of range should fail"
        ]

        testList "Creation" [

            testCase "init" <| fun () ->
                let actual =
                    Matrix.init 3 3 (fun i j -> testValuesArrRows.[i].[j])
                Expect.equal actual testSquareMatrixA "Matrix was not initialized correctly using Matrix.init"
                
            testCase "ofRows" <| fun () ->
                let actual =
                    testValues2x3
                    |> Array.map rowvec
                    |> Vector.Generic.ofSeq
                    |> Matrix.ofRows

                Expect.equal actual test2x3Matrix "Matrix was not initialized correctly using Matrix.ofRows"
            
            testCase "ofCols" <| fun () ->
                let actual =
                    testValues2x3Transposed
                    |> Array.map vector
                    |> RowVector.Generic.ofSeq
                    |> Matrix.ofCols

                Expect.equal actual test2x3Matrix "Matrix was not initialized correctly using Matrix.ofCols"      
                
            testCase "ofJaggedList" <| fun () ->

                let actual =
                    testValues2x3
                    |> List.ofArray
                    |> List.map List.ofArray
                    |> Matrix.ofJaggedList

                Expect.equal actual test2x3Matrix "Matrix was not initialized correctly using Matrix.ofJaggedList"

            testCase "ofJaggedColList" <| fun () ->
                let actual =
                    testValues2x3Transposed
                    |> List.ofArray
                    |> List.map List.ofArray
                    |> Matrix.ofJaggedColList

                Expect.equal actual test2x3Matrix "Matrix was not initialized correctly using Matrix.ofJaggedColList"


            testCase "ofJaggedSeq" <| fun () ->
                let actual =
                    testValues2x3
                    |> Seq.ofArray
                    |> Seq.map Seq.ofArray
                    |> Matrix.ofJaggedSeq

                Expect.equal actual test2x3Matrix "Matrix was not initialized correctly using Matrix.ofJaggedSeq"


            testCase "ofJaggedColSeq" <| fun () ->
                let actual =
                    testValues2x3Transposed
                    |> Seq.ofArray
                    |> Seq.map Seq.ofArray
                    |> Matrix.ofJaggedColSeq

                Expect.equal actual test2x3Matrix "Matrix was not initialized correctly using Matrix.ofJaggedColSeq"


            testCase "ofJaggedArray" <| fun () ->
                let actual =
                    testValues2x3
                    |> Matrix.ofJaggedArray

                Expect.equal actual test2x3Matrix "Matrix was not initialized correctly using Matrix.ofJaggedArray"


            testCase "ofJaggedColArray" <| fun () ->
                let actual =
                    testValues2x3Transposed
                    |> Matrix.ofJaggedColArray

                Expect.equal actual test2x3Matrix "Matrix was not initialized correctly using Matrix.ofJaggedColArray"


            testCase "diag" <| fun () ->
                let actual = Matrix.diag testVectorA

                Expect.equal actual testDiagonalMatrixA "Diagonal Matrix was not correctly initialized using Matrix.diag"

            testCase "initDiagonal" <| fun () ->

                let actual = Matrix.initDiagonal testVectorA
                    
                Expect.equal actual testDiagonalMatrixA "Diagonal Matrix was not correctly initialized using Matrix.initDiag"

            testCase "constDiag" <| fun () ->
                    
                let actual = Matrix.constDiag 3 3.

                Expect.equal actual testConstDiagMatrix "Constant diagonal matrix was not correctly initialized using Matrix.constDiag"


            testCase "create" <| fun () ->
                    
                let actual = Matrix.create 3 3 3.

                Expect.equal actual testConstMatrix "Constant matrix was not initialized correctly using Matrix.create"

            testCase "ofScalar" <| fun () ->
                    
                let actual = Matrix.ofScalar 3.

                Expect.equal actual testScalarMatrix "1x1 Matrix was not correctly initialized using Matrix.ofScalar"

            testCase "ofArray2D" <| fun () ->
                    
                let values =
                    Array2D.init
                        3
                        3
                        (fun i j ->
                            testValuesArrRows.[i].[j]
                        )

                let actual = Matrix.ofArray2D values

                Expect.equal actual testSquareMatrixA "Matrix was not initialized correctly using Matrix.ofArray2D"

            testCase "toArray2D" <| fun () ->

                let expected =
                    Array2D.init
                        3
                        3
                        (fun i j ->
                            testValuesArrRows.[i].[j]
                        )

                let actual = Matrix.toArray2D testSquareMatrixA

                Expect.equal actual expected "Matrix.toArray2D did not return the correct Array2D"

            testCase "toJaggedArray" <| fun () ->
                let actual = Matrix.toJaggedArray testSquareMatrixA
                Expect.equal actual testValuesArrRows "Matrix.toJaggedArray did not return the correct JaggedArray"

            testCase "toJaggedSeq" <| fun () ->
                let actual = Matrix.toJaggedSeq testSquareMatrixA |> JaggedArray.ofJaggedSeq
                Expect.equal actual testValuesArrRows "Matrix.toJaggedSeq did not return the correct JaggedSeq"
            
            testCase "toJaggedColArray" <| fun () ->
                let actual = Matrix.toJaggedColArray testSquareMatrixA
                Expect.equal actual testValuesArrCols "Matrix.toJaggedColArray did not return the correct JaggedArray"

            testCase "toJaggedColSeq" <| fun () ->
                let actual = Matrix.toJaggedColSeq testSquareMatrixA |> JaggedArray.ofJaggedSeq
                Expect.equal actual testValuesArrCols "Matrix.toJaggedColSeq did not return the correct JaggedSeq"


            testCase "getDiagN 1 above diagonal" <| fun () ->
                    
                let actual = Matrix.getDiagN testSquareMatrixA 1

                Expect.equal actual testVector1UpperDiag "Matrix.getDiagN did not return the correct offset +1 diagonal"

            testCase "getDiag 1 below diagonal" <| fun () ->

                let actual = Matrix.getDiagN testSquareMatrixA -1

                Expect.equal actual testVector1LowerDiag "Matrix.getDiagN did not return the correct offset -1 diagonal"

        ]

        testList "Operators" [

            testList "add" [

                testCase "Addition of 2 Matrices with the same dimensions" <| fun () ->
                        
                    let actual = Matrix.add testSquareMatrixA testSquareMatrixB

                    let expected =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    testValuesArrRows.[i].[j] + testValuesArrCols.[i].[j]
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))
                        
                    Expect.equal actual expected "Matrix.add did not add the values of two matrices with the same dimensions correctly"
                    
                testCase "Addition of matrices with different sizes should fail" <| fun () ->
                        
                    Expect.throws (fun () -> Matrix.add test2x3Matrix testSquareMatrixA |> ignore) "Addition of Matrices with different dimesnions did not fail although it should"

            ]
            testList "sub" [

                testCase "Substraction of 2 Matrices with the same dimensions" <| fun () ->
                        
                    let actual = Matrix.sub testSquareMatrixA testSquareMatrixB

                    let expected =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    testValuesArrRows.[i].[j] - testValuesArrCols.[i].[j]
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))
                        
                    Expect.equal actual expected "Matrix.add did not add the values of two matrices with the same dimensions correctly"
                    
                
                testCase "Subtraction of matrices with different sizes should fail" <| fun () ->
                        
                    Expect.throws (fun () -> Matrix.sub test2x3Matrix testSquareMatrixA |> ignore) "Subtraction of Matrices with different dimesnions did not fail although it should"
            ]
            testList "mul" [

                testCase "Matrix Multiplication with fitting dimensions" <| fun () ->
                        
                    let actual = Matrix.mul test2x3Matrix test3x2MatrixB
                    let expected =
                        let values =
                            [
                                [0.;0.;]
                                [13.;29.]
                            ]
                        let valArr =
                            Array2D.init
                                2
                                2
                                (fun i j ->
                                    values.[i].[j]
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),valArr))
                            
                    Expect.equal actual expected "Matrix multiplication of the 2x3 and 3x2 testmatrices did not return the correct result."

                testCase "Matrix Multiplication with non-fitting dimensions should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.mul testScalarMatrix testSquareMatrixA |> ignore) "Matrix multiplication with non-fitting dimensions did not fail although it should" 

            ]
            testList "mulV" [
                
                testCase "Matrix (m*1)Vector multiplication with correct dimensions" <| fun () ->
                    let actual = Matrix.mulV testSquareMatrixA testVectorA

                    let expected =
                        let values = [|15.;33.;51.|]
                        Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

                    Expect.equal actual expected "Matrix (m*1)Vector multiplication with correct dimensions did not return the correct result vector"
                
                testCase "Matrix (m*1)Vector multiplication with incorrect dimensions should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.mulV testSquareMatrixA testVector1UpperDiag  |> ignore) "Matrix (m*1)Vector multiplication with incorrect dimensions should fail although it should"
            ]
            testList "mulRV" [
                
                testCase "Matrix (1*n) RowVector multiplication with correct dimensions" <| fun () ->
                    let actual = Matrix.mulRV testRowVecB testSquareMatrixA 
                    let expected =
                        let values = [|0.;39.;48.|]
                        RowVector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)
                        
                    Expect.equal actual expected "Matrix (1*n) RowVector multiplication with correct dimensions did not return the correct result rowVector"
                    
                testCase "Matrix (1*n) RowVector multiplication with incorrect dimensions should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.mulRV testRowVecA testSquareMatrixA |> ignore) "Matrix (1*n) RowVector multiplication with incorrect dimensions didnt fail although it should"
                
            ]
            testList "cptMul" [
                
                testCase "Point wise multiplication of two matrices with the same dimensions" <| fun () ->
                    let actual = 
                        Matrix.cptMul testSquareMatrixA testSquareMatrixB

                    let expected =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    testValuesArrRows.[i].[j] * testValuesArrCols.[i].[j]
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))
                    Expect.equal actual expected "Point wise multiplication of two matrices with the same dimensions did not return the correct result matrix"

                testCase "Point wise multiplication of two matrices with different dimensions should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.cptMul testSquareMatrixA testScalarMatrix |> ignore) "Point wise multiplication of two matrices with different dimensions did not fail although it should"
            ]
            testList "cptMax" [

                testCase "Point wise maximization of two matrices with the same dimensions" <| fun () ->
                    let actual = 
                        Matrix.cptMax testSquareMatrixA testSquareMatrixB

                    let expected =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    max testValuesArrRows.[i].[j] testValuesArrCols.[i].[j]
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                    Expect.equal actual expected "Point wise maximization of two matrices with the same dimensions did not return the correct result matrix"

                testCase "Point wise maximization of two matrices with different dimensions should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.cptMax testSquareMatrixA testScalarMatrix |> ignore) "Point wise maximization of two matrices with different dimensions did not fail although it should"

                
            ]
            testList "cptMin" [
                
                testCase "Point wise minimization of two matrices with the same dimensions" <| fun () ->
                    let actual = 
                        Matrix.cptMin testSquareMatrixA testSquareMatrixB

                    let expected =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    min testValuesArrRows.[i].[j] testValuesArrCols.[i].[j]
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                    Expect.equal actual expected "Point wise minimization of two matrices with the same dimensions did not return the correct result matrix"

                testCase "Point wise minimization of two matrices with different dimensions should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.cptMin testSquareMatrixA testScalarMatrix |> ignore) "Point wise minimization of two matrices with different dimensions did not fail although it should"

                
            ]
            testList "scale" [
                
                testCase "scale" <| fun () ->
                    let actual = Matrix.scale 2. testSquareMatrixA

                    let expected : Matrix<float> =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    testValuesArrRows.[i].[j] * 2.
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                    Expect.equal actual expected "Scaling a matrix by a scalar did not return the correctly scaled matrix"
            ]
            testList "neg" [

                let actual = Matrix.neg testSquareMatrixA
                
                let expected : Matrix<float> =
                    let values =
                        Array2D.init
                            3
                            3
                            (fun i j ->
                                testValuesArrRows.[i].[j] * -1.
                            )
                    Matrix.DenseRepr
                        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                Expect.equal actual expected "Negating a matrix did not return the correctly negated matrix"

            ]
            testList "trace" [

                testCase "Trace of a square matrix" <| fun () ->
                    let actual = Matrix.trace testSquareMatrixA
                    Expect.equal actual 9. "Trace of a square matrix was not calculated correctly"

                testCase "Trace of a non-square matrix should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.trace test2x3Matrix |> ignore) "Trace of a non-square matrix did not fail although it should"

            ]
            testList "transpose" [
                
                testCase "transpose of a square matrix" <| fun () ->
                    let actual = Matrix.transpose testSquareMatrixA
                    Expect.equal actual testSquareMatrixB "Transposing a test square matrix did not return the correct result"

                testCase "transpose of a non-square matrix" <| fun () ->
                    let actual = Matrix.transpose test2x3Matrix
                    Expect.equal actual test2x3MatrixTransposed "Transposing a test non-square matrix did not return the correct result"
            ]
            testList "forall" [
                    
                testCase "Check if all values in a matrix are >= 0. (expected to be true)" <| fun () ->
                    Expect.isTrue (Matrix.forall (fun elem -> elem >= 0.) testSquareMatrixA) "test matrix had all values => 0. but the Matrix.forall function failed to recognize"

                testCase "Check if all values in a matrix are >= 1. (expected to be false)" <| fun () ->
                    Expect.isFalse (Matrix.forall (fun elem -> elem >= 1.) testSquareMatrixA) "test matrix did not have all values => 1. but the Matrix.forall function failed to recognize"

            ]
            testList "exists" [
                
                testCase "Check if a testMatrix contains 0. (expected to be true)" <| fun () ->
                    Expect.isTrue (Matrix.exists (fun elem -> elem = 0.) testSquareMatrixA) "Test matrix was expected to contain a value 0., but Matrix.exists returned false"

                testCase "Check if a testMatrix contains 1337. (expected to be false)" <| fun () ->
                    Expect.isFalse (Matrix.exists (fun elem -> elem = 1337.) testSquareMatrixA) "Test matrix was not expected to contain a value 1337., but Matrix.exists returned true"
            ]
            testList "foralli" [
                
                testCase "Check if all values in a matrix are >= 0. (expected to be true)" <| fun () ->
                    Expect.isTrue (Matrix.foralli (fun outerI innerI elem -> elem >= 0.) testSquareMatrixA) "test matrix had all values => 0. but the Matrix.forall function failed to recognize"

                testCase "Check if all values in a matrix are >= 1. (expected to be false)" <| fun () ->
                    Expect.isFalse (Matrix.foralli (fun outerI innerI elem -> elem >= 1.) testSquareMatrixA) "test matrix did not have all values => 1. but the Matrix.forall function failed to recognize"
                    
                testCase "Check if values on the diagonal in a matrix are >= 0. (expected to be true)" <| fun () ->
                    Expect.isTrue (Matrix.foralli (fun outerI innerI elem -> if outerI = innerI then elem >= 0. else true) testSquareMatrixA) "test matrix had all diagonal values => 0. but the Matrix.forall function failed to recognize"

                testCase "Check if all non-diagonal values in a matrix are >= 1337. (expected to be false)" <| fun () ->
                    Expect.isFalse (Matrix.foralli (fun outerI innerI elem -> if outerI <> innerI then elem >= 1337. else true) testSquareMatrixA) "test matrix did not have all non-diagonal values => 1337. but the Matrix.foralli function failed to recognize"

            ]
            testList "existsi" [
                
                testCase "Check if a testMatrix contains 0. (expected to be true)" <| fun () ->
                    Expect.isTrue (Matrix.existsi (fun outerI innerI elem -> elem = 0.) testSquareMatrixA) "Test matrix was expected to contain a value 0., but Matrix.existsi returned false"

                testCase "Check if a testMatrix contains 1337. (expected to be false)" <| fun () ->
                    Expect.isFalse (Matrix.existsi (fun outerI innerI elem -> elem = 1337.) testSquareMatrixA) "Test matrix was not expected to contain a value 1337., but Matrix.existsi returned true"

                testCase "Check if a testMatrix contains 0. on the diagonal (expected to be true)" <| fun () ->
                    Expect.isTrue (Matrix.existsi (fun outerI innerI elem -> if outerI = innerI then elem = 0. else false) testSquareMatrixA) "Test matrix was expected to contain a diagonal value 0., but Matrix.existsi returned false"

                testCase "Check if a testMatrix contains a non diagonal value 1337. (expected to be false)" <| fun () ->
                    Expect.isFalse (Matrix.existsi (fun outerI innerI elem -> if outerI <> innerI then elem = 1337. else false) testSquareMatrixA) "Test matrix was not expected to contain a non-diagonal value 1337., but Matrix.existsi returned true"
            ]
            testList "map" [
                
                testCase "map with (fun elem -> elem * 2)" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.map (fun elem -> elem * 2.)

                    let expected =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    testValuesArrRows.[i].[j] * 2.
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                    Expect.equal actual expected "Mapping the values of a test matrix with * 2. did not return the correct result"

                testCase "map with multiplication by constant should return the same result as matrix.scale" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.map (fun elem -> elem * 2.)

                    let expected = 
                        testSquareMatrixA
                        |> Matrix.scale 2.

                    Expect.equal actual expected "map with multiplication by constant did not return the same result as Matrix.scale"
                        
                testCase "map with multiplication by constant -1. should return the same result as matrix.neg" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.map (fun elem -> elem * -1.)

                    let expected = 
                        testSquareMatrixA
                        |> Matrix.neg

                    Expect.equal actual expected "map with multiplication by constant did not return the same result as Matrix.neg"

            ]
            testList "copy" [
                
                testCase "Matrix copy created by Matrix.copy should equal original matrix" <| fun () ->
                    Expect.equal (Matrix.copy testSquareMatrixA) testSquareMatrixA  "Matrix copy created by Matrix.copy was not equal to original matrix"

                testCase "Matrix copy created by Matrix.copy should stay the same when original matrix is mutated" <| fun () ->
                    let testCopyA = Matrix.copy testSquareMatrixA
                    let testCopyB = Matrix.copy testCopyA
                    Matrix.set testCopyA 0 0 1337.
                    Expect.notEqual testCopyA testCopyB "Matrix copy created by Matrix.copy did not stay the same when original matrix is mutated"
            ]
            testList "mapi" [
                
                testCase "mapi with (fun elem -> elem * 2)" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.mapi (fun i j elem -> elem * 2.)

                    let expected =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    testValuesArrRows.[i].[j] * 2.
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                    Expect.equal actual expected "Mapping the values of a test matrix with * 2. did not return the correct result"

                testCase "map with multiplication by constant should return the same result as matrix.scale" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.mapi (fun i j elem -> elem * 2.)

                    let expected = 
                        testSquareMatrixA
                        |> Matrix.scale 2.

                    Expect.equal actual expected "map with multiplication by constant did not return the same result as Matrix.scale"
                        
                testCase "map with multiplication by constant -1. should return the same result as matrix.neg" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.mapi (fun i j elem -> elem * -1.)

                    let expected = 
                        testSquareMatrixA
                        |> Matrix.neg

                    Expect.equal actual expected "map with multiplication by constant did not return the same result as Matrix.neg"

                testCase "create identity matrix using mapi" <| fun () ->
                    let actual =
                        testSquareMatrixA
                        |> Matrix.mapi 
                            (fun i j elem ->
                                if i = j then
                                    1.
                                else 
                                    0.
                            )
                    Expect.equal actual identityFloat3 "creating identity matrix using Matrix.mapi failed"

            ]
            testList "mapRows" [
                
                testCase "map with Seq.mean" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.mapRows Seq.mean

                    let expected =
                        Vector.init
                            3
                            (fun i -> Seq.mean testValuesArrRows.[i])

                    Expect.equal actual expected "Mapping the rows of a test matrix with Seq.mean did not return the correct result"
            ]
            testList "mapCols" [
                
                testCase "map with Seq.mean" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.mapCols Seq.mean

                    let expected =
                        RowVector.init 3 (fun i -> Seq.mean testValuesArrCols.[i])

                    Expect.equal actual expected "Mapping the cols of a test matrix with Seq.mean did not return the correct result"
            ]
            testList "mapiRows" [
                
                testCase "mapi with Seq.mean" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.mapiRows (fun i x -> float i * Seq.mean x)

                    let expected =
                        Vector.init 3 (fun i -> float i * Seq.average testValuesArrRows.[i])

                    Expect.equal actual expected "Mapping the rows of a test matrix with Seq.mean did not return the correct result"
            ]
            testList "mapiCols" [
                
                testCase "mapi with Seq.mean" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.mapiCols (fun i x -> float i * Seq.mean x)

                    let expected =
                        RowVector.init 3 (fun i -> float i * Seq.average testValuesArrCols.[i])

                    Expect.equal actual expected "Mapping the columns of a test matrix with Seq.mean did not return the correct result"
            ]
            testList "fold" [
                
                testCase "Sum of all matrix entries using Matrix.fold" <| fun () ->
                    let actual = 
                        testSquareMatrixA 
                        |> Matrix.fold (fun acc elem -> acc + elem) 0.

                    Expect.equal actual 21. "Sum of matrix elements was not correctly computed using Matrix.fold"
                
                testCase "count matrix entries using Matrix.fold" <| fun () ->
                    let actual =
                        testSquareMatrixA
                        |> Matrix.fold (fun acc _ -> acc + 1) 0

                    Expect.equal actual 9 "Matrix entries where not correctly counted using Matrix.fold"
            ]
            testList "foldi" [
               
                testCase "Sum of all matrix entries using Matrix.foldi" <| fun () ->
                    let actual = 
                        testSquareMatrixA 
                        |> Matrix.foldi (fun i j acc elem -> acc + elem) 0.

                    Expect.equal actual 21. "Sum of matrix elements was not correctly computed using Matrix.foldi"
                
                testCase "count matrix entries using Matrix.foldi" <| fun () ->
                    let actual =
                        testSquareMatrixA
                        |> Matrix.foldi (fun i j acc _ -> acc + 1) 0

                    Expect.equal actual 9 "Matrix entries where not correctly counted using Matrix.foldi"
                
                testCase "Calculation of Matrix trace using Matrix.foldi should be equal to the result of the Matrix.trace function" <| fun () ->
                    let actual =
                        testSquareMatrixA
                        |> Matrix.foldi 
                            (fun i j acc elem ->
                                if i = j then
                                    acc + elem
                                else
                                    acc
                            )
                            0.
                    Expect.equal actual (Matrix.trace testSquareMatrixA) "Results of Matrix.trace and calculating matrix trace with Matrix.foldi where not equal"
            ]
            testList "filterRows" [
                testCase "simple filter by sum" <| fun () ->
                    let expected = 
                        matrix [
                            [1.;2.]
                            [2.;1.]
                        ]
                    let actual =
                        matrix [
                            [5.;5.]
                            [1.;2.]
                            [5.;5.]
                            [2.;1.]
                            [5.;5.]
                            [5.;5.]
                        ]
                        |> Matrix.filterRows (fun r -> r |> Seq.sum = 3.)
                    Expect.equal actual expected "Matrix.filterRows did not return correct result"                

                testCase "simple filter by contains" <| fun () ->
                    let expected = 
                        matrix [
                            [1.;100.]
                            [2.;100.]
                        ]
                    let actual =
                        matrix [
                            [5.;5.]
                            [1.;2.]
                            [5.;5.]
                            [1.;100.]
                            [2.;100.]
                            [2.;1.]
                            [5.;5.]
                            [5.;5.]
                        ]
                        |> Matrix.filterRows (fun r -> r |> Seq.contains 100.)
                    Expect.equal actual expected "Matrix.filterRows did not return correct result"
            ]            
            testList "filterCols" [
                testCase "simple filter by sum" <| fun () ->
                    let expected = 
                        matrix [
                            [1.;2.]
                            [2.;1.]
                        ]
                    let actual =
                        matrix [
                            [5.;1.;6.;2.;0.]
                            [5.;2.;6.;1.;0.]
                        ]
                        |> Matrix.filterCols (fun c -> c |> Seq.sum = 3.)
                    Expect.equal actual expected "Matrix.filterCols did not return correct result"

                testCase "simple filter by contains" <| fun () ->
                    let expected = 
                        matrix [
                            [100.;2.]
                            [2.;100.]
                        ]
                    let actual =
                        matrix [
                            [5.;100.;6.;2.;0.]
                            [5.;2.;6.;100.;0.]
                        ]
                        |> Matrix.filterCols (fun c -> c |> Seq.contains 100.)
                    Expect.equal actual expected "Matrix.filterCols did not return correct result"
            ]            
            testList "filterCols" [

            ]
            testList "toDense" [
                
                testCase "toDense" <| fun () ->
                    ()
            ]
            testList "initDense" [
                
                testCase "initDense" <| fun () ->
                    ()
            ]
            testList "initSparse" [
                
                testCase "initSparse" <| fun () ->
                    ()
            ]
            testList "nonzero_entries" [
                
                testCase "nonzero_entries" <| fun () ->
                    let actual =
                        Matrix.nonzero_entries testSquareMatrixA
                        |> Array.ofSeq

                    let expected =
                        testValuesArrRows
                        |> Array.mapi 
                            (fun outerI row ->
                                row
                                |> Array.mapi 
                                    (fun innerI elem ->
                                        (outerI,innerI,elem)
                                    )
                            )
                        |> Array.concat
                        |> Array.filter (fun (_,_,elem) -> elem > 0.)

                    Expect.equal actual expected "Matrix.nonzero_entries returned the wron elements/indices"


            ]
            testList "zero" [
                
                let actual = 
                    Matrix.zero 3 3

                let expected =
                    let values =
                        Array2D.init
                            3
                            3
                            (fun i j ->
                                0.
                            )
                    Matrix.DenseRepr
                        (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                Expect.equal actual expected "Matrix with zero entries was not initialized corrtectly"

            ]
            testList "identity" [
                testCase "Create 3x3 identity matrix" <| fun () ->
                    let actual = Matrix.identity 3

                    Expect.equal actual identityFloat3 "Identity Matrix was not correctly initialized"
                
            ]
            testList "ones" [
                
                testCase "Create 3x3 Matrix with only 1. as entries" <| fun () ->
                    let actual = 
                        Matrix.ones 3 3

                    let expected =
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    1.
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))
                    Expect.equal actual expected "Matrix with only 1. as entries was not initialized correctly"
            ]
            testList "getRow" [
                
                testCase "getRow" <| fun () ->
                    let actual = Matrix.getRow testSquareMatrixA 1
                    Expect.equal actual testRowVecC "Matrix.getRow did not return the correct rowvector"

                testCase "Getting row out of row range using Matrix.getRow should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.getRow testSquareMatrixA 1337 |> ignore) "Getting row out of row range using Matrix.getRow did not fail although it should"

            ]
            testList "setRow" [
                
                testCase "Set Row" <| fun () ->

                    let actual = Matrix.copy testSquareMatrixA
                    Matrix.setRow actual 1 testVectorA

                    let expected =
                        let rows = 
                            [|
                                [|0.;1.;2.|]
                                [|0.;3.;6.|]
                                [|0.;5.;6.|]
                            |]
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    rows.[i].[j]
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                    Expect.equal actual expected "Matrix.getRow did not return the correct rowvector"

                testCase "Setting row out of row range using Matrix.setRow should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.setRow testSquareMatrixA 1337 testVectorA |> ignore) "Settingetting row out of row range using Matrix.getRow did not fail although it should"
                    
                testCase "Setting row with vector of wrong length using Matrix.setRow should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.setRow testSquareMatrixA 1 testVector1LowerDiag |> ignore) "Setting row with vector of wrong length using Matrix.setRow did not fail although it should"

            ]
            testList "getCol" [
                
                testCase "getCol" <| fun () ->
                    let actual = Matrix.getCol testSquareMatrixA 0
                    Expect.equal actual testVectorB "Matrix.getCol did nbot return the correct vector"

                testCase "Getting column out of col range using Matrix.getCol should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.getCol testSquareMatrixA 1337 |> ignore) "Getting Column out of col range using Matrix.getCol did not fail although it should"

            ]
            testList "setCol" [
                
                testCase "Set Column" <| fun () ->

                    let actual = Matrix.copy testSquareMatrixA
                    Matrix.setCol actual 0 testVectorA

                    let expected =
                        let rows = 
                            [|
                                [|0.;1.;2.|]
                                [|3.;3.;4.|]
                                [|6.;5.;6.|]
                            |]
                        let values =
                            Array2D.init
                                3
                                3
                                (fun i j ->
                                    rows.[i].[j]
                                )
                        Matrix.DenseRepr
                            (DenseMatrix<float>(Some (Instances.FloatNumerics :> INumeric<float>),values))

                    Expect.equal actual expected "Matrix.setCol did not return the correct vector"

                testCase "Setting column out of col range using Matrix.setCol should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.setRow testSquareMatrixA 1337 testVectorA |> ignore) "Setting column out of col range using Matrix.setCol did not fail although it should"
                
                testCase "Setting column with vector of wrong length using Matrix.setCol should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.setRow testSquareMatrixA 1 testVector1LowerDiag |> ignore) "Setting row with vector of wrong length using Matrix.setRow did not fail although it should"

            ]
            testList "getCols" [
                
                testCase "getCols" <| fun () ->
                    ()
            ]
            testList "getRows" [
                
                testCase "getRows" <| fun () ->
                    ()
            ]
            testList "getRegion" [
                
                testCase "get Region" <| fun () ->
                        
                    let actual = Matrix.getRegion testSquareMatrixA 0 0 3 2

                    Expect.equal actual test2x3MatrixTransposed "Matrix. getRegion did not return the correct matrix window"
            ]
            testList "rowRange" [
                
                testCase "rowRange" <| fun () ->
                    Expect.equal (Matrix.rowRange testSquareMatrixA) (0,2) "Matrix.rowRange did not return the correct dimension"
            ]
            testList "colRange" [
                
                testCase "colRange" <| fun () ->
                    Expect.equal (Matrix.colRange testSquareMatrixA) (0,2) "Matrix.colRange did not return the correct dimension"
            ]
            testList "wholeRegion" [
                
                testCase "wholeRegion" <| fun () ->
                        Expect.equal (Matrix.wholeRegion testSquareMatrixA) ((0,2),(0,2)) "Matrix.wholeRange did not return the correct dimensions"
            ]
            testList "foldByRow" [
                
                testCase "compute row sum vector" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.foldByRow (fun acc elem -> acc + elem) testVectorB

                    let expected = 
                        let values = [|3.;7.;11.|]
                        Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

                    Expect.equal actual expected "Matrix.foldByCol did not compute the correct row sum vector"
            ]
            testList "foldByCol" [
                
                testCase "compute column sum vector" <| fun () ->
                    let actual = 
                        testSquareMatrixA
                        |> Matrix.foldByCol (fun acc elem -> acc + elem) testRowVecD

                    let expected = 
                        let values = [|0.;9.;12.|]
                        RowVector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

                    Expect.equal actual expected "Matrix.foldByCol did not compute the correct column sum vector"
            ]
            testList "foldRow" [
                
                testCase "compute sum of a row" <| fun () ->
                    let actual = Matrix.foldRow (fun acc elem -> acc + elem) 0. testSquareMatrixA 0
                    Expect.equal actual 3. "Matrix.foldRow did not return the correct sum for a row"
            ]
            testList "foldCol" [
                
                testCase "compute sum of a column" <| fun () ->
                    let actual = Matrix.foldCol (fun acc elem -> acc + elem) 0. testSquareMatrixA 0
                    Expect.equal actual 0. "Matrix.foldRow did not return the correct sum for a column"
            ]
            testList "sum" [
                
                testCase "Sum of all matrix entries using Matrix.sum" <| fun () ->
                    let actual = 
                        testSquareMatrixA 
                        |> Matrix.sum

                    Expect.equal actual 21. "Sum of matrix elements was not correctly computed using Matrix.sum"
            ]
            testList "prod" [
                
                testCase "Product of all matrix entries using Matrix.prod" <| fun () ->
                    let actual = 
                        testSquareMatrixA 
                        |> Matrix.prod

                    Expect.equal actual 0. "Product of matrix elements was not correctly computed using Matrix.sum"

            ]
            testList "mean" [
                testCase "meanRowWise" <| fun() ->
                    let testMat = matrix [
                                            [20.;  11.];
                                            [6.;  29.];
                                            [12.;  8.];
                                          ]
                    let correctList = [15.5; 17.5; 10.]

                    let testlist = List.ofArray( Vector.toArray (Matrix.meanRowWise testMat))
                    List.iter2 (fun a b -> Expect.floatClose Accuracy.high a b "means of matrix RowWise was calculated incorrectly") testlist correctList
                
                testCase "meanColumnWise"<| fun() ->
                    let testMat = matrix[
                        [20.;6.;12.];
                        [11.;29.;8.]
                    ]
                    let correctList = [15.5; 17.5; 10.]
                    let testlist = List.ofArray( RowVector.toArray (Matrix.meanColumnWise testMat))
                    List.iter2 (fun a b -> Expect.floatClose Accuracy.high a b "means of matrix ColumnWise was calculated incorrectly") testlist correctList

            ]
            testList "norm" [
                
                testCase "norm" <| fun () ->
                    ()
            ]
            testList "dot" [
                
                testCase "dot" <| fun () ->
                    ()
            ]
            testList "cptPow" [
                
                testCase "cptPow" <| fun () ->
                    ()
            ]

        ]
    ]

