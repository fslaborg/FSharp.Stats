namespace FSharp.Stats.Tests

open Expecto
open FsCheck
open GeneratorsCode

module VectorTests =

    open FSharp.Stats
    open FSharp.Stats.Vector

    let private testVectorA =
        let values = [|0.;3.;6.|]
        Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)


module MatrixTests =

    open FSharp.Stats
    open FSharp.Stats.Matrix

    let private testVectorA =
        let values = [|0.;3.;6.|]
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

    let private testMatrixA : Matrix<float> =
        let values =
            Array2D.init
                3
                3
                (fun i j ->
                    testValuesArrRows.[i].[j]
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

    let testGenericImplementation = 
        testList "Matrix.GenericImplementation" [
            testCase "" <| fun () ->
                ()
        ]

    let testFloatImplementation =
        testList "Matrix.FloatImplementation" [
            //Tests for acessing and setting values in the underlying array2D
            testList "Acessors" [

                testCase "Get value" <| fun () ->
                    let actual = Matrix.get testMatrixA 0 1
                    Expect.equal actual 1. "Matrix.get returned wrong value"

                testCase "Getting value out of range should fail" <| fun () ->
                    Expect.throws (fun () -> Matrix.get testMatrixA 0 7 |> ignore) "Getting value out of range should fail"

                testCase "Set value" <| fun () ->

                    let actual =
                        Matrix.copy testMatrixA
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
                     Expect.throws (fun () -> Matrix.set (Matrix.copy testMatrixA) 0 7 3. |> ignore) "Getting value out of range should fail"
            ]

            testList "Creation" [

                testCase "init" <| fun () ->
                    let actual =
                        Matrix.init 3 3 (fun i j -> testValuesArrRows.[i].[j])
                    Expect.equal actual testMatrixA "Matrix was not initialized correctly using Matrix.init"
                
                testCase "ofJaggedList" <| fun () ->

                    let actual =
                        testValuesArrRows
                        |> List.ofArray
                        |> List.map List.ofArray
                        |> Matrix.ofJaggedList

                    Expect.equal actual testMatrixA "Matrix was not initialized correctly using Matrix.ofJaggedList"

                testCase "ofJaggedColList" <| fun () ->
                    let actual =
                        testValuesArrCols
                        |> List.ofArray
                        |> List.map List.ofArray
                        |> Matrix.ofJaggedColList

                    Expect.equal actual testMatrixA "Matrix was not initialized correctly using Matrix.ofJaggedColList"


                testCase "ofJaggedSeq" <| fun () ->
                    let actual =
                        testValuesArrRows
                        |> Seq.ofArray
                        |> Seq.map Seq.ofArray
                        |> Matrix.ofJaggedSeq

                    Expect.equal actual testMatrixA "Matrix was not initialized correctly using Matrix.ofJaggedSeq"


                testCase "ofJaggedColSeq" <| fun () ->
                    let actual =
                        testValuesArrCols
                        |> Seq.ofArray
                        |> Seq.map Seq.ofArray
                        |> Matrix.ofJaggedColSeq

                    Expect.equal actual testMatrixA "Matrix was not initialized correctly using Matrix.ofJaggedColSeq"


                testCase "ofJaggedArray" <| fun () ->
                    let actual =
                        testValuesArrRows
                        |> Matrix.ofJaggedArray

                    Expect.equal actual testMatrixA "Matrix was not initialized correctly using Matrix.ofJaggedArray"


                testCase "ofJaggedColArray" <| fun () ->
                    let actual =
                        testValuesArrCols
                        |> Matrix.ofJaggedColArray

                    Expect.equal actual testMatrixA "Matrix was not initialized correctly using Matrix.ofJaggedColArray"


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

                    Expect.equal actual testMatrixA "Matrix was not initialized correctly using Matrix.ofArray2D"

                testCase "toArray2D" <| fun () ->

                    let values =
                        Array2D.init
                            3
                            3
                            (fun i j ->
                                testValuesArrRows.[i].[j]
                            )

                    let actual = Matrix.toArray2D testMatrixA

                    Expect.equal actual values "Matrix.toArray2D did not return the correct Array2D"

                testCase "toJaggedArray" <| fun () ->

                    let actual = Matrix.toJaggedArray testMatrixA

                    Expect.equal actual testValuesArrRows "Matrix.toArray2D did not return the correct JaggedArray"

                testCase "getDiagN 1 above diagonal" <| fun () ->
                    
                    let actual = Matrix.getDiagN testMatrixA 1

                    Expect.equal actual testVector1UpperDiag "Matrix.getDiagN did not return the correct offset +1 diagonal"

                testCase "getDiag 1 below diagonal" <| fun () ->

                   let actual = Matrix.getDiagN testMatrixA -1

                   Expect.equal actual testVector1LowerDiag "Matrix.getDiagN did not return the correct offset -1 diagonal"

            


            ]
        ]

