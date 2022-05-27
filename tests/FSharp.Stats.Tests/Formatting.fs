module FormattingTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Algebra
open TestExtensions

[<Tests>]
let formatValueTests =

    testList "Formatting.formatValue" [
        testCase "Format small positive float value" (fun _ -> Expect.equal (Formatting.formatValue 10.5342) "10.534" "Incorrect format for this value")
        testCase "Format large positive float value" (fun _ -> Expect.equal (Formatting.formatValue 10000000.234) "1e+07" "Incorrect format for this value")
        testCase "Format small negative float value" (fun _ -> Expect.equal (Formatting.formatValue -122.2424456) "-122.24" "Incorrect format for this value")
        testCase "Format large negative float value" (fun _ -> Expect.equal (Formatting.formatValue -1000000.345) "-1e+06" "Incorrect format for this value")
        testCase "Format small positive int value" (fun _ -> Expect.equal (Formatting.formatValue 10) "10" "Incorrect format for this value")
        testCase "Format large positive int value" (fun _ -> Expect.equal (Formatting.formatValue 10000000) "10000000" "Incorrect format for this value")
        testCase "Format small negative int value" (fun _ -> Expect.equal (Formatting.formatValue -122) "-122" "Incorrect format for this value")
        testCase "Format nan" (fun _ -> Expect.equal (Formatting.formatValue nan) "NaN" "Incorrect format for this value")
        testCase "Format infinity" (fun _ -> Expect.equal (Formatting.formatValue infinity) "Infinity" "Incorrect format for this value")
        testCase "Format -infinity" (fun _ -> Expect.equal (Formatting.formatValue -infinity) "-Infinity" "Incorrect format for this value")
    ]

[<Tests>]
let formatTableTests =    
    
    let m1 = 
        [|
            [|0.1;-1003547672323.2|]
            [|-0.1;1003547672323.2|]
        |] 
        |> JaggedArray.map Formatting.formatValue
        |> array2D
        |> Formatting.formatTable

    let expected = readEmbeddedRessource "TableFormat.txt"

    testList "Formatting.formatTable" [
        testCase "string values formatted as table" (fun _ -> Expect.equal m1 expected "Incorrect format for this value")
    ]


[<Tests>]
let matrixFormattingtests =    
    testList "Formatting.MatrixFormatting" [

        let rnd = new System.Random(69)

        let mDense1 = Matrix.init 10 10 (fun i j -> float i * float j * rnd.NextDouble())
        let mDense2 = Matrix.init 10 100 (fun i j -> float i * float j * rnd.NextDouble())
        let mDense3 = Matrix.init 100 10 (fun i j -> float i * float j * rnd.NextDouble())
        let mDense4 = Matrix.init 100 100 (fun i j -> float i * float j * rnd.NextDouble())

        let mdense1_no_info = readEmbeddedRessource "DenseMatrixFormat1NoInfo.txt"
        let mdense2_no_info = readEmbeddedRessource "DenseMatrixFormat2NoInfo.txt"
        let mdense3_no_info = readEmbeddedRessource "DenseMatrixFormat3NoInfo.txt"
        let mdense4_no_info = readEmbeddedRessource "DenseMatrixFormat4NoInfo.txt"
        let mdense1_with_info = readEmbeddedRessource "DenseMatrixFormat1WithInfo.txt"
        let mdense2_with_info = readEmbeddedRessource "DenseMatrixFormat2WithInfo.txt"
        let mdense3_with_info = readEmbeddedRessource "DenseMatrixFormat3WithInfo.txt"
        let mdense4_with_info = readEmbeddedRessource "DenseMatrixFormat4WithInfo.txt"

        let mDenseSpecial = matrix[[nan;100000000.;infinity;1.4];[1.337;-nan;4269420.42;-infinity]]

        let mdenseSpecial_no_info = readEmbeddedRessource "DenseMatrixSpecialNoInfo.txt"
        let mdenseSpecial_with_info = readEmbeddedRessource "DenseMatrixSpecialWithInfo.txt"

        let mSparse1 = Matrix.initSparse 10 10 [ 1,1,13.37; 2,2,6942013.37 ]

        let msparse1_no_info = readEmbeddedRessource "SparseMatrixFormat1NoInfo.txt"
        let msparse1_with_info = readEmbeddedRessource "SparseMatrixFormat1WithInfo.txt"

        testCase "dense float matrix full display no info" (fun _ -> Expect.equal (mDense1.Format(false)) mdense1_no_info "Incorrect format for this value")        
        
        testCase "dense float matrix full display with info" (fun _ -> Expect.equal (mDense1.Format(true)) mdense1_with_info "Incorrect format for this value")

        testCase "dense float matrix omitted cols no info" (fun _ -> Expect.equal (mDense2.Format(false)) mdense2_no_info "Incorrect format for this value")        
        
        testCase "dense float matrix omitted cols with info" (fun _ -> Expect.equal (mDense2.Format(true)) mdense2_with_info "Incorrect format for this value")

        testCase "dense float matrix omitted rows no info" (fun _ -> Expect.equal (mDense3.Format(false)) mdense3_no_info "Incorrect format for this value")        
        
        testCase "dense float matrix omitted rows with info" (fun _ -> Expect.equal (mDense3.Format(true)) mdense3_with_info "Incorrect format for this value")

        testCase "dense float matrix omitted rows and cols no info" (fun _ -> Expect.equal (mDense4.Format(false)) mdense4_no_info "Incorrect format for this value")        
        
        testCase "dense float matrix omitted rows and cols with info" (fun _ -> Expect.equal (mDense4.Format(true)) mdense4_with_info "Incorrect format for this value" )
        
        testCase "dense float matrix with edge cases (+/- nan, +/- infinity) no info" (fun _ -> Expect.equal (mDenseSpecial.Format(true)) mdenseSpecial_with_info "Incorrect format for this value" )
        
        testCase "dense float matrix with edge cases (+/- nan, +/- infinity) with info" (fun _ -> Expect.equal (mDenseSpecial.Format(true)) mdenseSpecial_with_info "Incorrect format for this value" )

        testCase "sparse float matrix full display no info" (fun _ ->  Expect.equal (mSparse1.Format(false)) msparse1_no_info "Incorrect format for this value")

        testCase "sparse float matrix full display with info" (fun _ -> Expect.equal (mSparse1.Format(true)) msparse1_with_info "Incorrect format for this value")
    ]