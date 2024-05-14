module GLMTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Fitting.GLM
open TestExtensions
open System
open Deedle

let private extemes = 
    [   
        Double.PositiveInfinity
        Double.MaxValue
        Double.Epsilon
        0.
        Double.MinValue
        Double.NegativeInfinity
        Double.NaN
    ]

let internal currentDir  = System.IO.Directory.GetCurrentDirectory()

let internal tolRef = 1e-11

module internal HelperFunctions = 
    
    let internal testingWithOneCreation (matrix: Matrix<float>) =
        matrix
        |> Matrix.toJaggedArray
        |> Array.map(fun x -> 
            [
                [|1.|]
                x
            ]
            |> Array.concat
        )
        |> Matrix.ofJaggedArray

    let rec internal dropCols (frame:Frame<int,string>) (toDrop:string list) =
        if toDrop=List.empty then
            frame
        else
            let drop = List.head toDrop
            let frameNew = Frame.dropCol drop frame
            dropCols frameNew (toDrop|> List.tail)
    
    let internal generateBaseMatrixAndVector (yColumn:string) (colsToDrop:string list) (frame:Frame<int,string>) =
        let vector = 
            frame
            |> Frame.getCol yColumn
            |> Series.values
            |> Vector.ofSeq
        let matrix = 
            dropCols frame (yColumn::colsToDrop)
            |> Frame.toJaggedArray
            |> Matrix.ofJaggedArray
            |> testingWithOneCreation
        matrix,vector

    let internal checkIfInvIsPossible (matrix:Matrix<float>) =
        matrix  
        |> Matrix.exists (fun x -> 
            FSharp.Stats.Ops.isNan(x) ||
            FSharp.Stats.Ops.isPosInf(x)||
            FSharp.Stats.Ops.isNegInf(x)
            )
        |> not


[<Tests>]
let linkerFunctions = 
    Expect.isTrue true

[<Tests>]
let GLMTestsQR = 
    testList "GLM-QR-Results" [
        testCase "Test QR Results on Cheese Dataset in F# vs R" <| fun () ->
            //Results using GLM in R
            let expected = 
                [
                    1.179102        //Intercept
                    0.000776314     //Acetic
                    1.358578e-05    //H2S
                    1.145854        //Lactic
                ]
                |>Vector.ofList

            let dataPath    = System.IO.Path.Combine(currentDir,"data/glm_test_cheese.csv")
            let cheeseframe: Frame<int,string> =
                Deedle.Frame.ReadCsv(dataPath,hasHeaders=true,inferTypes=true,separators=",")
                |> Frame.indexRows "Column1"

            let cheeseMatrix,cheeseVector = HelperFunctions.generateBaseMatrixAndVector "Taste" [] cheeseframe

            let actualResults,actualStats =
                QR.solveQrNewton cheeseMatrix cheeseVector 200 GlmDistributionFamily.Poisson tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Acetic wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM H2S wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "GLM Lactic wrong"

        testCase "Test QR Results on Energy Dataset in F# vs R" <| fun () ->
            //Results using GLM in R
            let expected = 
                [
                    3.83535         //Intercept
                    0.004066056     //Fat
                    0.008595802     //NonFat
                ]
                |>Vector.ofList

            let dataPath    = System.IO.Path.Combine(currentDir,"data/glm_test_energy.csv")
            let energyframe: Frame<int,string> =
                Deedle.Frame.ReadCsv(dataPath,hasHeaders=true,inferTypes=true,separators=",")
                |> Frame.indexRows "Column1"

            let energyMatrix,energyVector = HelperFunctions.generateBaseMatrixAndVector "Energy" [] energyframe

            let actualResults,actualStats =
                QR.solveQrNewton energyMatrix energyVector 200 GlmDistributionFamily.Poisson tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Fat wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM NonFat wrong"

        testCase "Test QR Results on lungcap in F# vs R" <| fun () ->
            //Results using GLM in R
            let expected = 
                [
                    1.495925        //Intercept
                    -0.007646505    //Age
                    -0.0165144      //Ht
                    -0.0002111909   //Gender
                    0.01284481      //Smoke
                ]
                |>Vector.ofList

            let dataPath    = System.IO.Path.Combine(currentDir,"data/glm_test_lungcap.csv")
            let lungcapframe: Frame<int,string> =
                Deedle.Frame.ReadCsv(dataPath,hasHeaders=true,inferTypes=true,separators=",")
                |> Frame.indexRows "Column1"

            let lungcapMatrix,lungcapVector = HelperFunctions.generateBaseMatrixAndVector "FEV" [] lungcapframe

            let actualResults,actualStats =
                QR.solveQrNewton lungcapMatrix lungcapVector 200 GlmDistributionFamily.Gamma tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Age wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM Ht wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "GLM Gender wrong"
            Expect.floatClose Accuracy.medium actualResults.[4] expected.[4] "GLM Smoke wrong"

    ]
let GLMTestsIrLS = 
    testList "GLM-IrLS-Results" [
        testCase "Test IrLS Results on Cheese Dataset in F# vs R" <| fun () ->
            //Results using GLM in R
            let expected = 
                [
                    1.179102        //Intercept
                    0.000776314     //Acetic
                    1.358578e-05    //H2S
                    1.145854        //Lactic
                ]
                |>Vector.ofList

            let dataPath    = System.IO.Path.Combine(currentDir,"data/glm_test_cheese.csv")
            let cheeseframe: Frame<int,string> =
                Deedle.Frame.ReadCsv(dataPath,hasHeaders=true,inferTypes=true,separators=",")
                |> Frame.indexRows "Column1"

            let cheeseMatrix,cheeseVector = HelperFunctions.generateBaseMatrixAndVector "Taste" [] cheeseframe

            let actualResults,actualStats =
                IrLS.solveIrls cheeseMatrix cheeseVector 200 GlmDistributionFamily.Poisson tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Acetic wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM H2S wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "GLM Lactic wrong"

        testCase "Test IrLS Results on Energy Dataset in F# vs R" <| fun () ->
            //Results using GLM in R
            let expected = 
                [
                    3.83535         //Intercept
                    0.004066056     //Fat
                    0.008595802     //NonFat
                ]
                |>Vector.ofList

            let dataPath    = System.IO.Path.Combine(currentDir,"data/glm_test_energy.csv")
            let energyframe: Frame<int,string> =
                Deedle.Frame.ReadCsv(dataPath,hasHeaders=true,inferTypes=true,separators=",")
                |> Frame.indexRows "Column1"

            let energyMatrix,energyVector = HelperFunctions.generateBaseMatrixAndVector "Energy" [] energyframe

            let actualResults,actualStats =
                IrLS.solveIrls energyMatrix energyVector 200 GlmDistributionFamily.Poisson tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Fat wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM NonFat wrong"

        testCase "Test IrLS Results on lungcap in F# vs R" <| fun () ->
            //Results using GLM in R
            let expected = 
                [
                    1.495925        //Intercept
                    -0.007646505    //Age
                    -0.0165144      //Ht
                    -0.0002111909   //Gender
                    0.01284481      //Smoke
                ]
                |>Vector.ofList

            let dataPath    = System.IO.Path.Combine(currentDir,"data/glm_test_lungcap.csv")
            let lungcapframe: Frame<int,string> =
                Deedle.Frame.ReadCsv(dataPath,hasHeaders=true,inferTypes=true,separators=",")
                |> Frame.indexRows "Column1"

            let lungcapMatrix,lungcapVector = HelperFunctions.generateBaseMatrixAndVector "FEV" [] lungcapframe

            let actualResults,actualStats =
                IrLS.solveIrls lungcapMatrix lungcapVector 200 GlmDistributionFamily.Gamma tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Age wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM Ht wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "GLM Gender wrong"
            Expect.floatClose Accuracy.medium actualResults.[4] expected.[4] "GLM Smoke wrong"

    ]