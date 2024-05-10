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
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "Intecept GLM wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "Acetic GLM wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "H2S GLM wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "Lactic GLM wrong"

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
            

                Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "Intecept GLM wrong"
                Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "Fat GLM wrong"
                Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "NonFat GLM wrong"
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
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "Intecept GLM wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "Acetic GLM wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "H2S GLM wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "Lactic GLM wrong"

        testCase "Test IrLs Results on Energy Dataset in F# vs R" <| fun () ->
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
            

                Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "Intecept GLM wrong"
                Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "Fat GLM wrong"
                Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "NonFat GLM wrong"
    ]