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

let internal testingArray = 
    [|
        FSharp.Stats.Ops.inf
        888.
        1.
        0.
        -1
        -888.
        FSharp.Stats.Ops.infNeg
    |]

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
    testList "Test Linker functions for GLM" [
        testCase "LogLinkFunction" <| fun () ->
            
            let linkExpected        = 
                [|
                    FSharp.Stats.Ops.inf 
                    6.78897174
                    0.
                    -36.04365339
                    -36.04365339
                    -36.04365339
                    -36.04365339  
                |]

            let linkInvExpected        = 
                [|
                    FSharp.Stats.Ops.inf 
                    FSharp.Stats.Ops.inf
                    2.71828183
                    1.
                    0.36787944
                    0.
                    0.        
                |]

            let linkDerExpected  =
                [|
                    0.00000000e+00
                    1.12612613e-03
                    1.00000000e+00
                    4.50359963e+15
                    4.50359963e+15
                    4.50359963e+15
                    4.50359963e+15      
                |]

            let linkInvDerExpected  =
                [|
                    FSharp.Stats.Ops.inf
                    FSharp.Stats.Ops.inf
                    2.71828183
                    1.
                    0.36787944
                    0.
                    0.        
                |]
            
            let link            = Fitting.GLM.LinkFunctions.LogLinkFunction 
            let linkF           = link.getLink
            let linkFInv        = link.getInvLink
            let linkDer         = link.getDeriv
            let linkFInvDer     = link.getInvLinkDerivative

            let linkFActual             = testingArray |> Array.map(linkF) 
            let linkFInvActual          = testingArray |> Array.map(linkFInv) 
            let linkFDerActual          = testingArray |> Array.map(linkDer) 
            let linkFInvDerActual       = testingArray |> Array.map(linkFInvDer) 

            for i=0 to testingArray.Length-1 do
                let expected    = linkExpected.[i]
                let actual      = linkFActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} LogLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} LogLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} LogLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} LogLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"


            for i=0 to testingArray.Length-1 do
                let expected    = linkInvExpected.[i]
                let actual      = linkFInvActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} LogLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} LogLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} LogLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} LogLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkDerExpected.[i]
                let actual      = linkFDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i}  derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
        
            for i=0 to testingArray.Length-1 do
                let expected    = linkInvDerExpected.[i]
                let actual      = linkFInvDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} Poisson inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} Poisson inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} Poisson inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} Poisson inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
        
        testCase "InverseLinkFunction" <| fun () ->
            let linkExpected        = 
                [|
                    0.
                    0.00112613
                    1.
                    Ops.inf
                    -1.
                    -0.00112613
                    -0.           
                |]

            let linkInvExpected        = 
                [|
                    0.
                    0.00112613
                    1.
                    Ops.inf 
                    -1.
                    -0.00112613
                    -0.           
                |]

            let linkDerExpected  =
                [|
                    -0.00000000e+00
                    -1.26816005e-06
                    -1.00000000e+00
                    Ops.infNeg
                    -1.00000000e+00
                    -1.26816005e-06
                    -0.00000000e+00  
                |]
            
            let linkInvDerExpected  =
                [|
                    -0.00000000e+00
                    -1.26816005e-06
                    -1.00000000e+00
                    Ops.infNeg
                    -1.00000000e+00
                    -1.26816005e-06
                    -0.00000000e+00      
                |]
            
            let link            = Fitting.GLM.LinkFunctions.InverseLinkFunction
            let linkF           = link.getLink
            let linkFInv        = link.getInvLink
            let linkDer         = link.getDeriv
            let linkFInvDer     = link.getInvLinkDerivative

            let linkFActual             = testingArray |> Array.map(linkF) 
            let linkFInvActual          = testingArray |> Array.map(linkFInv) 
            let linkFDerActual          = testingArray |> Array.map(linkDer) 
            let linkFInvDerActual       = testingArray |> Array.map(linkFInvDer) 

            for i=0 to testingArray.Length-1 do
                let expected    = linkExpected.[i]
                let actual      = linkFActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} InverseLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} InverseLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} InverseLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} InverseLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkInvExpected.[i]
                let actual      = linkFInvActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} InverseLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} InverseLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} InverseLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} InverseLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkDerExpected.[i]
                let actual      = linkFDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} InverseLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} InverseLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} InverseLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} InverseLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"


            for i=0 to testingArray.Length-1 do
                let expected    = linkInvDerExpected.[i]
                let actual      = linkFInvDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} InverseLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} InverseLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} InverseLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} InverseLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

        testCase "LogitLinkFunction" <| fun () ->
            let linkExpected        = 
                [|
                    36.04365339
                    36.04365339
                    36.04365339
                    -36.04365339
                    -36.04365339
                    -36.04365339
                    -36.04365339        
                |]

            let linkInvExpected        = 
                [|
                    1.
                    1.
                    0.73105858
                    0.5
                    0.26894142
                    0.
                    0.        
                |]

            let linkDerExpected  =
                [|
                    4.50359963e+15
                    4.50359963e+15
                    4.50359963e+15
                    4.50359963e+15
                    4.50359963e+15
                    4.50359963e+15
                    4.50359963e+15          
                |]

            let linkInvDerExpected  =
                [|
                    nan
                    nan
                    0.19661193
                    0.25
                    0.19661193
                    0.
                    0.             
                |]
            
            let link            = Fitting.GLM.LinkFunctions.LogitLinkFunction
            let linkF           = link.getLink
            let linkFInv        = link.getInvLink
            let linkDer         = link.getDeriv
            let linkFInvDer     = link.getInvLinkDerivative

            let linkFActual             = testingArray |> Array.map(linkF) 
            let linkFInvActual          = testingArray |> Array.map(linkFInv) 
            let linkFDerActual          = testingArray |> Array.map(linkDer) 
            let linkFInvDerActual       = testingArray |> Array.map(linkFInvDer) 

            for i=0 to testingArray.Length-1 do
                let expected    = linkExpected.[i]
                let actual      = linkFActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} LogitLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} LogitLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} LogitLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} LogitLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkInvExpected.[i]
                let actual      = linkFInvActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} LogitLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} LogitLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} LogitLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} LogitLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkDerExpected.[i]
                let actual      = linkFDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} LogitLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} LogitLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} LogitLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} LogitLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"


            for i=0 to testingArray.Length-1 do
                let expected    = linkInvDerExpected.[i]
                let actual      = linkFInvDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} LogitLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} LogitLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} LogitLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} LogitLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

        testCase "InverseSquaredLinkFunction" <| fun () ->
            let linkExpected        = 
                [|
                    0.00000000e+00
                    1.26816005e-06
                    1.00000000e+00
                    Ops.inf
                    1.00000000e+00
                    1.26816005e-06
                    0.00000000e+00    
                |]

            let linkInvExpected        = 
                [|
                    0.
                    0.0335578
                    1.
                    Ops.inf 
                    nan
                    nan
                    0.       
                |]

            let linkDerExpected  =
                [|
                    -0.00000000e+00
                    -2.85621633e-09
                    -2.00000000e+00
                    Ops.infNeg
                    2.00000000e+00
                    2.85621633e-09
                    0.00000000e+00          
                |]

            let linkInvDerExpected  =
                [|
                    -0.00000000e+00
                    -1.88951592e-05
                    -5.00000000e-01
                    Ops.infNeg
                    nan
                    nan
                    -0.00000000e+00          
                |]
            
            let link            = Fitting.GLM.LinkFunctions.InverseSquaredLinkFunction
            let linkF           = link.getLink
            let linkFInv        = link.getInvLink
            let linkDer         = link.getDeriv
            let linkFInvDer     = link.getInvLinkDerivative

            let linkFActual             = testingArray |> Array.map(linkF) 
            let linkFInvActual          = testingArray |> Array.map(linkFInv) 
            let linkFDerActual          = testingArray |> Array.map(linkDer) 
            let linkFInvDerActual       = testingArray |> Array.map(linkFInvDer) 

            for i=0 to testingArray.Length-1 do
                let expected    = linkExpected.[i]
                let actual      = linkFActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} InverseSquaredLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} InverseSquaredLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} InverseSquaredLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} InverseSquaredLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkInvExpected.[i]
                let actual      = linkFInvActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} InverseSquaredLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} InverseSquaredLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} InverseSquaredLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} InverseSquaredLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkDerExpected.[i]
                let actual      = linkFDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} InverseSquaredLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} InverseSquaredLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} InverseSquaredLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} InverseSquaredLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkInvDerExpected.[i]
                let actual      = linkFInvDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} InverseSquaredLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} InverseSquaredLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} InverseSquaredLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} InverseSquaredLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

        testCase "IdentityLinkFunction" <| fun () ->
            let linkExpected        = 
                [|
                    Ops.inf
                    888.
                    1.
                    0.
                    -1.
                    -888.
                    Ops.infNeg
                |]

            let linkInvExpected        = 
                [|
                    Ops.inf
                    888.
                    1.
                    0.
                    -1.
                    -888.
                    Ops.infNeg
                |]

            let linkDerExpected  =
                [|
                    1.
                    1.
                    1.
                    1.
                    1.
                    1.
                    1.
                |]

            let linkInvDerExpected  =
                [|
                    1.
                    1.
                    1.
                    1.
                    1.
                    1.
                    1.
                |]
            
            let link            = Fitting.GLM.LinkFunctions.IdentityLinkFunction
            let linkF           = link.getLink
            let linkFInv        = link.getInvLink
            let linkDer         = link.getDeriv
            let linkFInvDer     = link.getInvLinkDerivative

            let linkFActual             = testingArray |> Array.map(linkF) 
            let linkFInvActual          = testingArray |> Array.map(linkFInv) 
            let linkFDerActual          = testingArray |> Array.map(linkDer) 
            let linkFInvDerActual       = testingArray |> Array.map(linkFInvDer) 

            for i=0 to testingArray.Length-1 do
                let expected    = linkExpected.[i]
                let actual      = linkFActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} IdentityLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} IdentityLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} IdentityLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} IdentityLinkFunction Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkInvExpected.[i]
                let actual      = linkFInvActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} IdentityLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} IdentityLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} IdentityLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} IdentityLinkFunction inverse Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkDerExpected.[i]
                let actual      = linkFDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} IdentityLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} IdentityLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} IdentityLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} IdentityLinkFunction derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

            for i=0 to testingArray.Length-1 do
                let expected    = linkInvDerExpected.[i]
                let actual      = linkFInvDerActual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $" isInf Element {i} IdentityLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $" isNegInf Element {i} IdentityLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"isNan Element {i} IdentityLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.medium
                        expected
                        actual
                        $" Else Element {i} IdentityLinkFunction inverse derivative Linkfunction is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

    ]
    //InverseSquaredLinkFunction

[<Tests>]
let familyVarianceFunctions = 
    testList "familyVarianceFunctions" [

        testCase "Binomial" <| fun () ->
            let formular x      = x - ((x*x)/(float testingArray.Length))
            let expected        = Array.map formular testingArray
            let actualFormular  = GlmDistributionFamily.getVariance (GlmDistributionFamily.Binomial)
            let actual          = Array.map actualFormular testingArray
            for i=0 to testingArray.Length-1 do
                let expected    = expected.[i]
                let actual      = actual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.high
                        expected
                        actual
                        $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

        testCase "Poisson" <| fun () ->
            let formular x = x
            let expected    = Array.map formular testingArray
            let actualFormular =  GlmDistributionFamily.getVariance (GlmDistributionFamily.Poisson)
            let actual      = Array.map actualFormular testingArray
            for i=0 to testingArray.Length-1 do
                let expected    = expected.[i]
                let actual      = actual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.high
                        expected
                        actual
                        $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

        testCase "Gaussian/Normal" <| fun () ->
            let formular x = 1.
            let expected    = Array.map formular testingArray
            let actualFormular =  GlmDistributionFamily.getVariance (GlmDistributionFamily.Normal)
            let actual      = Array.map actualFormular testingArray
            for i=0 to testingArray.Length-1 do
                let expected    = expected.[i]
                let actual      = actual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.high
                        expected
                        actual
                        $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
        
        testCase "Gamma" <| fun () ->
            let formular (x:float) = x**2
            let expected    = Array.map formular testingArray
            let actualFormular =  GlmDistributionFamily.getVariance (GlmDistributionFamily.Gamma)
            let actual      = Array.map actualFormular testingArray
            for i=0 to testingArray.Length-1 do
                let expected    = expected.[i]
                let actual      = actual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.high
                        expected
                        actual
                        $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
     
        testCase "Inv.Gaussian" <| fun () ->
            let formular (x:float) = x**3
            let expected    = Array.map formular testingArray
            let actualFormular =  GlmDistributionFamily.getVariance (GlmDistributionFamily.InverseGaussian)
            let actual      = Array.map actualFormular testingArray
            for i=0 to testingArray.Length-1 do
                let expected    = expected.[i]
                let actual      = actual.[i] 
                if isInf actual then
                    Expect.isTrue (isInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNegInf actual then
                    Expect.isTrue (isNegInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                elif isNan actual then
                    Expect.isTrue (isNan expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
                else
                    Expect.floatClose 
                        Accuracy.high
                        expected
                        actual
                        $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

    ]

[<Tests>]
let GLMTestsQR = 
    testList "GLM-QR-Results" [
        testCase "Test QR Poisson on Cheese Dataset in F# vs R" <| fun () ->
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

            let actualResults =
                QR.solveQrNewton cheeseMatrix cheeseVector 200 GlmDistributionFamily.Poisson tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Acetic wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM H2S wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "GLM Lactic wrong"

        testCase "Test QR Poisson on Energy Dataset in F# vs R" <| fun () ->
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

            let actualResults =
                QR.solveQrNewton energyMatrix energyVector 200 GlmDistributionFamily.Poisson tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Fat wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM NonFat wrong"

        testCase "Test QR Gamma on lungcap in F# vs R" <| fun () ->
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

            let actualResults =
                QR.solveQrNewton lungcapMatrix lungcapVector 200 GlmDistributionFamily.Gamma tolRef
            

            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] "GLM Intecept wrong"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Age wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM Ht wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "GLM Gender wrong"
            Expect.floatClose Accuracy.medium actualResults.[4] expected.[4] "GLM Smoke wrong"

    ]

[<Tests>]
let GLMTestsIrLS = 
    testList "GLM-IrLS-Results" [
        testCase "Test IrLS Poisson on Cheese Dataset in F# vs R" <| fun () ->
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

        testCase "Test IrLS Poisson on Energy Dataset in F# vs R" <| fun () ->
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

        testCase "Test IrLS Gamma on lungcap in F# vs R" <| fun () ->
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