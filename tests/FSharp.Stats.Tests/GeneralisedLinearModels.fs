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

        // testCase "Binomial" <| fun () ->
        //     let expected        = 
        //         [
        //             2.22044605e-16
        //             2.22044605e-16
        //             2.22044605e-16
        //             2.22044605e-16
        //             2.22044605e-16
        //             2.22044605e-16
        //             2.22044605e-16
        //         ]
        //     let actualFormular  = GlmDistributionFamily.getVariance (GlmDistributionFamily.Binomial)
        //     let actual          = Array.map actualFormular testingArray
        //     for i=0 to testingArray.Length-1 do
        //         let expected    = expected.[i]
        //         let actual      = actual.[i] 
        //         if isInf actual then
        //             Expect.isTrue (isInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
        //         elif isNegInf actual then
        //             Expect.isTrue (isNegInf expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
        //         elif isNan actual then
        //             Expect.isTrue (isNan expected) $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"
        //         else
        //             Expect.floatClose 
        //                 Accuracy.high
        //                 expected
        //                 actual
        //                 $"Element {i} Variance function is incorrect. {testingArray.[i]} was linked to {actual} instead to {expected}"

        testCase "Poisson" <| fun () ->
            let expected    = 
                [|
                    FSharp.Stats.Ops.inf
                    888.
                    1.
                    0.
                    -1
                    -888.
                    FSharp.Stats.Ops.infNeg
                |]
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
            let expected    = 
                [|
                    Ops.inf
                    7.88544e+05
                    1.00000e+00
                    0.00000e+00
                    1.00000e+00
                    7.88544e+05
                    Ops.inf
                |]
            let actualFormular =  GlmDistributionFamily.getVariance (GlmDistributionFamily.Gamma)
            let actual      = Array.map actualFormular testingArray
            let x = expected|>Array.map(fun x -> string x)|>String.concat "|"
            for i=0 to testingArray.Length-1 do
                printfn $"{x}"
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
let GLMStepwise = 
    testList "GLM-QR-Step" [
        testCase "Test QR Poisson Step one" <| fun () ->
            let A       = 
               [
                    [9.4000e+01; 2.3000e+01; 8.6000e-01];
                   [1.7400e+02; 1.5500e+02; 1.5300e+00];
                   [2.1400e+02; 2.3000e+02; 1.5700e+00];
                   [3.1700e+02; 1.8010e+03; 1.8100e+00];
                   [1.0600e+02; 4.5000e+01; 9.9000e-01];
                   [2.9800e+02; 2.0000e+03; 1.0900e+00];
                   [3.6200e+02; 6.1610e+03; 1.2900e+00];
                   [4.3600e+02; 2.8810e+03; 1.7800e+00];
                   [1.3400e+02; 4.7000e+01; 1.2900e+00];
                   [1.8900e+02; 6.5000e+01; 1.5800e+00];
                   [3.1100e+02; 4.6500e+02; 1.6800e+00];
                   [6.3000e+02; 2.7190e+03; 1.9000e+00];
                   [8.8000e+01; 2.0000e+01; 1.0600e+00];
                   [1.8800e+02; 1.4000e+02; 1.3000e+00];
                   [4.6900e+02; 8.5600e+02; 1.5200e+00];
                   [5.8100e+02; 1.4589e+04; 1.7400e+00];
                   [1.2000e+02; 5.0000e+01; 1.1600e+00];
                   [2.2400e+02; 1.1000e+02; 1.4900e+00];
                   [1.9000e+02; 4.8000e+02; 1.6300e+00];
                   [2.3000e+02; 8.6390e+03; 1.9900e+00];
                   [9.6000e+01; 1.4100e+02; 1.1500e+00];
                   [2.0000e+02; 1.8500e+02; 1.3300e+00];
                   [2.3400e+02; 1.0322e+04; 1.4400e+00];
                   [3.4900e+02; 2.6876e+04; 2.0100e+00];
                   [2.1400e+02; 3.9000e+01; 1.3100e+00];
                   [4.2100e+02; 2.5000e+01; 1.4600e+00];
                   [6.3800e+02; 1.0560e+03; 1.7200e+00];
                   [2.0600e+02; 5.0000e+01; 1.2500e+00];
                   [3.3100e+02; 8.0000e+02; 1.0800e+00];
                   [4.8100e+02; 1.2000e+02; 1.2500e+00]
                ]|>Matrix.ofJaggedList
            let b           = 
                [
                   12.3; 20.9; 39. ; 47.9;  5.6; 25.9; 37.3; 21.9; 18.1; 21. ; 34.9;
                   57.2;  0.7; 25.9; 54.9; 40.9; 15.9;  6.4; 18. ; 38.9; 14. ; 15.2;
                   32. ; 56.7; 16.8; 11.6; 26.5;  0.7; 13.4;  5.5
                ]|>Vector.ofList
            let mFam        = GlmDistributionFamily.Poisson
            let t           = Vector.init b.Length (fun x -> 1.)
            let oldResults  = Vector.zeroCreate A.NumCols
            let bMean       = Vector.mean b
            let mu          = Vector.map(fun x -> ((x+bMean)/2.)) b
            let linPred     = Vector.init A.NumRows (fun k -> GlmDistributionFamily.getLinkFunction(mFam).getLink(mu[k]))

            
            let muStartExpected         = 
                [
                   18.41666667; 22.71666667; 31.76666667; 36.21666667; 15.06666667;
                   25.21666667; 30.91666667; 23.21666667; 21.31666667; 22.76666667;
                   29.71666667; 40.86666667; 12.61666667; 25.21666667; 39.71666667;
                   32.71666667; 20.21666667; 15.46666667; 21.26666667; 31.71666667;
                   19.26666667; 19.86666667; 28.26666667; 40.61666667; 20.66666667;
                   18.06666667; 25.51666667; 12.61666667; 18.96666667; 15.01666667
                ]
            let linPredStartExpected    = 
                [
                    2.91325605; 3.12309887; 3.45841752; 3.58951942; 2.7124848 ;
                    3.22750515; 3.43129541; 3.14487041; 3.05948924; 3.12529748;
                    3.39170806; 3.71031473; 2.53501869; 3.22750515; 3.68177091;
                    3.48788463; 3.00650735; 2.73868717; 3.0571409 ; 3.45684231;
                    2.95837649; 2.98904329; 3.34168325; 3.70417849; 3.0285221 ;
                    2.89406862; 3.23933183; 2.53501869; 2.94268305; 2.7091607 
                ]
            let costExpected            = 0.
            let mu_newExpected          = 
                [ 
                    5.93387957; 23.85258586; 26.46314428; 45.4791678 ;  7.75651698;
                   10.60848283; 16.41901117; 45.91968565; 14.39483127; 26.60531649;
                   34.94104656; 65.52718178;  8.83137852; 15.16250092; 27.82629458;
                   45.88264872; 10.9991415 ; 22.67497617; 29.42288129; 61.75247747;
                   10.62818947; 16.21728283; 20.51875221; 68.28943052; 15.71037585;
                   23.99103941; 45.92629836; 13.86306533; 10.60971241; 16.31175616
                ]
            let linPred_newExpected     = Vector.zeroCreate 10
            let wlsResult_newExpected   = Vector.zeroCreate 10
            let wlsendogNewExpected     = Vector.zeroCreate 10

            let costActual,mu_newActual,linPred_newActual,wlsResult_newActual,wlsendogNewActual  = 
                FSharp.Stats.Fitting.GLM.QRSolver.stepwiseGainQR A b mFam t mu linPred oldResults

            for i=0 to (A.NumRows-1) do
                Expect.floatClose Accuracy.high mu.[i] muStartExpected.[i] "muStart differs great"
                Expect.floatClose Accuracy.high mu_newActual.[i] mu_newExpected.[i] "muNew differs great"
                Expect.floatClose Accuracy.high linPred.[i] linPredStartExpected.[i] "linPredStart differs great"
                //Expect.floatClose Accuracy.high linPred_newActual.[i] linPred_newExpected.[i] "linPredStart differs great"
                //Expect.floatClose Accuracy.high wlsResult_newActual.[i] wlsResult_newExpected.[i] "linPredStart differs great"
                //Expect.floatClose Accuracy.high wlsendogNewActual.[i] wlsendogNewExpected.[i] "linPredStart differs great"


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

            let actualResultsRaw =
                SolveGLM.solveQR cheeseMatrix cheeseVector 200 GlmDistributionFamily.Poisson tolRef
            let actualResults = actualResultsRaw.mX

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

            let actualResultsRaw =
                SolveGLM.solveQR energyMatrix energyVector 200 GlmDistributionFamily.Poisson tolRef
            let actualResults = actualResultsRaw.mX

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

            let actualResultsRaw =
                SolveGLM.solveQR lungcapMatrix lungcapVector 200 GlmDistributionFamily.Gamma tolRef
            let actualResults = actualResultsRaw.mX

            let x = $"{actualResults.[0]} {actualResults.[1]} {actualResults.[2]} {actualResults.[3]} {actualResults.[4]}"
            Expect.floatClose Accuracy.medium actualResults.[0] expected.[0] $"GLM Intecept wrong {x}"
            Expect.floatClose Accuracy.medium actualResults.[1] expected.[1] "GLM Age wrong"
            Expect.floatClose Accuracy.medium actualResults.[2] expected.[2] "GLM Ht wrong"
            Expect.floatClose Accuracy.medium actualResults.[3] expected.[3] "GLM Gender wrong"
            Expect.floatClose Accuracy.medium actualResults.[4] expected.[4] "GLM Smoke wrong"

    ]
