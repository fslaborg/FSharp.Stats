module MLTests
open Expecto
    
open FSharp.Stats
open FSharp.Stats.ML

module SimilarityMetrics = 
    
    type private testType = {
        testVal1: float
        testVal2: string []
    }
    let private stringTestSetX = set ["A"; "B"; "C"]
    let private stringTestSetY = set ["C";"D"]
    let private floatTestSetX = set [1.;2.;3.]
    let private floatTestSetY = set [2.;5.]
    let private recordTypeSetX = 
        set [
                {testVal1=1.;testVal2=[|"A"|]}
                {testVal1=2.;testVal2=[|"B"|]}
                {testVal1=3.;testVal2=[|"C"|]}
            ]
    let private recordTypeSetY = 
        set [
                {testVal1=2.;testVal2=[|"B"|]}
                {testVal1=5.;testVal2=[|"D"|]}
            ]
    let private emptyfloatSet : Set<float>          = Set.empty
    let private emptystringSet : Set<string>        = Set.empty
    let private emptyRecordTypeSet : Set<testType>  = Set.empty

    [<Tests>]
    let jaccardIndexTests =
        testList "SimilarityMetrics.jaccard" [

            testCase "Empty float Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.jaccard emptyfloatSet emptyfloatSet)
                Expect.floatClose Accuracy.high actual 1. "Jaccard index for two empty float sets was not 1."

            testCase "Empty String Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.jaccard emptystringSet emptystringSet)
                Expect.floatClose Accuracy.high actual 1. "Jaccard index for two empty float sets was not 1."

            testCase "Empty Record Type Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.jaccard emptyRecordTypeSet emptyRecordTypeSet)
                Expect.floatClose Accuracy.high actual 1. "Jaccard index for two empty float sets was not 1."

            testCase "Equal Sets"<| fun () ->
                let actual = (SimilarityMetrics.Set.jaccard floatTestSetX floatTestSetX)
                Expect.floatClose Accuracy.veryHigh actual 1. "Jaccard index for two equal sets was not 1."

            testCase "float Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.jaccard floatTestSetX floatTestSetY)
                Expect.floatClose Accuracy.high actual (1./4.) "Jaccard index for two float sets not correct"

            testCase "string Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.jaccard stringTestSetX stringTestSetY)
                Expect.floatClose Accuracy.high actual (1./4.) "Jaccard index for two string sets not correct"

            testCase "Record Type Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.jaccard recordTypeSetX recordTypeSetY)
                Expect.floatClose Accuracy.high actual (1./4.) "Jaccard index for two recordType sets not correct"
        ]

    [<Tests>]
    let overlapIndexTests =
        testList "SimilarityMetrics.overlap" [

            testCase "Empty String Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.overlap emptystringSet emptystringSet)
                Expect.floatClose Accuracy.high actual 1. "Overlap index for two empty float sets was not 1."

            testCase "Empty Record Type Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.overlap emptyRecordTypeSet emptyRecordTypeSet)
                Expect.floatClose Accuracy.high actual 1. "Overlap index for two empty float sets was not 1."

            testCase "Overlap of nonEmpty with empty set" <| fun () ->
                let actual = (SimilarityMetrics.Set.overlap emptyRecordTypeSet recordTypeSetX)
                Expect.floatClose Accuracy.high actual 0. "Overlap index for two empty float sets was not 1."

            testCase "Equal Sets"<| fun () ->
                let actual = (SimilarityMetrics.Set.overlap floatTestSetX floatTestSetX)
                Expect.floatClose Accuracy.veryHigh actual 1. "Jaccard index for two equal sets was not 1."

            testCase "float Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.overlap floatTestSetX floatTestSetY)
                Expect.floatClose Accuracy.high actual (1./2.) "Overlap index for two float sets not correct"

            testCase "string Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.overlap stringTestSetX stringTestSetY)
                Expect.floatClose Accuracy.high actual (1./2.) "Overlap index for two string sets not correct"

            testCase "record Type Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.overlap recordTypeSetX recordTypeSetY)
                Expect.floatClose Accuracy.high actual (1./2.) "Overlap index for two recordType sets not correct"

            testCase "Empty float Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.overlap emptyfloatSet emptyfloatSet)
                Expect.floatClose Accuracy.high actual 1. "Overlap index for two empty float sets was not 1."
        ]

    [<Tests>]
    let sorensenDiceIndexTests =
        testList "SimilarityMetrics.sorensenDice" [

            testCase "Empty float Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.sorensenDice emptyfloatSet emptyfloatSet)
                Expect.floatClose Accuracy.high actual 1. "Sorensen Dice index for two empty float sets was not 1."

            testCase "Empty String Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.sorensenDice emptystringSet emptystringSet)
                Expect.floatClose Accuracy.high actual 1. "Sorensen Dice index for two empty float sets was not 1."

            testCase "Empty Record Type Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.sorensenDice emptyRecordTypeSet emptyRecordTypeSet)
                Expect.floatClose Accuracy.high actual 1. "Sorensen Dice index for two empty float sets was not 1."

            testCase "Equal Sets"<| fun () ->
                let actual = (SimilarityMetrics.Set.sorensenDice floatTestSetX floatTestSetX)
                Expect.floatClose Accuracy.veryHigh actual 1. "Sorensen Dice index for two equal sets was not 1."

            testCase "float Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.sorensenDice floatTestSetX floatTestSetY)
                Expect.floatClose Accuracy.high actual (2./5.) "Sorensen Dice index for two float sets not correct"

            testCase "string Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.sorensenDice stringTestSetX stringTestSetY)
                Expect.floatClose Accuracy.high actual (2./5.) "Sorensen Dice index for two string sets not correct"

            testCase "record Type Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.sorensenDice recordTypeSetX recordTypeSetY)
                Expect.floatClose Accuracy.high actual (2./5.) "Sorensen Dice index for two recordType sets not correct"

            testCase "Overlap of nonEmpty with empty set" <| fun () ->
                let actual = (SimilarityMetrics.Set.sorensenDice emptyRecordTypeSet recordTypeSetX)
                Expect.floatClose Accuracy.high actual 0. "Sorensen Dice index for two empty float sets was not 1."
        ]

    [<Tests>]
    let tverskyIndexTests =
        testList "SimilarityMetrics.tversky" [
            testCase "Zero Weights" <| fun () ->
                let actual = (SimilarityMetrics.Set.tversky 0. 0. emptyfloatSet emptyfloatSet)
                Expect.floatClose Accuracy.high actual 1. "Tversky index with zero weights was not 1."

            testCase "Empty float Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.tversky 1. 1. emptyfloatSet emptyfloatSet)
                Expect.floatClose Accuracy.high actual 1. "Tversky index for two empty float sets was not 1."

            testCase "Empty String Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.tversky 1. 1. emptystringSet emptystringSet)
                Expect.floatClose Accuracy.high actual 1. "Tversky index for two empty float sets was not 1."

            testCase "Empty Record Type Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.tversky 1. 1. emptyRecordTypeSet emptyRecordTypeSet)
                Expect.floatClose Accuracy.high actual 1. "Tversky index for two empty float sets was not 1."

            testCase "prototypeWeight = variantWeight = 0.5 equals SorensenDice" <| fun () ->
                let actual      = (SimilarityMetrics.Set.tversky 0.5 0.5 floatTestSetX floatTestSetX)
                let expected    = (SimilarityMetrics.Set.sorensenDice floatTestSetX floatTestSetX)
                Expect.floatClose Accuracy.high actual expected "Tversky with 0.5 weights was not equal to sorensen dice"

            testCase "prototypeWeight = variantWeight = 1. equals jaccard" <| fun () ->
                let actual      = (SimilarityMetrics.Set.tversky 0.5 0.5 floatTestSetX floatTestSetX)
                let expected    = (SimilarityMetrics.Set.jaccard floatTestSetX floatTestSetX)
                Expect.floatClose Accuracy.high actual expected "Tversky with 1. weights was not equal to jaccard"
        ]

    [<Tests>]
    let tverskySymmetricIndexTests =
        testList "SimilarityMetrics.Symmetric Tversky" [
            testCase "Zero Weights" <| fun () ->
                let actual = (SimilarityMetrics.Set.tverskySymmetric 0. 0. emptyfloatSet emptyfloatSet)
                Expect.floatClose Accuracy.high actual 1. "Symmetric Tversky index with zero weights was not 1."

            testCase "Empty float Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.tverskySymmetric 1. 1. emptyfloatSet emptyfloatSet)
                Expect.floatClose Accuracy.high actual 1. "Symmetric Tversky index for two empty float sets was not 1."

            testCase "Empty String Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.tverskySymmetric 1. 1. emptystringSet emptystringSet)
                Expect.floatClose Accuracy.high actual 1. "Symmetric Tversky index for two empty float sets was not 1."

            testCase "Empty Record Type Set Similarity" <| fun () ->
                let actual = (SimilarityMetrics.Set.tverskySymmetric 1. 1. emptyRecordTypeSet emptyRecordTypeSet)
                Expect.floatClose Accuracy.high actual 1. "Symmetric Tversky index for two empty float sets was not 1."

        ]
    

module DistanceMetrics = 
    open DistanceMetrics

    [<Tests>]
    let hausdorff =
        testList "DistanceMetrics" [
            
            let dataA = 
                [|1.,3.;2.,5.;3.,2.;4.,6.;5.,2.;6.,4.;7.,2.|]
                |> Array.map (fun (x,y) -> [|x;y|])
            let dataB = 
                [|0.4,4.;0.7,4.7;4.,4.;4.2,8.;4.5,5.;4.7,6.;5.,7.|]
                |> Array.map (fun (x,y) -> [|x;y|])

            testCase "hausdorffDirected" <| fun () ->
                let directionA = DistanceMetrics.hausdorffDirected euclidean dataA dataB
                let directionB = DistanceMetrics.hausdorffDirected euclidean dataB dataA
                Expect.floatClose Accuracy.high directionA 3.60555127546 "Directed hausdorff distance should be 3.60555127"
                Expect.floatClose Accuracy.high directionB 2.00997512422 "Directed hausdorff distance should be 2.009975"
                
            testCase "hausdorff" <| fun () ->
                let actual = DistanceMetrics.hausdorff euclidean dataA dataB
                Expect.floatClose Accuracy.high actual 3.60555127546 "Hausdorff distance should be 3.60555127"

            
        ]
    
    