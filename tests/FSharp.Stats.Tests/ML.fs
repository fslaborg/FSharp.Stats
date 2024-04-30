module MLTests
open Expecto
    
open FSharp.Stats
open FSharp.Stats.ML
open TestExtensions

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

module PCA =         
    [<Tests>]
    let pcaTests =
        //The Implementation was compared to the R function prcomp(). The implementation is based on remarks found in https://stats.stackexchange.com/a/134283
        //Signs of loadings and principal components (scores) can differ from the R implementation due to different svd implementations being used internally.
        //Colab workbook for direct comparison to prcomps output is accessible at: https://colab.research.google.com/drive/1DJ4ky5F5kBM87JprmAbx_gTHqSdz3vqU?usp=sharing
    
        let data = 
            [
                [1.0; 2.0;1.0; 2.0;];
                [1.1; 2.1;1.1; 2.1;];
                [-1.0; -2.0;1.0; 2.0;];
                [-1.1; -2.1;1.1; 2.1;];
                [-1.15; -2.15;1.15; 2.15;];
            ]
            |> FSharp.Stats.Matrix.ofJaggedList
        
        let dataNan = 
            [
                [nan; 2.0;1.0; 2.0;];
                [1.1; 2.1;1.1; 2.1;];
                [-1.0; -2.0;1.0; 2.0;];
                [-1.1; -2.1;1.1; 2.1;];
                [-1.15; -2.15;1.15; 2.15;];
            ]
            |> FSharp.Stats.Matrix.ofJaggedList
        
        let dataInf = 
            [
                [infinity; 2.0;1.0; 2.0;];
                [1.1; 2.1;1.1; 2.1;];
                [-1.0; -2.0;1.0; 2.0;];
                [-1.1; -2.1;1.1; 2.1;];
                [-1.15; -2.15;1.15; 2.15;];
            ]
            |> FSharp.Stats.Matrix.ofJaggedList
        
        let dataNegInf = 
            [
                [-infinity; 2.0;1.0; 2.0;];
                [1.1; 2.1;1.1; 2.1;];
                [-1.0; -2.0;1.0; 2.0;];
                [-1.1; -2.1;1.1; 2.1;];
                [-1.15; -2.15;1.15; 2.15;];
            ]
            |> FSharp.Stats.Matrix.ofJaggedList

        testList "PCA" [

            testCase "center_catch_nan" <| fun () ->
                Expect.throws (fun _ -> ML.Unsupervised.PCA.center dataNan |> ignore) "did not catch nan in input."

            testCase "center_catch_inf" <| fun () ->
                Expect.throws (fun _ -> ML.Unsupervised.PCA.center dataInf |> ignore) "did not catch inf in input."
            
            testCase "center_catch_negInf" <| fun () ->
                Expect.throws (fun _ -> ML.Unsupervised.PCA.center dataNegInf |> ignore) "did not catch -inf in input."

            testCase "compute_catch_nan" <| fun () ->
                Expect.throws (fun _ -> ML.Unsupervised.PCA.compute dataNan |> ignore) "did not catch nan in input."

            testCase "compute_catch_inf" <| fun () ->
                Expect.throws (fun _ -> ML.Unsupervised.PCA.compute dataInf |> ignore) "did not catch inf in input."

            testCase "compute_catch_negInf" <| fun () ->
                Expect.throws (fun _ -> ML.Unsupervised.PCA.compute dataNegInf |> ignore) "did not catch -inf in input."

            testCase "centerMatrixColumnWise" <| fun () ->
                let m = ML.Unsupervised.PCA.center data
                let correctCentered = 
                    [|[|1.1751115628; 1.1995649372; -1.1666666667; -1.1666666667|];
                    [|1.2706490883; 1.2489297494; 0.5000000000; 0.5000000000|];
                    [|-0.7356389458; -0.7750275520; -1.1666666667; -1.1666666667|];
                    [|-0.8311764713; -0.8243923643; 0.5000000000; 0.5000000000|];
                    [|-0.8789452340; -0.8490747704; 1.3333333333; 1.3333333333|]|]
                    |> matrix
                TestExtensions.sequenceEqual Accuracy.low m correctCentered "matrix was centered incorrectly."

            testCase "compute_VarianceOfComponent" <| fun () ->
                let c = ML.Unsupervised.PCA.center data
                let pca = ML.Unsupervised.PCA.compute c
                let correct = vector [|1.794382894; 1.334096203; 0.01942548992; 8.52122906e-17|]
                TestExtensions.sequenceEqual Accuracy.low pca.VarianceOfComponent correct "Variances of components were not calculated correctly."

            testCase "compute_VarExplainedByComponentIndividual" <| fun () ->
                let c = ML.Unsupervised.PCA.center data
                let pca = ML.Unsupervised.PCA.compute c
                let correct = vector [|0.6439619942; 0.3559625358; 7.546993175e-05; 9.770164109e-34|]
                TestExtensions.sequenceEqual Accuracy.low pca.VarExplainedByComponentIndividual correct "Variance explained by individual components was not calculated correctly."

            testCase "compute_VarExplainedByComponentCumulative" <| fun () ->
                let c = ML.Unsupervised.PCA.center data
                let pca = ML.Unsupervised.PCA.compute c
                let correct = vector [|0.6439619942; 0.9999245301; 1.0; 1.0|]
                TestExtensions.sequenceEqual Accuracy.low pca.VarExplainedByComponentCumulative correct "Cumulative variances were not calculated correctly."
            
            testCase "compute_PrincipalComponents" <| fun () ->
                let c = ML.Unsupervised.PCA.center data
                let pca = ML.Unsupervised.PCA.compute c
                let correct = 
                    [|[|2.3539452686; -0.0211916481; -0.0236227100; -0.0; 0.0|];
                      [|0.7595272938; -1.7598322707; 0.0219839347; -0.0; 0.0|];
                      [|0.4117370587; 1.9220381325; 0.0172220468; -0.0; 0.0|];
                      [|-1.3277364367; 0.3280019411; -0.0027888992; -0.0; 0.0|];
                      [|-2.1974731844; -0.4690161547; -0.0127943723; -0.0; 0.0|]|]
                    |> matrix
                TestExtensions.sequenceEqual Accuracy.low pca.PrincipalComponents correct "Principal component scores were not calculated correctly."
            
            testCase "compute_Loadings" <| fun () ->
                let c = ML.Unsupervised.PCA.center data
                let pca = ML.Unsupervised.PCA.compute c
                let correct = 
                    [|[|0.5018458447; -0.4965896061; 0.7082016036; -0.0|];
                      [|0.4979792094; -0.5035828626; -0.7059893820; 0.|];
                      [|-0.5000837205; -0.4999015221; 0.003839354807; -0.7071067812|];
                      [|-0.5000837205; -0.4999015221; 0.003839354807; 0.7071067812|]|]
                    |> matrix
                TestExtensions.sequenceEqual Accuracy.low pca.Loadings correct "Loadings were not calculated correctly."
        ]
module hClust = 
    open FSharp.Stats.ML.Unsupervised
    open System.Text.RegularExpressions 
    open HierarchicalClustering
    open FSharpAux
    let datapath = @"data/testDatahClust.csv"

    
    let inline hClustTests name rounding (fromFloat : float -> 'a) (parse : string -> 'a) = 
        let lables,data =
            fromFileWithSep ',' datapath
            |> Seq.skip 1
            |> Seq.map (fun arr -> arr.[4], [| parse arr.[0]; parse arr.[1]; parse arr.[2]; parse arr.[3]; |])
            |> Seq.toArray
            |> Array.mapi (fun i (lable,data) -> sprintf "%s_%i" lable i, data)
            |> Array.unzip
        let distance x y = FSharp.Stats.DistanceMetrics.euclidean x y |> float
        let linker = Linker.singleLwLinker
        let testCluster = generate<'a[]> distance linker data |> Seq.item 0 |> (fun x -> x.Key)
        let testLeaf = createClusterValue 1 [|fromFloat 1.; fromFloat 2.|]
        let testLeaf2 = createClusterValue 2 [|fromFloat 3.;fromFloat 4.|]
        let testSingleCluster = createCluster 1 0.4 testLeaf testLeaf2
        let testClusterList = get testCluster |> Seq.toArray |> Array.mapi (fun i x -> i,x)
        let cachedDist = DistanceCaching<'a[]> (distance,linker)
        testList name [
            testCase "simple cluster" <| fun () ->
                let testSet =  
                    [|(0, (0.6164414287, 5, 15)); (1, (0.4123105705, 14, 11));
                        (2, (0.3000000119, 1, 13)); (3, (0.2645751238, 12, 6));
                        (4, (0.2449489683, 2, 3)); (5, (0.1732050776, 10, 9));
                        (6, (0.1414213628, 0, 4)); (7, (0.0, 7, 8))|]
                    |> Array.map (fun (a,(b,c,d)) -> (a,((Math.round (rounding/2) b),c,d) ))
                let actualSet = testClusterList |> Array.map (fun (a,(b,c,d)) -> (a,((Math.round (rounding/2) b),c,d) ))
 
                Expect.equal actualSet testSet "clusters aren't same "

            testCase "euclidean" <| fun () ->
                let testEuclidean = euclidean [|3.;1.;5.|] [|3.;1.;5.|]
                Expect.equal testEuclidean 0. "euclidean distance - same values check"
                Expect.equal (euclidean [|1.;2.;4.|][|3.;-2.;4.|] |> Math.round 5) 4.47214 "euclidean distance - negative check "
            testCase "create Clusters and Leafs "<| fun () -> 
                let testSingleCluster = Node (1, 0.4, 2, Leaf (1, 1, [|fromFloat 1.0; fromFloat 2.0|]), Leaf (2, 1, [|fromFloat -3.0; fromFloat 4.0|]))
                let testLeaf1 = Leaf (1, 1, [|fromFloat 1.0; fromFloat 2.0|])
                let testLeaf2 = Leaf (2, 1, [|fromFloat -3.0; fromFloat 4.0|])
                Expect.equal (createClusterValue 1 [|fromFloat 1.;fromFloat 2.|]) testLeaf1 "creating Leaf failed"
                Expect.equal (createClusterValue 2 [|fromFloat -3.;fromFloat 4.|]) testLeaf2 "creating Leaf failed"
                Expect.equal (createCluster 1 0.4 testLeaf testLeaf2) testSingleCluster "creating Cluster failed "
                Expect.equal (getClusterId testLeaf1 ) 1 "getClusterID Leaf"
                Expect.equal (getClusterId testLeaf2 ) 2 "getClusterID Leaf"
                Expect.equal (getClusterId testSingleCluster ) 1 "getClusterID Clust"
            testCase "getValues" <| fun () -> 
                let testDistances = [0.0; 0.1414213628; 0.1732050776; 0.2449489683; 0.2645751238; 0.3000000119;0.4123105705; 0.6164414287] |> List.map (fun x -> Math.round rounding x)
                let allLeafs =   [[|5.0; 3.4; 1.5; 0.2|]; [|5.0; 3.4; 1.5; 0.2|]; [|5.0; 3.6; 1.4; 0.2|];[|5.1; 3.5; 1.4; 0.2|]; [|4.6; 3.4; 1.4; 0.3|]; [|4.6; 3.1; 1.5; 0.2|];[|4.7; 3.2; 1.3; 0.2|]; [|4.9; 3.0; 1.4; 0.2|]; [|5.4; 3.9; 1.7; 0.4|]] |> List.map (Array.map fromFloat)
                Expect.equal (tryGetLeafValue testSingleCluster) None "tryGetLeafValue failed"
                Expect.equal (tryGetLeafValue testLeaf) (Some [|fromFloat 1.0;fromFloat 2.0|]) "tryGetLeafValue failed"
                Expect.equal (getClusterMemberCount testLeaf ) 1 "MemberCount off"
                Expect.equal (getClusterMemberCount testSingleCluster ) 2 "MemberCount off"
                Expect.equal (getClusterMemberCount testCluster) 9 "MemberCount off "
                Expect.equal (getDistancesOfCluster testCluster|> List.map (fun x -> Math.round rounding x))  testDistances "testDistances off"
                Expect.equal (getLeafsOfCluster testCluster ) allLeafs "Leaf retrieval failed "
                Expect.equal (getLeafsOfCluster testLeaf ) [[|fromFloat 1.0; fromFloat 2.0|]] "Leaf retrieval failed"
                Expect.equal (getLeafNamesOfCluster testCluster) [8; 7; 4; 0; 6; 3; 2; 1; 5] "ID retrieve failed "
                Expect.equal (getLeafNamesOfCluster testLeaf) [1] "ID retrieve failed"
                Expect.equal (getLeftChild testSingleCluster ) testLeaf "left child failed"
                Expect.equal (getRightChild testSingleCluster ) testLeaf2 "right child failed"
                Expect.equal (getLeftChild testCluster ) (Leaf(5, 1, [|fromFloat 5.4; fromFloat 3.9; fromFloat 1.7; fromFloat 0.4|])) "complex left child failed"
                Expect.equal (getDistance testSingleCluster ) 0.4 "getDistance failed"
                Expect.equal (getDistance testLeaf) -1. "getDistance at Leaf failed"
                Expect.equal 
                    (usedDistancesAndLabels testCluster  |> List.map (fun x -> (Math.round rounding (fst x) ,snd x )  ))
                      ([(0.0, [9]); (0.1414213628, [10]); (0.1732050776, [11]);(0.2449489683, [12]); (0.2645751238, [13]); (0.3000000119, [14]);(0.4123105705, [15]); (0.6164414287, [16])] |> List.map (fun x -> (Math.round rounding (fst x) ,snd x )  ))
                    "used Distances and Labels won't work "
                Expect.equal 
                    (getDistancesAndLabels testCluster  |> List.map (fun x -> (fst x ,Math.round rounding (snd x )  )))
                      ([(9, 0.0); (10, 0.1414213628); (11, 0.1732050776); (12, 0.2449489683);(13, 0.2645751238); (14, 0.3000000119); (15, 0.4123105705);(16, 0.6164414287)]|> List.map (fun x -> (fst x ,Math.round rounding (snd x ))))

                     "Distances and Labels won't work "
        ]
    [<Tests>]
    let hClustTestsFloat = hClustTests "hClust Tests" 10 id float
    [<Tests>]
    let hClustTestsFloat32 = hClustTests "hClust Tests float32" 4 float32 float32

module KNN =
    open FSharp.Stats.ML.Unsupervised

    [<Tests>]
    let knnTests = 
        testList "KNN Tests" [
            testCase "Array.blueVsRedPoints" <| fun () ->                
                let reds =
                    [| 
                        [ 2.0; 4.0 ]
                        [ 1.0; 3.0 ]
                        [ 2.0; 4.0 ]
                        [ 3.0; 2.0 ]
                        [ 2.0; 1.0 ]
                    |] |> Array.map (fun p -> (p, "red"))                
                let blues =
                    [| 
                        [ 5.0; 6.0 ]
                        [ 4.0; 5.0 ]
                        [ 4.0; 6.0 ]
                        [ 6.0; 6.0 ]
                        [ 5.0; 4.0 ]
                    |] |> Array.map (fun p -> (p, "blue"))  

                let labeledPoints = Array.append reds blues
                let prediction = KNN.Array.predict FSharp.Stats.DistanceMetrics.euclidean labeledPoints

                let predicted = prediction 3 [3.0; 3.0]

                Expect.isTrue predicted.IsSome "Has Label"
                Expect.equal predicted.Value "red" "label should be red"

                let predicted = prediction 3 [6.0; 6.0]

                Expect.isTrue predicted.IsSome "Has Label"
                Expect.equal predicted.Value "blue" "label should be blue"

            
            testCase "Seq.blueVsRedPoints" <| fun () ->
                let points = seq {
                        vector [ 2.0; 4.0 ]
                        vector [ 1.0; 3.0 ]
                        vector [ 2.0; 4.0 ]
                        vector [ 3.0; 2.0 ]
                        vector [ 2.0; 1.0 ]
                        vector [ 5.0; 6.0 ]
                        vector [ 4.0; 5.0 ]
                        vector [ 4.0; 6.0 ]
                        vector [ 6.0; 6.0 ]
                        vector [ 5.0; 4.0 ]
                    }
                let labels = seq { "red"; "red"; "red"; "red"; "red"; "blue"; "blue"; "blue"; "blue"; "blue" }
                let labeledPoints = Seq.zip points labels
                let prediction = KNN.Seq.predict FSharp.Stats.DistanceMetrics.Vector.euclidean labeledPoints 3

                let predicted = prediction (vector [3.0; 3.0])

                Expect.isTrue predicted.IsSome "Has Label"
                Expect.equal predicted.Value "red" "label should be red"

                let predicted = prediction (vector [6.0; 6.0])

                Expect.isTrue predicted.IsSome "Has Label"
                Expect.equal predicted.Value "blue" "label should be blue"

            testCase "KnnClassifier.blueVsRedPoints" <| fun () ->
                let knnClassifier = KNN.Classifier(FSharp.Stats.DistanceMetrics.euclidean, 3)

                let reds  = [| [ 2.0; 4.0 ]; [ 1.0; 3.0 ]; [ 2.0; 4.0 ]; [ 3.0; 2.0 ]; [ 2.0; 1.0 ] |]
                let blues = [| [ 5.0; 6.0 ]; [ 4.0; 5.0 ]; [ 4.0; 6.0 ]; [ 6.0; 6.0 ]; [ 5.0; 4.0 ] |]

                let labeledPoints = Map [ "blue", blues; "red", reds ]
                knnClassifier.fit(labeledPoints)

                let color  = knnClassifier.predict [3.0; 3.0]
                Expect.isTrue color.IsSome "Has Label"
                Expect.equal color.Value "red" "label should be red"
                
                let color  = knnClassifier.predict [6.0; 6.0]
                Expect.isTrue color.IsSome "Has Label"
                Expect.equal color.Value "blue" "label should be blue"

                let points = Array.append reds blues
                let labels = [| "red"; "red"; "red"; "red"; "red"; "blue"; "blue"; "blue"; "blue"; "blue" |]
                knnClassifier.fit(points, labels)

                let color  = knnClassifier.predict [3.0; 3.0]
                Expect.isTrue color.IsSome "Has Label"
                Expect.equal color.Value "red" "label should be red"
                
                let color  = knnClassifier.predict [6.0; 6.0]
                Expect.isTrue color.IsSome "Has Label"
                Expect.equal color.Value "blue" "label should be blue"

            testCase "KnnClassifier.1d" <| fun () ->
                let points = Array.init 200 (fun idx -> 0.1 * float idx)

                let labeledPoints = Map [
                    "blue", points;
                    "red", points |> Array.map (fun p -> -p)
                ]
                
                let distance a b = abs (a - b)
                let knnClassifier = KNN.Classifier(distance, 5)
                knnClassifier.fit(labeledPoints)

                let positiveSamples = Array.init 300 (fun idx -> float (idx + 1))
                let negativeSamples = Array.init 300 (fun idx -> float -(idx + 1))

                let positivePredictions = knnClassifier.predict positiveSamples
                let negativePredictions = knnClassifier.predict negativeSamples

                (positivePredictions, negativePredictions)
                ||> Array.zip
                |> Array.iter (fun (posLabel, negLabel) ->                
                    Expect.isTrue posLabel.IsSome "Has Label"
                    Expect.equal posLabel.Value "blue" "label should be blue"
                    
                    Expect.isTrue negLabel.IsSome "Has Label"
                    Expect.equal negLabel.Value "red" "label should be red"
                )        
            ]
    