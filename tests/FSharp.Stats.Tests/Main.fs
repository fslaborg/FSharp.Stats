module FSharp.Stats.Tests
open Expecto

[<EntryPoint>]
let main argv =
    //================================ Matrix ===============================================================
    Tests.runTestsWithCLIArgs [] argv MatrixTests.floatImplementationDenseTests |> ignore
    
    //================================ Vector ===============================================================
    Tests.runTestsWithCLIArgs [] argv VectorTests.covarianceTests               |> ignore

    //=========================== Special Functions =========================================================    
    Tests.runTestsWithCLIArgs [] argv SpecialFunctionsTests.gammaFunctionsTests |> ignore
    Tests.runTestsWithCLIArgs [] argv SpecialFunctionsTests.betaFunctionsTests  |> ignore

    //============================= Distributions ===========================================================
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.distanceFunctionsTests |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.chiTests               |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.chiSquaredTests        |> ignore
    //Tests.runTestsWithCLIArgs [] argv DistributionsTests.studentizedRangeTests  |> ignore //Test ommitted due to extremely long runtime of CodeCov.
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.multivariateNormalTests|> ignore
    
    //=============================== Correlation ===========================================================
    Tests.runTestsWithCLIArgs [] argv CorrelationTests.kendallCorrelationTests |> ignore
    Tests.runTestsWithCLIArgs [] argv CorrelationTests.pearsonCorrelationTests |> ignore
    
    //================================ Testing ==============================================================
    Tests.runTestsWithCLIArgs [] argv TestingTests.testPostHocTests |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.hTestTests       |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.chiSquaredTests  |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.pearsonTests     |> ignore
    
    //================================== ML =================================================================
    //SimilarityMetrics
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.jaccardIndexTests           |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.overlapIndexTests           |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.sorensenDiceIndexTests      |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.tverskyIndexTests           |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.tverskySymmetricIndexTests  |> ignore

    0