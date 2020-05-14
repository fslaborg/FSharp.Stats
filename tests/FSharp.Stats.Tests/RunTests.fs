namespace FSharp.Stats.Tests

open Expecto

module RunTests =
    open System

    [<EntryPoint>]
    let main args =
        //================================ Matrix ===============================================================
        Tests.runTestsWithArgs defaultConfig args MatrixTests.testFloatImplementationDense |> ignore

        //=========================== Special Functions =========================================================
        Tests.runTestsWithArgs defaultConfig args SpecialFunctionsTests.testGammaFunctions |> ignore
        Tests.runTestsWithArgs defaultConfig args SpecialFunctionsTests.testBetaFunctions  |> ignore
        
        //=========================== Distributions =========================================================
        Tests.runTestsWithArgs defaultConfig args DistributionsTests.testDistanceFunctions |> ignore

        //================================== ML =================================================================
        //SimilarityMetrics
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.jaccardIndexTests           |> ignore
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.overlapIndexTests           |> ignore
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.sorensenDiceIndexTests      |> ignore
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.tverskyIndexTests           |> ignore
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.tverskySymmetricIndexTests  |> ignore
        //Console.ReadKey()
        0

