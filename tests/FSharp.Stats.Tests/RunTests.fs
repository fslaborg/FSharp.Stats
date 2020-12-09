namespace FSharp.Stats.Tests

open Expecto

module RunTests =
    open System

    [<EntryPoint>]
    let main args =
        //================================ Matrix ===============================================================
        Tests.runTestsWithArgs defaultConfig args MatrixTests.testFloatImplementationDense |> ignore
        Tests.runTestsWithArgs defaultConfig args VectorTests.testCovariance               |> ignore

        //=========================== Special Functions =========================================================
        Tests.runTestsWithArgs defaultConfig args SpecialFunctionsTests.testGammaFunctions |> ignore
        Tests.runTestsWithArgs defaultConfig args SpecialFunctionsTests.testBetaFunctions  |> ignore
        
        //============================= Distributions ===========================================================
        Tests.runTestsWithArgs defaultConfig args DistributionsTests.testDistanceFunctions |> ignore
        Tests.runTestsWithArgs defaultConfig args DistributionsTests.testChi               |> ignore
        Tests.runTestsWithArgs defaultConfig args DistributionsTests.testChiSquared        |> ignore
        Tests.runTestsWithArgs defaultConfig args DistributionsTests.testStudentizedRange  |> ignore
        Tests.runTestsWithArgs defaultConfig args DistributionsTests.testMultivariateNormal|> ignore

        //=============================== Correlation ===========================================================
        Tests.runTestsWithArgs defaultConfig args CorrelationTests.testKendallCorrelation |> ignore

        //================================ Testing ==============================================================
        Tests.runTestsWithArgs defaultConfig args TestingTests.testPostHoc |> ignore
        Tests.runTestsWithArgs defaultConfig args TestingTests.hTest       |> ignore
        Tests.runTestsWithArgs defaultConfig args TestingTests.chiSquared  |> ignore

        //================================== ML =================================================================
        //SimilarityMetrics
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.jaccardIndexTests           |> ignore
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.overlapIndexTests           |> ignore
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.sorensenDiceIndexTests      |> ignore
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.tverskyIndexTests           |> ignore
        Tests.runTestsWithArgs defaultConfig args MLTests.SimilarityMetrics.tverskySymmetricIndexTests  |> ignore
        //Console.ReadKey()
        0

