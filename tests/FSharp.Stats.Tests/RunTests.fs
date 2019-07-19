namespace FSharp.Stats.Tests

open Expecto

module RunTests =
    open System

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args SpecialFunctionsTests.testGammaFunctions |> ignore
        Tests.runTestsWithArgs defaultConfig args SpecialFunctionsTests.testBetaFunctions  |> ignore
        //Console.ReadKey()
        0

