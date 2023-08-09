module FSharp.Stats.Tests

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [] argv |> ignore
    0