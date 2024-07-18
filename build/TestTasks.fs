﻿module TestTasks

open BlackFox.Fake
open Fake.DotNet

open ProjectInfo
open BasicTasks

let runTests = BuildTask.create "RunTests" [clean; build] {
    Fake.DotNet.DotNet.test(fun testParams ->
        {
            testParams with
                Logger = Some "console;verbosity=detailed"
                Configuration = DotNet.BuildConfiguration.fromString configuration
                NoBuild = true
                MSBuildParams = { testParams.MSBuildParams with DisableInternalBinLog = true }
        }
        |> DotNet.Options.withCustomParams (Some "-tl")
    ) testProject
}

// to do: use this once we have actual tests
let runTestsWithCodeCov = BuildTask.create "RunTestsWithCodeCov" [clean; build] {
    let standardParams = Fake.DotNet.MSBuild.CliArguments.Create ()
    Fake.DotNet.DotNet.test(fun testParams ->
        {
            testParams with
                MSBuildParams = {
                    standardParams with
                        Properties = [
                            "AltCover","true"
                            "AltCoverCobertura","../../codeCov.xml"
                            "AltCoverForce","true"
                        ]
                        DisableInternalBinLog = true 
                };
                Logger = Some "console;verbosity=detailed"
        }
    ) testProject
}