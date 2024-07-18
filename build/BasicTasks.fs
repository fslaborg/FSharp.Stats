module BasicTasks

open BlackFox.Fake
open Fake.IO
open Fake.DotNet
open Fake.IO.Globbing.Operators

open ProjectInfo

let setPrereleaseTag = BuildTask.create "SetPrereleaseTag" [] {
    printfn "Please enter pre-release package suffix"
    let suffix = System.Console.ReadLine()
    prereleaseSuffix <- suffix
    //prereleaseTag <- (sprintf "%s-%s" release.NugetVersion suffix)
    prereleaseTag <- (sprintf "%i.%i.%i-%s" release.SemVer.Major release.SemVer.Minor release.SemVer.Patch suffix)
    isPrerelease <- true
}

let clean = BuildTask.create "Clean" [] {
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    ++ "pkg"
    |> Shell.cleanDirs 
}

let build = BuildTask.create "Build" [clean] {
    solutionFile
    |> DotNet.build (fun p ->
        let msBuildParams =
            {p.MSBuildParams with 
                DisableInternalBinLog = true
            }
        {
            p with 
                MSBuildParams = msBuildParams
        }
        |> DotNet.Options.withCustomParams (Some "-tl")
    )
}