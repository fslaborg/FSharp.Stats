#r "paket:
nuget BlackFox.Fake.BuildTask
nuget Fake.Core.Target
nuget Fake.Core.Process
nuget Fake.Core.ReleaseNotes
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Fake.DotNet.FSFormatting
nuget Fake.DotNet.Fsi
nuget Fake.DotNet.NuGet
nuget Fake.Api.Github
nuget Fake.Extensions.Release
nuget Fake.DotNet.Testing.Expecto //"

#load ".fake/build.fsx/intellisense.fsx"

open BlackFox.Fake
open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open Fake.DotNet.Testing
open Fake.Tools
open Fake.Api
open Fake.Tools.Git
open Fake.Extensions.Release

Target.initEnvironment ()

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

[<AutoOpen>]
module MessagePrompts =

    let prompt (msg:string) =
      System.Console.Write(msg)
      System.Console.ReadLine().Trim()
      |> function | "" -> None | s -> Some s
      |> Option.map (fun s -> s.Replace ("\"","\\\""))

    let rec promptYesNo msg =
      match prompt (sprintf "%s [Yn]: " msg) with
      | Some "Y" | Some "y" -> true
      | Some "N" | Some "n" -> false
      | _ -> System.Console.WriteLine("Sorry, invalid answer"); promptYesNo msg

    let releaseMsg = """This will stage all uncommitted changes, push them to the origin and bump the release version to the latest number in the RELEASE_NOTES.md file. 
        Do you want to continue?"""

    let releaseDocsMsg = """This will push the docs to gh-pages. Do you want to continue?"""

let solutionFile    = "FSharp.Stats.sln"
let configuration   = "Release"
let website         = "/FSharp.Stats"
let gitOwner        = "fslaborg"
let gitName         = "FSharp.Stats"
let gitHome         = sprintf "%s/%s" "https://github.com" gitOwner

let pkgDir          = "pkg"
let authors         = "Benedikt Venn, Timo Muehlhaus, Heinrich Lukas Weil, David Zimmer, Kevin Schneider, open source contributors"
let title           = "FSharp.Stats"
let owners          = "fslaborg, Timo Muehlhaus"
let description     = "F#-first linear algebra, machine learning, fitting, signal processing, and statistical testing."
let licenseUrl      = "https://github.com/fslaborg/FSharp.Stats/blob/developer/LICENSE"
let projectUrl      = "https://github.com/fslaborg/FSharp.Stats"
let iconUrl         = "http://raw.githubusercontent.com/fslaborg/FSharp.Stats/developer/docs/img/logo.png"
let tags            = "F# FSharp dotnet data-science linear-algebra machine-learning fitting signal-processing statistical-testing"
let releaseNotes    = (release.Notes |> String.concat "\r\n")
let repositoryUrl   = "https://github.com/fslaborg/FSharp.Stats"
let stableVersion   = SemVer.parse release.NugetVersion

let testProject     = "tests/FSharp.Stats.Tests/FSharp.Stats.Tests.fsproj"

let clean = BuildTask.create "Clean" [] {
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "pkg"
    ++ "bin"
    |> Shell.cleanDirs 
}

let build = BuildTask.create "Build" [clean] {
    !! "src/**/*.*proj"
    ++ "tests/**/*.*proj"
    |> Seq.iter (DotNet.build id)
}

let copyBinaries = BuildTask.create "CopyBinaries" [clean; build] {
    let targets = 
        !! "src/**/*.??proj"
        -- "src/**/*.shproj"
        |>  Seq.map (fun f -> ((Path.getDirectory f) </> "bin" </> configuration, "bin" </> (Path.GetFileNameWithoutExtension f)))
    for i in targets do printfn "%A" i
    targets
    |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
}

let pack = BuildTask.create "Pack" [clean; build] {
    if promptYesNo (sprintf "creating stable package with version %i.%i.%i OK?" stableVersion.Major stableVersion.Minor stableVersion.Patch ) then
        !! "src/**/*.*proj"
        |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->
            let msBuildParams =
                {p.MSBuildParams with 
                    Properties = ([
                        "Version",(sprintf "%i.%i.%i" stableVersion.Major stableVersion.Minor stableVersion.Patch )
                        "PackageReleaseNotes",  releaseNotes
                    ] @ p.MSBuildParams.Properties)
                }
            {
                p with 
                    MSBuildParams = msBuildParams
                    OutputPath = Some pkgDir
            }
        ))
    else failwith "aborted"
}

let packPrerelease = BuildTask.create "PackPrerelease" [clean; build] {
    !! "src/**/*.*proj"
    |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->
        printfn "Please enter pre-release package suffix"
        let suffix = System.Console.ReadLine()
        let prereleaseTag = (sprintf "%s-%s" release.NugetVersion suffix)
        if promptYesNo (sprintf "package tag will be %s OK?" prereleaseTag )
            then 
                let msBuildParams =
                    {p.MSBuildParams with 
                        Properties = ([
                            "Version",              prereleaseTag
                            "PackageReleaseNotes",  releaseNotes
                        ] @ p.MSBuildParams.Properties)
                    }
                {
                    p with 
                        VersionSuffix = Some suffix
                        OutputPath = Some pkgDir
                        MSBuildParams = msBuildParams
                }
            else
                failwith "aborted"
    ))
}

// --------------------------------------------------------------------------------------
// generate the docs
let buildDocs = BuildTask.create "BuildDocs" [build; copyBinaries] {
    runDotNet "fsdocs build --eval --clean --strict --property Configuration=Release" "./"
}

let watchDocs = BuildTask.create "WatchDocs" [build; copyBinaries] {
   runDotNet "fsdocs watch --eval --clean --property Configuration=Release" "./"
}

let releaseDocs =  BuildTask.create "ReleaseDocs" [buildDocs] {
    Shell.cleanDir "temp"
    Git.CommandHelper.runSimpleGitCommand "." (sprintf "clone %s temp/gh-pages --depth 1 -b gh-pages" repositoryUrl) |> ignore
    Shell.copyRecursive "output" "temp/gh-pages" true |> printfn "%A"
    Git.CommandHelper.runSimpleGitCommand "temp/gh-pages" "add ." |> printfn "%s"
    let cmd = sprintf """commit -a -m "Update generated documentation for version %s""" release.NugetVersion
    Git.CommandHelper.runSimpleGitCommand "temp/gh-pages" cmd |> printfn "%s"
    Git.Branches.push "temp/gh-pages"
}

// --------------------------------------------------------------------------------------
// run test
let runTests = BuildTask.create "RunTests" [clean; build; copyBinaries] {
    let standardParams = Fake.DotNet.MSBuild.CliArguments.Create ()
    Fake.DotNet.DotNet.test(fun testParams ->
        {
            testParams with
                Logger = Some "console;verbosity=detailed"
                Configuration = DotNet.BuildConfiguration.fromString configuration
                NoBuild = true
        }
    ) testProject
}

let runTestsWithCodeCov = BuildTask.create "RunTestsWithCodeCov" [clean; build; copyBinaries] {
    let standardParams = Fake.DotNet.MSBuild.CliArguments.Create ()
    Fake.DotNet.DotNet.test(fun testParams ->
        {
            testParams with
                MSBuildParams = {
                    standardParams with
                        Properties = [
                            "AltCover","true"
                            "AltCoverLcovReport","../../codeCov.txt"
                            "AltCoverForce","true"
                        ]
                };
                Logger = Some "console;verbosity=detailed"
                Configuration = DotNet.BuildConfiguration.fromString configuration
                NoBuild = true
        }
    ) testProject
}

let createAssemblyVersion = BuildTask.create "createvfs" [] {
    AssemblyVersion.create gitName
}

let updateReleaseNotes = BuildTask.createFn "ReleaseNotes" [] (fun config ->
    Release.exists()
    Release.update(gitOwner, gitName, config)
)

let githubDraft = BuildTask.createFn "GithubDraft" [] (fun config ->
    let body = "We are ready to go for the first release!"
    Github.draft(
        gitOwner,
        gitName,
        (Some body),
        None,
        config
    )
)

let _all = BuildTask.createEmpty "All" [clean; build; copyBinaries; runTestsWithCodeCov; pack; buildDocs]

BuildTask.runOrDefaultWithArguments runTests
