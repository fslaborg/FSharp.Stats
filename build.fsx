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

Target.initEnvironment ()

let release = ReleaseNotes.load "RELEASE_NOTES.md"

[<AutoOpen>]
module TemporaryDocumentationHelpers =

    type LiterateArguments =
        { ToolPath : string
          Source : string
          OutputDirectory : string 
          Template : string
          ProjectParameters : (string * string) list
          LayoutRoots : string list 
          FsiEval : bool }


    let private run toolPath command = 
        if 0 <> Process.execSimple ((fun info ->
                { info with
                    FileName = toolPath
                    Arguments = command }) >> Process.withFramework) System.TimeSpan.MaxValue

        then failwithf "FSharp.Formatting %s failed." command

    let createDocs p =
        let toolPath = Tools.findToolInSubPath "fsformatting.exe" (Directory.GetCurrentDirectory() @@ "lib/Formatting")
        let defaultLiterateArguments =
            { ToolPath = toolPath
              Source = ""
              OutputDirectory = ""
              Template = ""
              ProjectParameters = []
              LayoutRoots = [] 
              FsiEval = false }

        let arguments = (p:LiterateArguments->LiterateArguments) defaultLiterateArguments
        let layoutroots =
            if arguments.LayoutRoots.IsEmpty then []
            else [ "--layoutRoots" ] @ arguments.LayoutRoots
        let source = arguments.Source
        let template = arguments.Template
        let outputDir = arguments.OutputDirectory
        let fsiEval = if arguments.FsiEval then [ "--fsieval" ] else []

        let command = 
            arguments.ProjectParameters
            |> Seq.map (fun (k, v) -> [ k; v ])
            |> Seq.concat
            |> Seq.append 
                   (["literate"; "--processdirectory" ] @ layoutroots @ [ "--inputdirectory"; source; "--templatefile"; template; 
                      "--outputDirectory"; outputDir] @ fsiEval @ [ "--replacements" ])
            |> Seq.map (fun s -> 
                   if s.StartsWith "\"" then s
                   else sprintf "\"%s\"" s)
            |> String.separated " "
        run arguments.ToolPath command
        printfn "Successfully generated docs for %s" source

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
let iconUrl         = "https://fslab.org/FSharp.Stats/img/logo.svg"
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

let cleanDocs = BuildTask.create "CleanDocs" [] {
    !! "docs"
    ++ "temp"
    |> Shell.cleanDirs 
}


let build = BuildTask.create "Build" [clean] {
    !! "src/**/*.*proj"
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

let pack = BuildTask.create "Pack" [clean; build.IfNeeded] {
    if promptYesNo (sprintf "creating stable package with version %i.%i.%i OK?" stableVersion.Major stableVersion.Minor stableVersion.Patch ) then
        !! "src/**/*.*proj"
        |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->
            let msBuildParams =
                {p.MSBuildParams with 
                    Properties = ([
                        "Version",(sprintf "%i.%i.%i" stableVersion.Major stableVersion.Minor stableVersion.Patch )
                        "Authors",              authors
                        "Title",                title
                        "Owners",               owners
                        "Description",          description
                        "PackageLicenseUrl",    licenseUrl
                        "PackageProjectUrl",    projectUrl
                        "IconUrl",              iconUrl
                        "PackageTags",          tags
                        "PackageReleaseNotes",  releaseNotes
                        "RepositoryUrl",        repositoryUrl
                        "RepositoryType",       "git"
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

let packPrerelease = BuildTask.create "PackPrerelease" [clean; build.IfNeeded] {
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
                            "Authors",              authors
                            "Title",                title
                            "Owners",               owners
                            "Description",          description
                            "PackageLicenseUrl",    licenseUrl
                            "PackageProjectUrl",    projectUrl
                            "IconUrl",              iconUrl
                            "PackageTags",          tags
                            "PackageReleaseNotes",  releaseNotes
                            "RepositoryUrl",        repositoryUrl
                            "RepositoryType",       "git"
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
// Generate the documentation

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "bin"
let content    = __SOURCE_DIRECTORY__ @@ "docsrc/content"
let output     = __SOURCE_DIRECTORY__ @@ "docs"
let files      = __SOURCE_DIRECTORY__ @@ "docsrc/files"
let templates  = __SOURCE_DIRECTORY__ @@ "docsrc/tools/templates"
let formatting = __SOURCE_DIRECTORY__ @@ "packages/formatting/FSharp.Formatting"
let docTemplate = "docpage.cshtml"

let github_release_user = Environment.environVarOrDefault "github_release_user" gitOwner
let githubLink = sprintf "https://github.com/%s/%s" github_release_user gitName

// Specify more information about your project
let info =
  [ "project-name", title
    "project-author", authors
    "project-summary", description
    "project-github", githubLink
    "project-nuget", "http://nuget.org/packages/FSharp.Stats" ]

let root = website

let referenceBinaries = []

let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en",[   templates;
                            formatting @@ "templates"
                            formatting @@ "templates/reference" ])


let docs = BuildTask.create "Docs" [cleanDocs; build; copyBinaries] {
    let copyFiles () =
        Shell.copyRecursive files output true
        |> Trace.logItems "Copying file: "
        Directory.ensure (output @@ "content")
        Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true
        |> Trace.logItems "Copying styles and scripts: "
    
    File.delete "docsrc/content/release-notes.md"
    Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
    Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

    File.delete "docsrc/content/license.md"
    Shell.copyFile "docsrc/content/" "LICENSE"
    Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE"

    DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
    |> Seq.iter (fun d ->
                    let name = d.Name
                    if name.Length = 2 || name.Length = 3 then
                        layoutRootsAll.Add(
                                name, [templates @@ name
                                       formatting @@ "templates"
                                       formatting @@ "templates/reference" ]))
    copyFiles ()

    for dir in  [ content; ] do
        let langSpecificPath(lang, path:string) =
            path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.exists(fun i -> i = lang)
        let layoutRoots =
            let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
            match key with
            | Some lang -> layoutRootsAll.[lang]
            | None -> layoutRootsAll.["en"] // "en" is the default language

        createDocs (fun args ->
            { args with
                Source = content
                OutputDirectory = output
                LayoutRoots = layoutRoots
                ProjectParameters  = ("root", root)::info
                Template = docTemplate 
                FsiEval = true
                } )
}

let referenceDocs = BuildTask.create "ReferenceDocs" [docs] {
    Directory.ensure (output @@ "reference")
    
    let binaries () =
        let manuallyAdded =
            referenceBinaries
            |> List.map (fun b -> bin @@ b)
        let conventionBased =
            DirectoryInfo.getSubDirectories <| DirectoryInfo bin
            |> Array.collect (fun d ->
                let name, dInfo =
                    d.Name,(DirectoryInfo.getSubDirectories d).[0]
    
                dInfo.GetFiles()
                |> Array.filter (fun x ->
                    x.Name.ToLower() = (sprintf "%s.dll" name).ToLower())
                |> Array.map (fun x -> x.FullName)
                )
            |> List.ofArray
    
        conventionBased @ manuallyAdded
    
    binaries()
    |> FSFormatting.createDocsForDlls (fun args ->
        { args with
            OutputDirectory = output @@ "reference"
            LayoutRoots =  layoutRootsAll.["en"]
            ProjectParameters =  ("root", root)::info
            SourceRepository = githubLink @@ "tree/master" }
            )
}

let releaseDocsConfirmation = BuildTask.create "ReleaseDocsConfirmation" [] { match promptYesNo releaseDocsMsg with | true -> () |_ -> failwith "Release canceled"}

let releaseDocs = BuildTask.create "ReleaseDocs" [releaseDocsConfirmation; docs] {
    let tempDocsDir = "temp/gh-pages"
    Shell.cleanDir tempDocsDir
    Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    Shell.copyRecursive "docs" tempDocsDir true |> Fake.Core.Trace.tracef "%A"
    Git.Staging.stageAll tempDocsDir
    Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Git.Branches.push tempDocsDir
}

let releaseLocal = BuildTask.create "ReleaseLocal" [docs] {
    let tempDocsDir = "temp/localDocs"
    Shell.cleanDir tempDocsDir |> ignore
    Shell.copyRecursive "docs" tempDocsDir true  |> printfn "%A"
    Shell.replaceInFiles 
        (seq {
            yield "href=\"/" + title + "/","href=\""
            yield "src=\"/" + title + "/","src=\""}) 
        (Directory.EnumerateFiles tempDocsDir |> Seq.filter (fun x -> x.EndsWith(".html")))
}


let runTests = BuildTask.create "RunTests" [clean; build; copyBinaries] {
    let standardParams = Fake.DotNet.MSBuild.CliArguments.Create ()
    Fake.DotNet.DotNet.test(fun testParams ->
        {
            testParams with
                Logger = Some "console;verbosity=detailed"
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
                            "AltCoverCobertura","../../codeCov.xml"
                            "AltCoverForce","true"
                        ]
                };
                Logger = Some "console;verbosity=detailed"
        }
    ) testProject
}
let _all = BuildTask.createEmpty "All" [clean; cleanDocs; build; copyBinaries; runTestsWithCodeCov; pack; docs; referenceDocs]

BuildTask.runOrDefault runTests
