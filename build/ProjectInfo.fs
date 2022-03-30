module ProjectInfo

open Fake.Core

let project = "FSharp.Stats"

let testProject = "tests/FSharp.Stats.Tests/FSharp.Stats.Tests.fsproj"

let solutionFile  = $"{project}.sln"

let configuration = "Release"

let gitOwner = "fslaborg"

let gitHome = $"https://github.com/{gitOwner}"

let projectRepo = $"https://github.com/{gitOwner}/{project}"

let pkgDir = "pkg"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let stableVersion = SemVer.parse release.NugetVersion

let stableVersionTag = (sprintf "%i.%i.%i" stableVersion.Major stableVersion.Minor stableVersion.Patch )

let mutable prereleaseSuffix = ""

let mutable prereleaseTag = ""

let mutable isPrerelease = false