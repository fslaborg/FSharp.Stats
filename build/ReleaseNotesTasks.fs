module ReleaseNotesTasks

open Fake.Extensions.Release
open BlackFox.Fake

//let createAssemblyVersion = BuildTask.create "createvfs" [] {
//    AssemblyVersion.create ProjectInfo.project
//}

let updateReleaseNotes = BuildTask.createFn "ReleaseNotes" [] (fun config ->
    ReleaseNotes.ensure()
    ReleaseNotes.update(ProjectInfo.gitOwner, ProjectInfo.project, config)
)

//let githubDraft = BuildTask.createFn "GithubDraft" [] (fun config ->
//    let body = "We are ready to go for the first release!"
//    Github.draft(
//        ProjectInfo.gitOwner,
//        ProjectInfo.project,
//        (Some body),
//        None,
//        config
//    )
//)