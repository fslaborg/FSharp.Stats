# AGENTS.md

Guidance for AI coding agents working in this repository.

## What this repo is

`FSharp.Stats` is an F# library implementing statistical and machine learning methods (descriptive statistics, distributions, hypothesis tests, regression, clustering, ML algorithms, etc.).

This repo focuses on **statistical/ML methods**. The underlying numerical primitives — matrix math, linear algebra, vector operations, BLAS/LAPACK bindings — live in the reference library [**FsMath**](https://github.com/fslaborg/FsMath). When you need low-level numeric routines, prefer pulling them from FsMath rather than re-implementing them here. If something fundamental is missing in FsMath, raise it there instead of duplicating math primitives in this repo.

Source layout:
- [src/FSharp.Stats/](src/FSharp.Stats/) — main library
- [src/FSharp.Stats.Interactive/](src/FSharp.Stats.Interactive/) — `dotnet interactive` integration
- [tests/FSharp.Stats.Tests/](tests/FSharp.Stats.Tests/) — Expecto test suite
- [docs/](docs/) — fsdocs tutorials and examples that should stay in sync with public API changes
- [benchmarks/](benchmarks/) — BenchmarkDotNet benchmark projects and checked-in benchmark outputs

## Building

This repo uses a **FAKE** build project ([build/build.fsproj](build/build.fsproj), entrypoint [build/Build.fs](build/Build.fs)). Treat the FAKE targets as the build/test contract for final verification, CI parity, docs, packaging, and release work.

For inner-loop iteration, narrowly scoped raw `dotnet build` / `dotnet test --no-build --filter ...` commands are acceptable as a local optimization when they help you move faster. Do not stop there: before considering the work done or ready for PR, run the repository entrypoint and finish with `./build.sh RunTests` (or `build.cmd RunTests` on Windows).

Entry points:
- Windows: [build.cmd](build.cmd)
- Unix: [build.sh](build.sh)

Both forward arguments to `dotnet run --project ./build/build.fsproj <target>`.

### Targets

Defined across [build/Build.fs](build/Build.fs), [build/BasicTasks.fs](build/BasicTasks.fs), [build/TestTasks.fs](build/TestTasks.fs), [build/PackageTasks.fs](build/PackageTasks.fs), [build/DocumentationTasks.fs](build/DocumentationTasks.fs), [build/ReleaseTasks.fs](build/ReleaseTasks.fs), [build/ReleaseNotesTasks.fs](build/ReleaseNotesTasks.fs):

| Target | Purpose |
|---|---|
| `Clean` | Remove `src/**/bin`, `src/**/obj`, `tests/**/bin`, `tests/**/obj`, `pkg`. |
| `Build` *(default)* | `dotnet build` the solution (Release). Depends on `Clean`. |
| `RunTests` | `dotnet test` the test project with detailed console logger. Depends on `Clean`, `Build`. |
| `RunTestsWithCodeCov` | Same as `RunTests` plus AltCover Cobertura output to `codeCov.xml`. |
| `Pack` / `PackPrerelease` | Produce NuGet packages into `pkg/`. Prompts interactively for confirmation. |
| `BuildDocs` / `BuildDocsPrerelease` | `fsdocs build --eval --clean` against the project. |
| `WatchDocs` / `WatchDocsPrerelease` | `fsdocs watch` for local doc preview. |
| `SetPrereleaseTag` | Reads a prerelease suffix from stdin and sets package version metadata. |
| `ReleaseDocs` / `PrereleaseDocs` | Push built docs. |
| `CreateTag` / `CreatePrereleaseTag`, `PublishNuget` / `PublishNugetPrerelease` | Tag git, push package to NuGet. |
| `UpdateReleaseNotes` | Regenerate `RELEASE_NOTES.md` from commits since the last release. |
| `Release` | Aggregate: `Clean → Build → RunTests → Pack → BuildDocs → CreateTag → PublishNuget → ReleaseDocs`. |
| `PreRelease` | Aggregate prerelease variant of `Release`. |
| `ReleaseNoDocs` / `PreReleaseNoDocs` | Release aggregates without doc steps. |

Common usage:

```sh
./build.sh                 # default: Build
./build.sh RunTests
./build.sh BuildDocs
./build.sh WatchDocs
```

`Pack` and the `Release*` targets are interactive (prompt for confirmation, prerelease suffix, etc.) — do not run them in non-interactive automation.

## F# project files and compile order

F# file order is load-bearing in this repo. If you add, remove, rename, or move a `.fs` file, you must update the corresponding project file and place it in the correct compile order:

- [src/FSharp.Stats/FSharp.Stats.fsproj](src/FSharp.Stats/FSharp.Stats.fsproj) — main library compile order
- [tests/FSharp.Stats.Tests/FSharp.Stats.Tests.fsproj](tests/FSharp.Stats.Tests/FSharp.Stats.Tests.fsproj) — test compile order

An otherwise correct code change can fail to compile if the new file is missing from the project file or inserted in the wrong slot.

## Adding a new statistical / ML method

When a PR or commit introduces a new statistical method (test, estimator, distribution, ML algorithm, etc.), it is expected to cite a **reference implementation** so reviewers can validate numerics. The current repo is not fully uniform about this yet, but new method work should follow this rule — undocumented numeric code is effectively unreviewable.

What to include:

1. **Link to a canonical reference implementation** in the PR description and/or in a comment above the function. Acceptable references (in rough order of preference):
   - R (`stats`, `MASS`, CRAN packages) — link to source or function docs.
   - Python (`numpy`, `scipy.stats`, `scikit-learn`, `statsmodels`) — link to source on GitHub or stable docs.
   - A peer-reviewed paper (DOI) when no canonical implementation exists.

2. **A small reproducible script** in the reference language that produces the expected numbers. Put it either:
   - Inline in the PR description (preferred for review), and/or
   - As a comment block above the corresponding test in [tests/FSharp.Stats.Tests/](tests/FSharp.Stats.Tests/), so the expected values in the test are traceable.

3. **Tests that pin the numbers from that script.** The test should assert the same values the reference script produces (within an explicit tolerance), and the comment should make the provenance obvious.

Example comment style for a test:

```fsharp
// Reference: scipy.stats.shapiro
// https://github.com/scipy/scipy/blob/v1.13.0/scipy/stats/_morestats.py
//
// >>> from scipy import stats
// >>> stats.shapiro([1.0, 2.0, 3.0, 4.0, 5.0])
// ShapiroResult(statistic=0.9868...,  pvalue=0.9672...)
let ``shapiro matches scipy on [1..5]`` () = ...
```

If you cannot find a reference implementation, say so explicitly in the PR and propose how the numbers were validated (hand derivation, paper, cross-check against another method). Do not silently ship unverified numerics.

## Conventions

- Match the surrounding F# style; prefer adding to existing modules over creating new top-level ones.
  - The code styling in this repo changed over time. Follow the style of the area you are editing, not necessarily the style of the oldest code in the repo.
  - For older functional and nested-module style, see [src/FSharp.Stats/Correlation.fs](src/FSharp.Stats/Correlation.fs) and [src/FSharp.Stats/Quantile.fs](src/FSharp.Stats/Quantile.fs).
  - For newer ergonomic APIs with static members, overloads, and optional parameters, see [src/FSharp.Stats/Integration/Integration.fs](src/FSharp.Stats/Integration/Integration.fs), [src/FSharp.Stats/Signal/QQPlot.fs](src/FSharp.Stats/Signal/QQPlot.fs), [src/FSharp.Stats/Testing/ConfusionMatrix.fs](src/FSharp.Stats/Testing/ConfusionMatrix.fs), and [src/FSharp.Stats/Fitting/LinearRegression.fs](src/FSharp.Stats/Fitting/LinearRegression.fs).
  - When adding new ergonomic APIs, prefer a two-tier shape: a core implementation that takes all parameters explicitly, plus overloads or convenience entrypoints for common defaults.
- If you change public API or user-facing behavior, update the relevant docs script in [docs/](docs/) and keep XML documentation comments in sync. Good public API examples to mirror include [src/FSharp.Stats/Integration/Integration.fs](src/FSharp.Stats/Integration/Integration.fs) and [src/FSharp.Stats/Fitting/LinearRegression.fs](src/FSharp.Stats/Fitting/LinearRegression.fs).
- Run `./build.sh RunTests` before opening a PR.
- Target the `developer` branch with PRs.
- Avoid churning checked-in benchmark output under `benchmarks/**/BenchmarkDotNet.Artifacts` unless you are intentionally refreshing benchmark results.
- Keep PRs focused — one method (or one tightly-related family) per PR makes the reference-implementation review tractable.
- Absolutely no changes to code should come without (regression) tests, even if no reference implementation is available. If you add code, you must add tests that validate it.
