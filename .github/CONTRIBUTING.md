# Contributing to FSharp.Stats

First of all, we are really happy that you are reading this, because this means that you are considering contributing to our repositories, which is awesome. 🎉👍

The following is a set of guidelines for contributing to repositories which are hosted in the fslab organization on GitHub. These guidelines are subject to changes, and therefore should not be thought about as strict rules. Feel free to suggest changes with a pull request to this file

**If you just want to know how to start contributing without reading all of this stuff, go [here](#Starting-point-for-first-time-contributors) for a quickstart guide, or [here](#How-can-i-contribute) for more indepth information**

## Table of contents

[Who we are](#Who-we-are)

[Repository structure and scope](#Repository-structure-and-scope)

* [Projects](#Projects)

* [Issue and pull request tags](#Issue-and-pull-request-labels)

* [Build process](#Build-process)

[How can i contribute?](#How-can-i-contribute)

* [Bug reports](#Bug-reports)

* [Feature requests](#Feature-requests)

* [Pull Requests](#Pull-Requests)

* [Starting point for first-time contributors](#Starting-point-for-first-time-contributors)

[Style guidelines](#Style-guidelines)

* [Git Commit Messages](#Git-Commit-Messages)

* [Coding conventions](#Coding-conventions)

* [Documentation guidelines](#Documentation-guidelines)

## Code of Conduct

This project and everyone participating in it is governed by the [CSBiology Code of Conduct](../CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to [info@biofsharp.com](mailto:info@biofsharp.com).

## Who we are

FSharp.Stats was initialized by [Prof. Dr. Timo Mühlhaus](https://scholar.google.com/citations?user=hKrd6ocAAAAJ&hl=en&oi=ao), head of the [CSBiology](https://github.com/CSBiology) group at the [RPTU Kaiserslautern](https://rptu.de/).

The main focus of our group is the application and development of computational methods to process and integrate quantitative biological data from modern high-throughput measurements to gain novel insights into plant acclimation responses. Therefore, we want to drive theory and technology forward with a combination of biological science, applied informatics and statistical approaches.

However, the scope of our projects is not limited to our particular research topic.

We mainly develop in the [F# programming language](https://fsharp.org/), which we see as perfectly suited for large scale data analysis.
All of our repositories are the result of our active research, which we make publicly available by going open source.

In 2020 under supervision of [bvenn](https://github.com/bvenn) the project was migrated to [fslab.org](https://fslab.org/), the F# community project incubation space for data science.

## Repository structure and scope

One part of our research activity is data analysis. Therefore, projects that are used in that process may be subject to a burst of changes in a brief period of time.

* `developer` branch: As the name suggests, this is where the bulk development work is done. All changes and pull request should target this branch.

* `master` branch: This is the place for stable releases, that are available at [nuget.org](https://www.nuget.org/packages/FSharp.Stats/). The `developer` branch will be merged into this branch on a semi-regular basis.

### Issue and pull request labels

|Label|Description|
|---|---|
| bug | Confirmed bugs or reports that are very likely to be bugs |
| duplicate| Issues which are duplicates of other issues, i.e. they have been reported before.|
| documentation | Issues concerned with the docs of the project |
| FeatureRequest | The name says it all. |
| invalid | Issues which aren't valid (e.g. user errors). |
| priority-*| `low`, `medium`, or `high`. high means that the CSBiology core team is actively working on this issue|
| project-*| Signals the respective subproject this issue is about|
| question | Questions more than bug reports or feature requests (e.g. how do I do X).|
| up-for-grabs| Issues with this label show up on https://up-for-grabs.net if the respective project is registered there. Marks good issues for contributig to the project.|
| wontfix| The CSBiology core team has decided not to fix these issues for now, either because they're working as intended or for some other reason. |

### Build process

To build our projects, you will need [.NET Core SDK](https://dotnet.microsoft.com/download).

All projects have at least the following build targets:

|Target|Description|Command Line Arguments|
|---|---|---|
|build|Builds the binaries |./build|
|watchdocs|Uses Build. Afterwards creates a live view of the documentation with proper style and relative paths. Use this to test documentation. | ./build watchdocs|

## How can i contribute?

### Bug reports

This section guides you through submitting a bug report for our repositories. Following these guidelines helps maintainers and the community understand your report, reproduce the behavior, and find related reports.

Before creating bug reports, please check [this list](#before-submitting-a-bug-report) as you might find out that you don't need to create one. When you are creating a bug report, please [include as many details as possible](#how-do-i-submit-a-good-bug-report). Fill out [the required template](ISSUE_TEMPLATE/bug_report.md), the information it asks for helps us resolve issues faster.

> **Note:** If you find a **Closed** issue that seems like it is the same thing that you're experiencing, open a new issue and include a link to the original issue in the body of your new one.

#### Before Submitting A Bug Report

* **Perform a [cursory search](https://github.com/search?q=+is:issue+user:fslaborg)** to see if the problem has already been reported. If it has **and the issue is still open**, add a comment to the existing issue instead of opening a new one.

#### How Do I Submit A (Good) Bug Report?

Bugs are tracked as [GitHub issues](https://guides.github.com/features/issues/). After you've determined [which repository](#Projects) your bug is related to, create an issue on that repository and provide the following information by filling in [the template](ISSUE_TEMPLATE/bug_report.md).

Explain the problem and include additional details to help maintainers reproduce the problem:

* **Use a clear and descriptive title** for the issue to identify the problem.
* **Describe the exact steps which reproduce the problem** in as many details as possible. When listing steps, **don't just say what you did, but explain how you did it**.
* **Provide specific examples to demonstrate the steps**. Include links to files or GitHub projects, or copy/pasteable snippets, which you use in those examples. If you're providing snippets in the issue, use [Markdown code blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the behavior you observed after following the steps** and point out what exactly is the problem with that behavior.
* **Explain which behavior you expected to see instead and why.**
* **Include screenshots** which show you following the described steps and clearly demonstrate the problem. 
* **If the problem wasn't triggered by a specific action**, describe what you were doing before the problem happened and share more information using the guidelines below.

Provide more context by answering these questions:

* If the problem started happening recently, **can you reproduce the problem in an older version of the project?** What's the most recent version in which the problem doesn't happen?
* **Can you reliably reproduce the issue?** If not, provide details about how often the problem happens and under which conditions it normally happens.

Include details about your configuration and environment:

* **What's the name and version of the OS you're using**?
* **Which version of .NET Core SDK are you using?**
* **Are you encountering the bug in a virtual machine?** If so, which VM software are you using and which operating systems and versions are used for the host and the guest?

### Feature requests

This section guides you through submitting a feature request, including completely new features and minor improvements to existing functionality. Following these guidelines helps maintainers and the community understand your suggestion and find related suggestions.

Before creating feature requests, please check [this list](#before-submitting-an-enhancement-suggestion) as you might find out that you don't need to create one. When you are creating an enhancement suggestion, please [include as many details as possible](#how-do-i-submit-a-good-enhancement-suggestion). Fill in [the template](ISSUE_TEMPLATE/feature_request.md), including the steps that you imagine you would take if the feature you're requesting existed.

#### Before Submitting An Enhancement Suggestion

* **Perform a [cursory search](https://github.com/search?q=+is%3Aissue+user:CSBiology)** to see if the enhancement has already been suggested. If it has, add a comment to the existing issue instead of opening a new one.

#### How Do I Submit A (Good) Enhancement Suggestion?

Feature requests are tracked as [GitHub issues](https://guides.github.com/features/issues/). After you've determined [which repository](#Projects) your enhancement suggestion is related to, create an issue on that repository and provide the following information:

* **Use a clear and descriptive title** for the issue to identify the suggestion.
* **Provide a step-by-step description of the suggested enhancement** in as many details as possible.
* **Provide specific examples to demonstrate the steps**. Include copy/pasteable snippets which you use in those examples, as [Markdown code blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the current behavior** and **explain which behavior you expected to see instead** and why.
* **Include screenshots** which help you demonstrate the steps which the suggestion is related to.
* **Explain why this feature would be useful**
* **Specify the name and version of the OS you're using.**

### Pull Requests

Please follow these steps to have your contribution considered by the maintainers:

1. Follow all instructions in [the template](../PULL_REQUEST_TEMPLATE.md)
2. Follow the [styleguides](#Style-guidelines)
3. After you submit your pull request, verify that all [status checks](https://help.github.com/articles/about-status-checks/) are passing <details><summary>What if the status checks are failing?</summary>If a status check is failing, and you believe that the failure is unrelated to your change, please leave a comment on the pull request explaining why you believe the failure is unrelated.</details>

### Starting point for first-time contributors

1. [Submit an issue](#Feature-requests) related to the contribution you want to make. Please do not submit pull request that have no corresponding issue.
2. Create a fork of the respective project and create a new branch (ideally named after the contribution you want to make)
3. Keep your commits granular and concise, so that your commit history is easily trackable by reviewers. See [here](#Git-Commit-Messages) for basic guidelines about git commit messages
4. Group like-changes in one commit, and avoid to combine different types of changes in a single commit.
5. Base pull requests on the `developer` branch of the respective project
6. Fill out the template which will show up when creating a pull request
7. Thank you for contributing :heart:

## Style guidelines

### Git Commit Messages

* Use the present tense ("add feature" not "added feature")
* Use the imperative mood ("move cursor to..." not "moves cursor to...")
* Limit the first line to 72 characters or less
* Reference issues and pull requests liberally after the first line

### Coding conventions

As most of our members are more or less self-taught programmers, we are not used to obey strict coding conventions. We try to obey the [general F# style guide](https://docs.microsoft.com/de-de/dotnet/fsharp/style-guide/), but there could be exceptions. Suggestions in this field are highly appreciated.

One of our biggest weakness at the moment are unit tests. We will love everyone helping us out on that front.

### Documentation guidelines

The docs in our repos are built by the [F# Formatting Documentation tools](http://fsprojects.github.io/FSharp.Formatting/) which parse `.fsx` files in the docsrc/content folder to generate html files. Again, no strict rules here, just a few minor guidelines:

* If you add or change functionality to a project, please consider adding/changing the respective documentation script. If you dont have time to do that, please comment it on your pull request.

* Most documentation is written in a tutorial-style manner. If you use external files in your examples, put them into docsrc/content/data.

* API References are built from triple frontslash comments (`///`), please consider adding those above the functions/types/classes you are adding.

## Additional notes

This document is adapted from the [atom contribution guidelines](https://github.com/atom/atom/blob/master/CONTRIBUTING.md#how-do-i-submit-a-good-bug-report)