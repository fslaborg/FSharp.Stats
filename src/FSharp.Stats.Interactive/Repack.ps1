# Clean up the previously-cached NuGet packages.
# Lower-case is intentional (that's how nuget stores those packages).
Remove-Item -Recurse ~\.nuget\packages\fsharp.stats.interactive* -Force
Remove-Item -Recurse ~\.nuget\packages\fsharp.stats* -Force

# build and pack Interactive 
cd ../../
dotnet restore FSharp.Stats.sln
dotnet build FSharp.Stats.sln
dotnet pack -c Release -p:PackageVersion=0.0.1-dev -o "./pkg" FSharp.Stats.sln
cd src/FSharp.Stats.Interactive
