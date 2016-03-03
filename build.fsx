#I "packages/FAKE/tools"
#r "FakeLib.dll"
#load "packages/SourceLink.Fake/tools/SourceLink.fsx"

open System
open System.IO
open Fake
open Fake.AppVeyor
open Fake.AssemblyInfoFile
open Fake.Testing
open SourceLink

let release = ReleaseNotesHelper.LoadReleaseNotes "release_notes.md"
let isAppVeyorBuild = buildServer = BuildServer.AppVeyor
let isVersionTag tag = Version.TryParse tag |> fst
let hasRepoVersionTag = isAppVeyorBuild && AppVeyorEnvironment.RepoTag && isVersionTag AppVeyorEnvironment.RepoTagName
let assemblyVersion = if hasRepoVersionTag then AppVeyorEnvironment.RepoTagName else release.NugetVersion
let buildDate = DateTime.UtcNow
let buildVersion =
    if hasRepoVersionTag then assemblyVersion
    else if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion (Int32.Parse(AppVeyorEnvironment.BuildNumber).ToString("000"))
    else sprintf "%s-a%s" assemblyVersion (buildDate.ToString "yyMMddHHmm")

MSBuildDefaults <- { MSBuildDefaults with Verbosity = Some MSBuildVerbosity.Minimal }

Target "BuildVersion" <| fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore

Target "Clean" <| fun _ -> !! "**/bin/" ++ "**/obj/" |> DeleteDirs

Target "AssemblyInfo" <| fun _ ->
    let iv = Text.StringBuilder() // json
    iv.Appendf "{\\\"buildVersion\\\":\\\"%s\\\"" buildVersion
    iv.Appendf ",\\\"buildDate\\\":\\\"%s\\\"" (buildDate.ToString "yyyy'-'MM'-'dd'T'HH':'mm':'sszzz")
    if isAppVeyorBuild then
        iv.Appendf ",\\\"gitCommit\\\":\\\"%s\\\"" AppVeyor.AppVeyorEnvironment.RepoCommit
        iv.Appendf ",\\\"gitBranch\\\":\\\"%s\\\"" AppVeyor.AppVeyorEnvironment.RepoBranch
    iv.Appendf "}"
    let common = [
        Attribute.Version assemblyVersion
        Attribute.InformationalVersion iv.String ]
    common |> CreateFSharpAssemblyInfo "ProtoParser/AssemblyInfo.fs"
    common |> CreateFSharpAssemblyInfo "Core/AssemblyInfo.fs"
    common |> CreateFSharpAssemblyInfo "Roslyn/AssemblyInfo.fs"
    common |> CreateFSharpAssemblyInfo "Exe/AssemblyInfo.fs"

Target "Build" <| fun _ ->
    !! "Froto.sln" |> MSBuildRelease "" "Rebuild" |> ignore

Target "UnitTest" <| fun _ ->
    CreateDir "bin"
    xUnit2 (fun p ->
        { p with
            IncludeTraits = ["Kind", "Unit"]
            XmlOutputPath = Some @"bin/UnitTest.xml"
            Parallel = ParallelMode.All
        })
        [   @"ProtoParser.Test/bin/Release/Froto.Parser.Test.dll"
            @"Froto.Core.Test/bin/Release/Froto.Core.Test.dll"
            @"Roslyn.Test/bin/Release/Froto.Roslyn.Test.dll"
        ]

Target "SourceLink" <| fun _ ->
    let sourceIndex proj pdb =
        let p = VsProj.LoadRelease proj
        let pdbToIndex = if Option.isSome pdb then pdb.Value else p.OutputFilePdb
        let url = "https://raw.githubusercontent.com/ctaggart/froto/{0}/%var2%"
        SourceLink.Index p.Compiles pdbToIndex __SOURCE_DIRECTORY__ url
    sourceIndex "ProtoParser/Froto.Parser.fsproj" None
    sourceIndex "Core/Froto.Core.fsproj" None
    sourceIndex "Roslyn/Froto.Roslyn.fsproj" None
    sourceIndex "Exe/Exe.fsproj" None

Target "NuGet" <| fun _ ->
    CreateDir "bin"
    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "ProtoParser/bin/Release"
        OutputPath = "bin"
        DependenciesByFramework =
        [{
            FrameworkVersion = "net45"
            Dependencies =
                [
                "FParsec", GetPackageVersion "./packages/" "FParsec"
                ]
        }]
    }) "ProtoParser/Froto.Parser.nuspec"

    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "Core/bin/Release"
        OutputPath = "bin"
        DependenciesByFramework =
        [{
            FrameworkVersion = "net45"
            Dependencies =
                [
                ]
        }]
    }) "Core/Froto.Core.nuspec"

    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "Roslyn/bin/Release"
        OutputPath = "bin"
        DependenciesByFramework =
        [{
            FrameworkVersion = "net45"
            Dependencies =
                [
                "Froto.Parser", sprintf "[%s]" buildVersion // exact version
                "Microsoft.CodeAnalysis.CSharp.Workspaces", GetPackageVersion "./packages/" "Microsoft.CodeAnalysis.CSharp.Workspaces"
                ]
        }]
    }) "Roslyn/Froto.Roslyn.nuspec"

    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "Exe/bin/Release"
        OutputPath = "bin"
    }) "Exe/Froto.nuspec"

// chain targets together only on AppVeyor
//let (==>) a b = a =?> (b, isAppVeyorBuild)

"Clean"
=?> ("BuildVersion", isAppVeyorBuild)
=?> ("AssemblyInfo", isAppVeyorBuild)
==> "Build"
==> "UnitTest"
=?> ("SourceLink", isAppVeyorBuild)
==> "NuGet"

RunTargetOrDefault "NuGet"
