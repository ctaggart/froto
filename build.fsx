#I "packages/FAKE/tools"
#r "FakeLib.dll"

open System
open Fake
open Fake.AppVeyor
open Fake.AssemblyInfoFile
open Fake.Testing

type System.Text.StringBuilder with
    member x.Appendf format = Printf.ksprintf (fun s -> x.Append s |> ignore) format

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
let mutable configuration = "Release"

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
        Attribute.InformationalVersion (iv.ToString()) ]
    common |> CreateFSharpAssemblyInfo "Parser/AssemblyInfo.fs"
    common |> CreateFSharpAssemblyInfo "Serialization/AssemblyInfo.fs"
    common |> CreateFSharpAssemblyInfo "Compiler/AssemblyInfo.fs"
    common |> CreateFSharpAssemblyInfo "TypeProvider/AssemblyInfo.fs"

Target "SwitchToDebug" <| fun _ ->
    configuration <- "Debug"

Target "Build" <| fun _ ->
    ["Froto.sln"] //; "Froto.TypeProvider.TestAndDocs.sln"] TODO 
    |> MSBuild "" "restore;build" ["Configuration", configuration] 
    |> ignore

Target "UnitTest" <| fun _ ->
    CreateDir "bin"
    let dlls =
        [   sprintf @"Parser.Test/bin/%s/net46/Froto.Parser.Test.dll" configuration
            sprintf @"Serialization.Test/bin/%s/net46/Froto.Serialization.Test.dll" configuration
            sprintf @"TypeProvider.Test/bin/%s/net46/Froto.TypeProvider.Test.dll" configuration
        ]
    xUnit2 (fun p ->
        { p with
            IncludeTraits = ["Kind", "Unit"]
            XmlOutputPath = Some @"bin/UnitTest.xml"
            Parallel = ParallelMode.Assemblies
            TimeOut = TimeSpan.FromMinutes 10.0
        })
        dlls

Target "NuGet" <| fun _ ->
    CreateDir "bin"
    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "Parser/bin/Release"
        OutputPath = "bin"
        DependenciesByFramework =
        [{
            FrameworkVersion = "net45"
            Dependencies =
                [
                "FParsec", GetPackageVersion "./packages/" "FParsec"
                ]
        }]
    }) "Parser/Froto.Parser.nuspec"

    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "Serialization/bin/Release"
        OutputPath = "bin"
        DependenciesByFramework =
        [{
            FrameworkVersion = "net45"
            Dependencies =
                [
                ]
        }]
    }) "Serialization/Froto.Serialization.nuspec"

    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "Compiler/bin/Release"
        OutputPath = "bin"
    }) "Compiler/Froto.Compiler.nuspec"

    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "TypeProvider/bin/Release"
        OutputPath = "bin"
        DependenciesByFramework =
        [{
            FrameworkVersion = "net45"
            Dependencies =
            [
                "FParsec", GetPackageVersion "./packages/" "FParsec"
                "Froto.Parser", sprintf "[%s]" buildVersion 
                "Froto.Serialization", sprintf "[%s]" buildVersion 
            ]
        }]
    }) "TypeProvider/Froto.TypeProvider.nuspec"

Target "Default" DoNothing

Target "Debug" DoNothing

// chain targets together only on AppVeyor
//let (==>) a b = a =?> (b, isAppVeyorBuild)

"Clean"
=?> ("BuildVersion", isAppVeyorBuild)
=?> ("AssemblyInfo", isAppVeyorBuild)
==> "Build"
==> "UnitTest"
// =?> ("NuGet", not isMono)
==> "Default"

"UnitTest" ==> "Debug"
"SwitchToDebug" ==> "Debug"
"SwitchToDebug" ?=> "Build"


RunTargetOrDefault "Default"
