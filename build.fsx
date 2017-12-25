#I "packages/FAKE/tools"
#r "FakeLib.dll"

open System
open Fake
open Fake.AppVeyor
open Fake.Core
open Fake.Core.BuildServer
open Fake.Core.Globbing.Operators
open Fake.Core.Process
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.DotNet.NuGet.NuGet
// open Fake.DotNet.Testing.XUnit2

type Text.StringBuilder with
    member x.Appendf format = Printf.ksprintf (fun s -> x.Append s |> ignore) format

let release = ReleaseNotesHelper.LoadReleaseNotes "release_notes.md"
let isAppVeyorBuild = match buildServer with AppVeyor -> true | _ -> false

let isVersionTag tag = Version.TryParse tag |> fst
let hasRepoVersionTag = isAppVeyorBuild && AppVeyorEnvironment.RepoTag && isVersionTag AppVeyorEnvironment.RepoTagName
let assemblyVersion = if hasRepoVersionTag then AppVeyorEnvironment.RepoTagName else release.NugetVersion
let buildDate = DateTime.UtcNow
let buildVersion =
    if hasRepoVersionTag then assemblyVersion
    else if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion (Int32.Parse(AppVeyorEnvironment.BuildNumber).ToString("000"))
    else sprintf "%s-a%s" assemblyVersion (buildDate.ToString "yyMMddHHmm")
let mutable configuration = "Release"

Target.Create "BuildVersion" <| fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore

Target.Create "Clean" <| fun _ -> !! "**/bin/" ++ "**/obj/" |> DeleteDirs

Target.Create "AssemblyInfo" <| fun _ ->
    let iv = Text.StringBuilder() // json
    iv.Appendf "{\\\"buildVersion\\\":\\\"%s\\\"" buildVersion
    iv.Appendf ",\\\"buildDate\\\":\\\"%s\\\"" (buildDate.ToString "yyyy'-'MM'-'dd'T'HH':'mm':'sszzz")
    if isAppVeyorBuild then
        iv.Appendf ",\\\"gitCommit\\\":\\\"%s\\\"" AppVeyor.AppVeyorEnvironment.RepoCommit
        iv.Appendf ",\\\"gitBranch\\\":\\\"%s\\\"" AppVeyor.AppVeyorEnvironment.RepoBranch
    iv.Appendf "}"
    let common = [
        DotNet.AssemblyInfo.Version assemblyVersion
        DotNet.AssemblyInfo.InformationalVersion (iv.ToString()) ]
    common |> AssemblyInfoFile.CreateFSharp "Parser/AssemblyInfo.fs"
    common |> AssemblyInfoFile.CreateFSharp "Serialization/AssemblyInfo.fs"
    common |> AssemblyInfoFile.CreateFSharp "Compiler/AssemblyInfo.fs"
    common |> AssemblyInfoFile.CreateFSharp "TypeProvider/AssemblyInfo.fs"

Target.Create "SwitchToDebug" <| fun _ ->
    configuration <- "Debug"

Target.Create "Build" <| fun _ ->
    let build project = DotNetCli.Build (fun p -> { p with Project = project })
    build "Froto.sln"
    // build "Froto.TypeProvider.TestAndDocs.sln"

Target.Create "UnitTest" <| fun _ ->
    let test project = DotNetCli.Test (fun p -> { p with Project = project })
    test "Parser.Test/Froto.Parser.Test.fsproj"
    test "Serialization.Test/Froto.Serialization.Test.fsproj"

// https://github.com/fsharp/FAKE/blob/master/help/markdown/dotnet-nuget.md
Target.Create "NuGet" <| fun _ ->
    IO.Directory.create "bin"
    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "Parser/bin/Release"
        OutputPath = "bin"
        Dependencies =
            [
            // "FParsec", GetPackageVersion "./packages/" "FParsec"
            "FParsec", "1.0.3" // TODO can we get it from Froto.Parser.fsproj PackageReference?
            ]
    }) "Parser/Froto.Parser.nuspec"

    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "Serialization/bin/Release"
        OutputPath = "bin"
    }) "Serialization/Froto.Serialization.nuspec"

    // NuGet (fun p ->
    // { p with
    //     Version = buildVersion
    //     WorkingDir = "Compiler/bin/Release"
    //     OutputPath = "bin"
    // }) "Compiler/Froto.Compiler.nuspec"

    NuGet (fun p ->
    { p with
        Version = buildVersion
        WorkingDir = "TypeProvider/bin/Release"
        OutputPath = "bin"
        Dependencies =
        [
            "FParsec", "1.0.3"
            "Froto.Parser", sprintf "[%s]" buildVersion 
            "Froto.Serialization", sprintf "[%s]" buildVersion 
        ]
    }) "TypeProvider/Froto.TypeProvider.nuspec"

Target.Create "Start" Target.DoNothing
Target.Create "Default" Target.DoNothing
Target.Create "Debug" Target.DoNothing

// chain targets together only on AppVeyor
//let (==>) a b = a =?> (b, isAppVeyorBuild)

"Start"
=?> ("BuildVersion", isAppVeyorBuild)
=?> ("AssemblyInfo", isAppVeyorBuild)
==> "Build"
==> "UnitTest"
=?> ("NuGet", isAppVeyorBuild)
==> "Default"

"UnitTest" ==> "Debug"
"SwitchToDebug" ==> "Debug"
"SwitchToDebug" ?=> "Build"

Target.RunOrDefault "Default"