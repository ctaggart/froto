
module Froto.Roslyn.RoslynTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open System.Reflection
open System.Diagnostics

/// gets the path for a test file based on the relative path from the executing assembly
let getTestFile file =
     let codeBase = Reflection.Assembly.GetExecutingAssembly().CodeBase
     let assemblyPath = DirectoryInfo (Uri codeBase).LocalPath
     let solutionPath = (assemblyPath.Parent.Parent.Parent.Parent).FullName
     Path.Combine(solutionPath, Path.Combine("test",file))

[<Fact>]
let ``test createCompilation`` () =
    let path = getTestFile "addressbook1.proto"
    let cmp = createCompilation path

    let ms = new MemoryStream()
    let emitResult = cmp.Emit(ms)
    if not emitResult.Success then
        failwithf "unable to emit: %A" emitResult.Diagnostics

    let assembly = Assembly.Load(ms.GetBuffer())
    let t = assembly.GetType("Tutorial.Blah.AddressBookProto")

    "Tutorial.Blah.AddressBookProto" |> should equal t.FullName

    ()

[<Fact>]
let ``address1 proto creates types`` () =
    let path = getTestFile "addressbook1.proto"
    let cmp = createCompilation path
    let nsList = namespaces "Tutorial" cmp
    2 |> should equal nsList.Count
    "Tutorial.Blah" |> should equal (nsList.[1].ToString())
    
    ()