
[<Xunit.Trait("Kind", "Unit")>]
module Froto.Roslyn.RoslynTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open System.Reflection
open System.Diagnostics
open Froto.Roslyn

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Formatting

// aliases
module Cmp = Compilation
module CU = CompilationUnitSyntax
module NS = NamespaceDeclarationSyntax
module CD = ClassDeclarationSyntax

/// gets the path for a test file based on the relative path from the executing assembly
let getTestFile file =
     let codeBase = Reflection.Assembly.GetExecutingAssembly().CodeBase
     let assemblyPath = DirectoryInfo (Uri codeBase).LocalPath
     let solutionPath = (assemblyPath.Parent.Parent.Parent.Parent).FullName
     Path.Combine(solutionPath, Path.Combine("test",file))

[<Fact>]
let ``test creating a type`` () =
    let assembly =
        Cmp.createDll "tutorial"
        |> Cmp.addReference typeof<Object> // mscorlib
        |> Cmp.addSyntaxTree (
            CU.Empty
            |> CU.addMember (
                NS.create "Tutorial"
                |> NS.addMember (
                    CD.create "Person"
                    |> CD.addModifier Keyword.Public
                )
            )
            |> CU.createSyntaxTree
        )
        |> Cmp.emitAssembly
    assembly.GetType "Tutorial.Person" |> should not' (be Null)

[<Fact>]
let ``address1 proto creates types`` () =
    let path = getTestFile "addressbook1.proto"
    let cmp = ProtoGen.createCompilation path "Froto.Roslyn.RoslynTests" "AddressbookProto"
    cmp |> Cmp.syntaxTrees |> Seq.iter (fun st ->
        Debug.WriteLine <| sprintf "%A" st
    )
    let assembly = cmp |> Cmp.emitAssembly
    assembly.GetType "tutorial.Person" |> should not' (be Null)
    assembly.GetType "tutorial.AddressBook" |> should not' (be Null)
    assembly.GetType "tutorial.PhoneNumber" |> should not' (be Null)
    assembly.GetType "tutorial.PhoneType" |> should not' (be Null)

[<Fact>]
let ``riak proto create source code`` () =
    let path = getTestFile "riak.proto"
    ProtoGen.generate path "Riak" "RiakProto" "riak.cs"