
module Froto.Roslyn

open Froto.Parser
open Roslyn.Compilers
open Roslyn.Compilers.CSharp
open System.IO
open System.Collections.Generic
open System.Diagnostics
open Roslyn.Services
open Roslyn.Services.Formatting

/// get all namespaces in a compilation under the root name 
let namespaces name (compilation:Compilation) =
    let list = List()
    let gnsMembers = compilation.GlobalNamespace.GetMembers(name).AsEnumerable() |> List.ofSeq
    if gnsMembers.Length = 1 then
        let nsRoot = gnsMembers.[0] :?> NamespaceSymbol
        let rec add (ns:NamespaceSymbol) =
            list.Add ns
            ns.GetNamespaceMembers() |> Seq.iter (fun m -> add m)
        add nsRoot
    list

let createCompilation (path:string) =

    //let proto = ProtoParser.parseProtoFile path
    
    let st = SyntaxTree.ParseText("""
        using System;

        namespace Tutorial.Blah
        {
            public class AddressBookProto
            {
                public string Name = "Hello World";
            }
        }
        """)

    
    let root2 = st.GetRoot().AddUsings([| Syntax.UsingDirective(Syntax.ParseName("Protobuf")) |])
    let st2 = SyntaxTree.Create root2

    let fn = st2.GetRoot().Format(FormattingOptions.GetDefaultOptions()).GetFormattedRoot()

    Debug.WriteLine fn


    let options = CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
    
    let compilation =
        Compilation.Create("proto", options=options)
            .AddSyntaxTrees([st])
            .AddReferences(
                [|
                    MetadataReference.CreateAssemblyReference("mscorlib")
                |]
            )

    compilation