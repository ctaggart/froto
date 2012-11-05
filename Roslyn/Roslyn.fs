
module Froto.Roslyn

open Froto.Parser
open Roslyn.Compilers
open Roslyn.Compilers.CSharp
open System.IO

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