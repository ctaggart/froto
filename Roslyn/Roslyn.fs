
namespace Froto.Roslyn

open Froto.Parser
open Roslyn.Compilers
open Roslyn.Compilers.CSharp
open System
open System.IO
open System.Collections.Generic
open Roslyn.Services
open Roslyn.Services.Formatting

module NameSyntax = 
    let create name =
        Syntax.ParseName name

module Nm = NameSyntax

module NamespaceDeclarationSyntax =
    let create name =
        Syntax.NamespaceDeclaration <| Nm.create name

    let addMember m (ns:NamespaceDeclarationSyntax) =
        ns.AddMembers [| m |]

    let addMembers (mbs:MemberDeclarationSyntax list) (ns:NamespaceDeclarationSyntax) =
        ns.AddMembers(mbs |> List.toArray)

module NS = NamespaceDeclarationSyntax

module CompilationUnitSyntax =
    let Empty =
        Syntax.ParseCompilationUnit String.Empty

    let addUsing name (cu:CompilationUnitSyntax) =
        cu.AddUsings [| Syntax.UsingDirective <| Nm.create name |]

    let addMember m (cu:CompilationUnitSyntax) =
        cu.AddMembers [| m |]

    let formatDefault (cu:CompilationUnitSyntax) =
        cu.Format(FormattingOptions.GetDefaultOptions()).GetFormattedRoot() :?> CompilationUnitSyntax

    let createSyntaxTree (cu:CompilationUnitSyntax) =
        SyntaxTree.Create cu

module CU = CompilationUnitSyntax

module SyntaxNode =
    let descendants (sn:SyntaxNode) : SyntaxNode seq =
        sn.DescendantNodes((null:Func<SyntaxNode,bool>))

module Keyword =
    let Public = Syntax.Token SyntaxKind.PublicKeyword
    let Private = Syntax.Token SyntaxKind.PrivateKeyword
    let Get = Syntax.Token SyntaxKind.PrivateKeyword

module ClassDeclarationSyntax =
    let create name =
        Syntax.ClassDeclaration (name:string)

    let addModifier m (cd:ClassDeclarationSyntax) =
        cd.AddModifiers [| m |]

    let addMember m (cd:ClassDeclarationSyntax) =
        cd.AddMembers [| m |]

module CD = ClassDeclarationSyntax

module Compilation =
    let createDll name = 
        Compilation.Create(name, options=CompilationOptions(OutputKind.DynamicallyLinkedLibrary))

    let addReference name (cmp:Compilation) =
        cmp.AddReferences [| MetadataReference.CreateAssemblyReference(name) |]

    let addSyntaxTree (st:SyntaxTree) (cmp:Compilation) =
        cmp.AddSyntaxTrees [ st ]

    let emitStream (cmp:Compilation) =
        let ms = new MemoryStream()
        let result = cmp.Emit ms
        if not result.Success then
            failwithf "emitAssembly failed: %A" result.Diagnostics
        ms

    let emitAssembly cmp =
        use ms = emitStream cmp
        Reflection.Assembly.Load(ms.GetBuffer())

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

    let syntaxTrees (cmp:Compilation) =
        cmp.SyntaxTrees.AsEnumerable() |> List.ofSeq

module Cmp = Compilation

// TODO
module ProtoGen =
    let createCompilation path =

        let proto = ProtoParser.parseProtoFile path
        
        let cds = proto.Messages |> List.map (fun msg ->
            CD.create msg.Name
            |> CD.addModifier Keyword.Public
            :> MemberDeclarationSyntax
        )

        let ns =
            NS.create proto.Packages.[0]
            |> NS.addMembers cds

        let st =
            CU.Empty
            //|> CU.addUsing "Protobuf"
            |> CU.addMember ns
            |> CU.formatDefault
            |> CU.createSyntaxTree

        Cmp.createDll "proto"
        |> Cmp.addReference "mscorlib"
        |> Cmp.addSyntaxTree st
