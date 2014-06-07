
module Froto.Roslyn.ProtoGen

open System
open System.IO
open Froto.Parser
open Froto.Parser.ProtoAst
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

// aliases
module NS = NamespaceDeclarationSyntax
module CU = CompilationUnitSyntax
module CD = ClassDeclarationSyntax
module ED = EnumDeclarationSyntax
module Cmp = Compilation
module MD = MethodDeclarationSyntax
module PD = PropertyDeclarationSyntax

// https://developers.google.com/protocol-buffers/docs/proto#scalar
let scalarToSyntaxKind =
    [   "double", TypeSyntax.double;
        "float", TypeSyntax.float;
        "int32", TypeSyntax.int;
        "int64", TypeSyntax.int64;
        "uint32", TypeSyntax.uint;
        "uint64", TypeSyntax.uint64;
        "sint32", TypeSyntax.int;
        "sint64", TypeSyntax.int64;
        "fixed32", TypeSyntax.uint;
        "fixed64", TypeSyntax.uint64;
        "sfixed32", TypeSyntax.int;
        "sfixed64", TypeSyntax.int64;
        "bool", TypeSyntax.bool;
        "string", TypeSyntax.string;
        "bytes", TypeSyntax.byteArray;
    ]
    |> Map.ofList

let createCompilation path rootNsName rootTpName =

    let proto = ProtoParser.parseProtoFile path
        
    let rec listMessages (messages:ProtoMessage list) =
        [   for message in messages do
                yield message
                yield! listMessages message.Messages 
        ]

    let rec listEnums (enums:ProtoEnum  list) (messages:ProtoMessage list) =
        [   yield! enums
            for message in messages do
                yield! listEnums message.Enums message.Messages
        ]

    let messages = listMessages proto.Messages

    let cds = messages |> List.map (fun msg ->
        CD.create msg.Name
        |> CD.addAttribute (
            AttributeSyntax.create "ProtoContract"
        )
        |> CD.addModifier Keyword.Public
        |> CD.addModifier Keyword.Sealed
        |> CD.addMembers (
            msg.Fields |> List.map (fun f ->
                let tp =
                    if scalarToSyntaxKind.ContainsKey f.Type then
                        scalarToSyntaxKind.[f.Type]
                    else
                        TypeSyntax.create f.Type

                PD.create f.Name tp
                |> PD.addModifier Keyword.Public
                :> MemberDeclarationSyntax
            )
        )
        :> MemberDeclarationSyntax
    )

    let enums = listEnums [] proto.Messages |> List.map (fun enum ->
        ED.create enum.Name
        |> ED.addAttribute (
            AttributeSyntax.create "ProtoContract"
        )
        |> ED.addModifier Keyword.Public
        |> ED.addMembers (enum.Items |> List.map (fun item -> EnumMemberDeclarationSyntax.create item.Name item.Value))
        :> MemberDeclarationSyntax
    )

    let nsName =
        if proto.Packages.Length > 0 then
            proto.Packages.[0]
        else
            rootNsName

    let ns =
        NS.create nsName
        |> NS.addMembers enums
        |> NS.addMembers cds

    let rootTp =
        CD.create rootTpName
        |> CD.addModifier Keyword.Public
        |> CD.addModifier Keyword.Static
        |> CD.fold messages (fun cd msg ->
            cd
            // Create
            |> CD.addMember (
                MD.create (TypeSyntax.create msg.Name) ("Create" + msg.Name)
                |> MD.addModifier Keyword.Public
                |> MD.addModifier Keyword.Static
                |> MD.addBodyStatements [
                    "return new " + msg.Name + "();" |> SyntaxFactory.ParseStatement
                ]
            )
            // Serialize
            |> CD.addMember (
                MD.create (TypeSyntax.vd) ("Serialize" + msg.Name)
                |> MD.addModifier Keyword.Public
                |> MD.addModifier Keyword.Static
                |> MD.addParameters [
                    ParameterSyntax.create (TypeSyntax.create "Stream") "stream"
                    ParameterSyntax.create (TypeSyntax.create msg.Name) "instance"
                ]
                |> MD.addBodyStatements [
                    "Serializer.Serialize(stream, instance);" |> SyntaxFactory.ParseStatement
                ]
            )
            // Deserialize
            |> CD.addMember (
                MD.create (TypeSyntax.create msg.Name) ("Deserialize" + msg.Name)
                |> MD.addModifier Keyword.Public
                |> MD.addModifier Keyword.Static
                |> MD.addParameters [
                    ParameterSyntax.create (TypeSyntax.create "Stream") "stream"
                ]
                |> MD.addBodyStatements [
                    "return Serializer.Deserialize<" + msg.Name + ">(stream);" |> SyntaxFactory.ParseStatement
                ]
            )
        )

    let rootNs =
        NS.create rootNsName
        |> NS.addUsing nsName
        |> NS.addMember rootTp

    let st =
        CU.Empty
        |> CU.addUsing "ProtoBuf"
        |> CU.addUsing "System.IO"
        |> CU.addMember ns
        |> CU.addMember rootNs
        |> CU.formatDefault
        |> CU.createSyntaxTree

    Cmp.createDll rootTpName
    |> Cmp.addReference typeof<Object> // mscorlib
    |> Cmp.addReference typeof<Attribute> // System.Runtime
    |> Cmp.addReference typeof<ProtoBuf.Serializer> // protobuf-net
    |> Cmp.addSyntaxTree st

let generate pathProto rootNamespace rootType pathCs =
    use sw = new StreamWriter(pathCs, false, Text.Encoding.UTF8)
    createCompilation pathProto rootNamespace rootType
    |> Cmp.syntaxTrees |> Seq.iter (fun st ->
        sprintf "%A" st |> sw.WriteLine
    )