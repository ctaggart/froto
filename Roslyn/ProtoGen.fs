
module Froto.Roslyn.ProtoGen

open Froto.Parser
open Froto.Parser.ProtoAst
open Roslyn.Compilers.CSharp

// aliases
module NS = NamespaceDeclarationSyntax
module CU = CompilationUnitSyntax
module CD = ClassDeclarationSyntax
module ED = EnumDeclarationSyntax
module Cmp = Compilation

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

let createCompilation path =

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

    let cds = listMessages proto.Messages |> List.map (fun msg ->
        CD.create msg.Name
        |> CD.addModifier Keyword.Public
        |> CD.addMembers (
            msg.Fields |> List.map (fun f ->
                let tp =
                    if scalarToSyntaxKind.ContainsKey f.Type then
                        scalarToSyntaxKind.[f.Type]
                    else
                        Syntax.ParseTypeName f.Type

                PropertyDeclarationSyntax.create f.Name tp
                :> MemberDeclarationSyntax
            )
        )
        :> MemberDeclarationSyntax
    )

    let enums = listEnums [] proto.Messages |> List.map (fun enum ->
        ED.create enum.Name
        |> ED.addModifier Keyword.Public
        |> ED.addMembers (enum.Items |> List.map (fun item -> EnumMemberDeclarationSyntax.create item.Name item.Value))
        :> MemberDeclarationSyntax
    )

    let ns =
        NS.create proto.Packages.[0]
        |> NS.addMembers enums
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