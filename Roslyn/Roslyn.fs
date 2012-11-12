
namespace Froto.Roslyn

open Roslyn.Compilers
open Roslyn.Compilers.CSharp
open System
open System.IO
open System.Collections.Generic
open Roslyn.Services
open Roslyn.Services.Formatting

module TypeSyntax =
    let predefined kind =
        Syntax.PredefinedType <| Syntax.Token kind :> TypeSyntax

    let double = predefined SyntaxKind.DoubleKeyword
    let float = predefined SyntaxKind.FloatKeyword
    let int = predefined SyntaxKind.IntKeyword
    let uint = predefined SyntaxKind.UIntKeyword
    let bool = predefined SyntaxKind.BoolKeyword
    let string = predefined SyntaxKind.StringKeyword

    let int64 = Syntax.ParseTypeName "int64"
    let uint64 = Syntax.ParseTypeName "uint64"
    let byteArray = Syntax.ParseTypeName "byte[]"

module NamespaceDeclarationSyntax =
    let create name =
        Syntax.NamespaceDeclaration <| Syntax.ParseName  name

    let addMember m (ns:NamespaceDeclarationSyntax) =
        ns.AddMembers [| m |]

    let addMembers (mbs:MemberDeclarationSyntax list) (ns:NamespaceDeclarationSyntax) =
        ns.AddMembers(mbs |> List.toArray)

module NS = NamespaceDeclarationSyntax

module CompilationUnitSyntax =
    let Empty =
        Syntax.ParseCompilationUnit String.Empty

    let addUsing name (cu:CompilationUnitSyntax) =
        cu.AddUsings [| Syntax.UsingDirective <| Syntax.ParseName name |]

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

module AccessorDeclarationSyntax =
    //Syntax.AccessorDeclaration SyntaxKind.GetKeyword
    // http://social.msdn.microsoft.com/Forums/nl/roslyn/thread/679f4fd9-f37f-4fa6-8816-88eb64e6a3d3

    let get = 
        let st = SyntaxTree.ParseText "string Foo { get; }"
        st.GetRoot() |> SyntaxNode.descendants |> Seq.find(fun sn -> sn.GetType() = typeof<AccessorDeclarationSyntax>)
        :?> AccessorDeclarationSyntax
        
    let set = 
        let st = SyntaxTree.ParseText "string Foo { set; }"
        st.GetRoot() |> SyntaxNode.descendants |> Seq.find(fun sn -> sn.GetType() = typeof<AccessorDeclarationSyntax>)
        :?> AccessorDeclarationSyntax

module PropertyDeclarationSyntax =
    let create (id:string) tp =
        let p = Syntax.PropertyDeclaration(tp, id)
        p.AddAccessorListAccessors([| AccessorDeclarationSyntax.get; AccessorDeclarationSyntax.set |])

module ClassDeclarationSyntax =
    let create name =
        Syntax.ClassDeclaration (name:string)

    let addModifier m (cd:ClassDeclarationSyntax) =
        cd.AddModifiers [| m |]

    let addMember m (cd:ClassDeclarationSyntax) =
        cd.AddMembers [| m |]

    let addMembers mbrs (cd:ClassDeclarationSyntax) =
        cd.AddMembers (mbrs |> Array.ofList)

module CD = ClassDeclarationSyntax

module EnumMemberDeclarationSyntax =
    let create name (value:int32) =
        let v = Syntax.EqualsValueClause(Syntax.LiteralExpression(SyntaxKind.NumericLiteralExpression, Syntax.Literal value))
        Syntax.EnumMemberDeclaration(SyntaxList(), Syntax.Identifier name, v)

module EnumDeclarationSyntax =
    let create name =
        Syntax.EnumDeclaration (name:string)

    let addModifier m (ed:EnumDeclarationSyntax) =
        ed.AddModifiers [| m |]

    let addMember name value (ed:EnumDeclarationSyntax) =
        let mbr = EnumMemberDeclarationSyntax.create name value
        ed.AddMembers [| mbr |]

    let addMembers members (ed:EnumDeclarationSyntax) =
        ed.AddMembers(members |> Array.ofList)

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

    let syntaxTrees (cmp:Compilation) =
        cmp.SyntaxTrees.AsEnumerable() |> List.ofSeq