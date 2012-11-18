
namespace Froto.Roslyn

open Roslyn.Compilers
open Roslyn.Compilers.CSharp
open System
open System.IO
open System.Collections.Generic
open Roslyn.Services
open Roslyn.Services.Formatting

module TypeSyntax =
    let create name =
        Syntax.ParseTypeName name

    let predefined kind =
        Syntax.PredefinedType <| Syntax.Token kind :> TypeSyntax

    let double = predefined SyntaxKind.DoubleKeyword
    let float = predefined SyntaxKind.FloatKeyword
    let int = predefined SyntaxKind.IntKeyword
    let uint = predefined SyntaxKind.UIntKeyword
    let bool = predefined SyntaxKind.BoolKeyword
    let string = predefined SyntaxKind.StringKeyword
    let vd = predefined SyntaxKind.VoidKeyword

    let int64 = Syntax.ParseTypeName "int64"
    let uint64 = Syntax.ParseTypeName "uint64"
    let byteArray = Syntax.ParseTypeName "byte[]"

module MethodDeclarationSyntax =
    let create returnType id =
        Syntax.MethodDeclaration(returnType, (id:string))

    let fold list f (md:MethodDeclarationSyntax) =
        List.fold (fun acc elem -> f acc elem) md list

    let addModifier m (md:MethodDeclarationSyntax) =
        md.AddModifiers [| m |]
    
    let addParameters pl (md:MethodDeclarationSyntax) =
        md.AddParameterListParameters (pl |> Array.ofList)

    let addBodyStatements stmts (md:MethodDeclarationSyntax) =
        md.AddBodyStatements(stmts |> Array.ofList)

module ParameterSyntax =
    let create tp name =
        Syntax.Parameter(Syntax.List(), Syntax.TokenList(), tp, Syntax.Identifier name, null)

module NamespaceDeclarationSyntax =
    let create name =
        Syntax.NamespaceDeclaration <| Syntax.ParseName name

    let addUsing name (ns:NamespaceDeclarationSyntax) =
        ns.AddUsings [| Syntax.UsingDirective <| Syntax.ParseName name |]

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
    let Static = Syntax.Token SyntaxKind.StaticKeyword
    let Sealed = Syntax.Token SyntaxKind.SealedKeyword

module AccessorDeclarationSyntax =
//    let get = Syntax.AccessorDeclaration SyntaxKind.GetAccessorDeclaration  
//    let set = Syntax.AccessorDeclaration SyntaxKind.SetAccessorDeclaration

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

    let addModifier m (pd:PropertyDeclarationSyntax) =
        pd.AddModifiers [| m |]

module AttributeListSyntax =
    let addAttribute a (al:AttributeListSyntax) =
        al.AddAttributes [| a |]

module ClassDeclarationSyntax =
    let create name =
        Syntax.ClassDeclaration (name:string)

    let fold list f (cd:ClassDeclarationSyntax) =
        List.fold (fun acc elem -> f acc elem) cd list

    let addModifier m (cd:ClassDeclarationSyntax) =
        cd.AddModifiers [| m |]

    let addMember m (cd:ClassDeclarationSyntax) =
        cd.AddMembers [| m |]

    let addMembers mbrs (cd:ClassDeclarationSyntax) =
        cd.AddMembers (mbrs |> Array.ofList)

    let addAttribute attr (cd:ClassDeclarationSyntax) =
        cd.AddAttributeLists [|
            Syntax.AttributeList()
            |> AttributeListSyntax.addAttribute attr
        |]

module AttributeSyntax =
    let create name =
        Syntax.Attribute(Syntax.ParseName name)

    let addArgument arg (attr:AttributeSyntax) =
        attr.AddArgumentListArguments [| arg |]

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

    let addAttribute attr (ed:EnumDeclarationSyntax) =
        ed.AddAttributeLists [|
            Syntax.AttributeList()
            |> AttributeListSyntax.addAttribute attr
        |]

module Compilation =
    let createDll name = 
        Compilation.Create(name, options=CompilationOptions(OutputKind.DynamicallyLinkedLibrary))

    let addReference (tp:Type) (cmp:Compilation) =
        cmp.AddReferences [| MetadataFileReference(tp.Assembly.Location) :> MetadataReference |]

    let addSyntaxTree (st:SyntaxTree) (cmp:Compilation) =
        cmp.AddSyntaxTrees [ st ]

    let emit (stream:Stream) (cmp:Compilation) =
        let result = cmp.Emit stream
        if not result.Success then
            failwithf "emit failed: %A" result.Diagnostics

    let emitMemoryStream (cmp:Compilation) =
        let ms = new MemoryStream()
        emit ms cmp
        ms.Position <- 0L
        ms

    let emitAssembly cmp =
        use ms = emitMemoryStream cmp
        Reflection.Assembly.Load(ms.GetBuffer())

    let emitFile path cmp =
        use fs = File.OpenWrite path
        emit fs cmp

    let syntaxTrees (cmp:Compilation) =
        cmp.SyntaxTrees.AsEnumerable() |> List.ofSeq