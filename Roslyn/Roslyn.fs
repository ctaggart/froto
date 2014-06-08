
namespace Froto.Roslyn

open System
open System.IO
open System.Collections.Generic
open Microsoft.CodeAnalysis // SyntaxTree
open Microsoft.CodeAnalysis.CSharp // CSharpSyntaxTree, SyntaxFactory
open Microsoft.CodeAnalysis.CSharp.Syntax // MethodDeclarationSyntax
open Microsoft.CodeAnalysis.Formatting

module TypeSyntax =
    let create name =
        SyntaxFactory.ParseTypeName name

    let predefined kind =
        SyntaxFactory.PredefinedType <| SyntaxFactory.Token kind :> TypeSyntax

    let double = predefined SyntaxKind.DoubleKeyword
    let float = predefined SyntaxKind.FloatKeyword
    let int = predefined SyntaxKind.IntKeyword
    let uint = predefined SyntaxKind.UIntKeyword
    let bool = predefined SyntaxKind.BoolKeyword
    let string = predefined SyntaxKind.StringKeyword
    let vd = predefined SyntaxKind.VoidKeyword

    let int64 = SyntaxFactory.ParseTypeName "int64"
    let uint64 = SyntaxFactory.ParseTypeName "uint64"
    let byteArray = SyntaxFactory.ParseTypeName "byte[]"

module MethodDeclarationSyntax =
    let create returnType id =
        SyntaxFactory.MethodDeclaration(returnType, (id:string))

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
        SyntaxFactory.Parameter(SyntaxFactory.List(), SyntaxFactory.TokenList(), tp, SyntaxFactory.Identifier name, null)

module NamespaceDeclarationSyntax =
    let create name =
        SyntaxFactory.NamespaceDeclaration <| SyntaxFactory.ParseName name

    let addUsing name (ns:NamespaceDeclarationSyntax) =
        ns.AddUsings [| SyntaxFactory.UsingDirective <| SyntaxFactory.ParseName name |]

    let addMember m (ns:NamespaceDeclarationSyntax) =
        ns.AddMembers [| m |]

    let addMembers (mbs:MemberDeclarationSyntax list) (ns:NamespaceDeclarationSyntax) =
        if mbs.IsEmpty then ns
        else ns.AddMembers(mbs |> List.toArray)

module NS = NamespaceDeclarationSyntax

module CompilationUnitSyntax =
    let Empty =
        SyntaxFactory.ParseCompilationUnit String.Empty

    let addUsing name (cu:CompilationUnitSyntax) =
        cu.AddUsings [| SyntaxFactory.UsingDirective <| SyntaxFactory.ParseName name |]

    let addMember m (cu:CompilationUnitSyntax) =
        cu.AddMembers [| m |]

    let formatDefault (cu:CompilationUnitSyntax) =
        Formatter.Format(cu, MSBuild.MSBuildWorkspace.Create()) :?> CompilationUnitSyntax

    let createSyntaxTree (cu:CompilationUnitSyntax) =
        CSharpSyntaxTree.Create cu

module CU = CompilationUnitSyntax

module SyntaxNode =
    let descendants (sn:SyntaxNode) : SyntaxNode seq =
        sn.DescendantNodes((null:Func<SyntaxNode,bool>))

module Keyword =
    let Public = SyntaxFactory.Token SyntaxKind.PublicKeyword
    let Private = SyntaxFactory.Token SyntaxKind.PrivateKeyword
    let Get = SyntaxFactory.Token SyntaxKind.PrivateKeyword
    let Static = SyntaxFactory.Token SyntaxKind.StaticKeyword
    let Sealed = SyntaxFactory.Token SyntaxKind.SealedKeyword

module AccessorDeclarationSyntax =
//    let get = SyntaxFactory.AccessorDeclaration SyntaxKind.GetAccessorDeclaration  
//    let set = SyntaxFactory.AccessorDeclaration SyntaxKind.SetAccessorDeclaration

    let get = 
        let st = CSharpSyntaxTree.ParseText "string Foo { get; }"
        st.GetRoot() |> SyntaxNode.descendants |> Seq.find(fun sn -> sn.GetType() = typeof<AccessorDeclarationSyntax>)
        :?> AccessorDeclarationSyntax
        
    let set = 
        let st = CSharpSyntaxTree.ParseText "string Foo { set; }"
        st.GetRoot() |> SyntaxNode.descendants |> Seq.find(fun sn -> sn.GetType() = typeof<AccessorDeclarationSyntax>)
        :?> AccessorDeclarationSyntax

module PropertyDeclarationSyntax =
    let create (id:string) tp =
        let p = SyntaxFactory.PropertyDeclaration(tp, id)
        p.AddAccessorListAccessors([| AccessorDeclarationSyntax.get; AccessorDeclarationSyntax.set |])

    let addModifier m (pd:PropertyDeclarationSyntax) =
        pd.AddModifiers [| m |]

module AttributeListSyntax =
    let addAttribute a (al:AttributeListSyntax) =
        al.AddAttributes [| a |]

module ClassDeclarationSyntax =
    let create name =
        SyntaxFactory.ClassDeclaration (name:string)

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
            SyntaxFactory.AttributeList()
            |> AttributeListSyntax.addAttribute attr
        |]

module AttributeSyntax =
    let create name =
        SyntaxFactory.Attribute(SyntaxFactory.ParseName name)

    let addArgument arg (attr:AttributeSyntax) =
        attr.AddArgumentListArguments [| arg |]

module CD = ClassDeclarationSyntax

module EnumMemberDeclarationSyntax =
    let create name (value:int32) =
        let v = SyntaxFactory.EqualsValueClause(SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal value))
        SyntaxFactory.EnumMemberDeclaration(SyntaxList(), SyntaxFactory.Identifier name, v)

module EnumDeclarationSyntax =
    let create name =
        SyntaxFactory.EnumDeclaration (name:string)

    let addModifier m (ed:EnumDeclarationSyntax) =
        ed.AddModifiers [| m |]

    let addMember name value (ed:EnumDeclarationSyntax) =
        let mbr = EnumMemberDeclarationSyntax.create name value
        ed.AddMembers [| mbr |]

    let addMembers members (ed:EnumDeclarationSyntax) =
        ed.AddMembers(members |> Array.ofList)

    let addAttribute attr (ed:EnumDeclarationSyntax) =
        ed.AddAttributeLists [|
            SyntaxFactory.AttributeList()
            |> AttributeListSyntax.addAttribute attr
        |]

module Compilation =
    let createDll name = 
        CSharpCompilation.Create(name, options=CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary))

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
        cmp.SyntaxTrees |> List.ofSeq