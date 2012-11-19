
namespace Froto.Gen.Name
type internal ProtoGen() = inherit obj()

namespace Froto.Gen

open System
open System.Diagnostics
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices

// aliases
module PG = Froto.Roslyn.ProtoGen
module Cmp = Froto.Roslyn.Compilation

[<TypeProvider>]
type ProtoTypeProvider(cfg:TypeProviderConfig) =
    
    let mutable protoPath = None
    let mutable assemblyBytes = None
    let mutable assembly = None
    let mutable rootTp = None

    let invalidation = new Event<_,_>()
    
    interface IProvidedNamespace with
        member x.NamespaceName with get() = "Froto.Gen"
        member x.GetTypes() = [| typeof<Name.ProtoGen> |]
        member x.GetNestedNamespaces() = [||]
        member x.ResolveTypeName(typeName) = if typeName = "ProtoGen" then typeof<Name.ProtoGen> else null

    interface ITypeProvider with
        member x.GetNamespaces() = [| x |]

        [<CLIEvent>]
        member x.Invalidate =
#if DEBUG
            Debugger.Break()
#endif
            invalidation.Publish

        member x.GetStaticParameters(typeWithoutArguments) =
            Debug.WriteLine <| sprintf "GetStaticParameters %A" typeWithoutArguments
            if typeWithoutArguments = typeof<Name.ProtoGen> then
                [|
                    { new ParameterInfo() with
                        override x.Name with get() = "path"
                        override x.ParameterType with get() = typeof<string>
                        override x.Position with get() = 0
                    }
                |]
            else
                [||]

        // the type name returned must match typeNameWithArguments[n-1]
        member x.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            Debug.WriteLine <| sprintf "ApplyStaticArguments(%A, %A, %A)" typeWithoutArguments typeNameWithArguments staticArguments
            let rootTpFullName = String.Join(".", typeNameWithArguments)

            if typeWithoutArguments = typeof<Name.ProtoGen> then
                let path = staticArguments.[0] :?> string
                if false = File.Exists path then
                    failwithf "proto file not found: %s" path

                // build a new compilation if...
                if protoPath.IsNone || path <> protoPath.Value || rootTpFullName <> rootTp.Value then
                    protoPath <- path |> Some
                    rootTp <- rootTpFullName |> Some
                    let rootNsName =
                        match typeNameWithArguments.Length with
                        | 2 -> typeNameWithArguments.[0]
                        | _ -> String.Join(".", typeNameWithArguments.[0..typeNameWithArguments.Length-2])
                    let rootTpName = typeNameWithArguments.[typeNameWithArguments.Length-1]
                    let cmp = PG.createCompilation path rootNsName rootTpName
                    //let cmp = PG.createCompilation @"test\riak.proto" "RiakProto" "tutorial"
                    use assemblyStream = cmp |> Cmp.emitMemoryStream
                    assemblyBytes <- assemblyStream.ToArray() |> Some
                    assembly <- Assembly.Load assemblyBytes.Value |> Some
                
                if assembly.IsSome then
                    assembly.Value.GetType rootTpFullName
                else
                    failwith "assembly not generated"
            else
                failwith "only ProtoGen supported"

        member x.GetInvokerExpression(syntheticMethodBase, parameters) =
            Debug.WriteLine <| sprintf "GetInvokerExpression(%A, %A)" syntheticMethodBase parameters
            match syntheticMethodBase with
            | :? ConstructorInfo as ctor ->
                Quotations.Expr.NewObject(ctor, Array.toList parameters) 
            | :? MethodInfo as mi ->
                if parameters.Length = 0 then
                    Quotations.Expr.Call(mi, parameters |> List.ofArray)
                else
                    Quotations.Expr.Call(parameters.[0], mi, Array.toList parameters.[1..])
            | _ ->
                NotImplementedException(sprintf "Not Implemented: GetInvokerExpression(%A, %A)" syntheticMethodBase parameters) |> raise

        member x.GetGeneratedAssemblyContents(assembly) =
            Debug.WriteLine <| sprintf "GetGeneratedAssemblyContents(%A)" assembly
            if assemblyBytes.IsSome then
                assemblyBytes.Value
            else
                failwith "assembly bytes not generated"

        member x.Dispose() = ()


[<assembly: TypeProviderAssembly>]
do ()
