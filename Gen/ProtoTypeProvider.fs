
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
    
    let mutable protoPath = null
    let mutable assemblyStream = null
    let mutable assembly = null

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
//            Debugger.Break()
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
            let path = staticArguments.[0] :?> string

            // build a new compilation if the path is different
            if path <> protoPath then
                protoPath <- path
                assemblyStream <- PG.createCompilation path |> Cmp.emitStream
                assembly <- Assembly.Load(assemblyStream.GetBuffer())

            assembly.GetType("Tutorial.Blah.AddressBookProto")

        member x.GetInvokerExpression(syntheticMethodBase, parameters) =
            Debug.WriteLine <| sprintf "GetInvokerExpression(%A, %A)" syntheticMethodBase parameters
            match syntheticMethodBase with
            | :? ConstructorInfo as ctor ->
                Quotations.Expr.NewObject(ctor, Array.toList parameters) 
            | :? MethodInfo as mi ->
                Quotations.Expr.Call(parameters.[0], mi, Array.toList parameters.[1..])
            | _ ->
                NotImplementedException(sprintf "Not Implemented: ITypeProvider.GetInvokerExpression(%A, %A)" syntheticMethodBase parameters) |> raise

        member x.GetGeneratedAssemblyContents(assembly) =
            Debug.WriteLine <| sprintf "GetGeneratedAssemblyContents(%A)" assembly
            assemblyStream.GetBuffer()

        member x.Dispose() =
            Debug.WriteLine <| sprintf "Dispose()"
            use s = assemblyStream
            ()


[<assembly: TypeProviderAssembly>]
do ()
