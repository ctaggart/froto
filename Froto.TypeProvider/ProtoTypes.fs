namespace ProtoTypes

open System
open System.IO
open System.Reflection

open ProtoTypes.Core
open ProtoTypes.Generation
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices

open Froto.Parser.Model

[<TypeProvider>]
type ProtocolBuffersTypeProviderCreator(config : TypeProviderConfig) as this= 
    inherit TypeProviderForNamespaces()
    
    let ns = typeof<ProtocolBuffersTypeProviderCreator>.Namespace
    let asm = Assembly.LoadFrom config.RuntimeAssembly
    let tempAssembly = Path.ChangeExtension(Path.GetTempFileName(), ".dll") |> ProvidedAssembly
    
    let protobufProvider = 
        ProvidedTypeDefinition(
            asm, ns, "ProtocolBuffersTypeProvider", Some typeof<obj>, 
            IsErased = false, 
            HideObjectMethods = true)

    let parameters = [ProvidedStaticParameter("pathToFile", typeof<string>)]
    
    do 
        protobufProvider.DefineStaticParameters(parameters, fun typeName args ->
            let provider = 
                ProvidedTypeDefinition(
                    asm, ns, typeName, Some typeof<obj>, 
                    HideObjectMethods = true, 
                    IsErased = false)
                    
            let tempAssembly = Path.ChangeExtension(Path.GetTempFileName(), ".dll") |> ProvidedAssembly
            
            let pathToFile = args.[0] :?> string

            let protoLocation = 
                if Path.IsPathRooted pathToFile then pathToFile
                else config.ResolutionFolder </> pathToFile
            
            let protoFile = ProtoFile.ParseFile protoLocation
            
            let rootScope = protoFile.Packages |> Seq.tryHead |> Option.getOrElse String.Empty
            
            let container = 
                if String.IsNullOrEmpty rootScope
                then provider 
                else
                    let root, deepest = TypeGen.createNamespaceContainer rootScope
                    provider.AddMember root
                    deepest

            let lookup = TypeResolver.discoverTypes rootScope protoFile.Messages

            protoFile.Messages
            |> Seq.map (TypeGen.createType rootScope lookup)
            |> Seq.iter container.AddMember
            
            if config.IsHostedExecution then
                Testing.FormatProvidedType(container, true)
                |> printfn "%s"
            
            tempAssembly.AddTypes [provider]
            provider)
        
        tempAssembly.AddTypes [protobufProvider]
        this.AddNamespace(ns, [protobufProvider])

[<assembly:TypeProviderAssembly>] 
do()
