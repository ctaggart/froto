namespace Froto.TypeProvider

open System
open System.IO
open System.Reflection

open Froto.TypeProvider.Core
open Froto.TypeProvider.Generation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

open Froto.Parser.ClassModel

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

    let createProvidedTypes typeName protoPath = 
        let provider = 
            ProvidedTypeDefinition(
                asm, ns, typeName, Some typeof<obj>, 
                HideObjectMethods = true, 
                IsErased = false)
                    
        let tempAssembly = Provided.assembly()

        let protoLocation = 
            if Path.IsPathRooted protoPath then protoPath
            else config.ResolutionFolder </> protoPath
            
        let protoFile = ProtoFile.fromFile protoLocation
            
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
            
        tempAssembly.AddTypes [provider]
        provider

    do 
        protobufProvider.DefineStaticParameters(
            [ProvidedStaticParameter("pathToFile", typeof<string>)], 
            fun typeName args -> args.[0] :?> string |> createProvidedTypes typeName)
        
        tempAssembly.AddTypes [protobufProvider]
        this.AddNamespace(ns, [protobufProvider])

[<assembly:TypeProviderAssembly>] 
do()
