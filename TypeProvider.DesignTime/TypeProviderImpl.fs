module Froto.TypeProvider.TypeProviderImpl

open System

open Froto.TypeProvider.Core
open Froto.TypeProvider.Generation
open ProviderImplementation.ProvidedTypes

open Froto.Parser.ClassModel

let createProvidedTypes typeName protoPath ns =
        let tempAssembly = Provided.assembly()

        let provider =
            ProvidedTypeDefinition(
                tempAssembly, ns, typeName, Some typeof<obj>,
                hideObjectMethods = true,
                isErased = false)

        Logger.log "Generating types from %s" protoPath

        let protoFile = ProtoFile.fromFile protoPath

        let rootScope = protoFile.Packages |> Seq.tryHead |> Option.getOrElse String.Empty

        let container =
            if String.IsNullOrEmpty rootScope
            then provider
            else
                let root, deepest = TypeGen.createNamespaceContainer rootScope
                provider.AddMember root
                deepest

        let lookup = TypeResolver.discoverTypes rootScope protoFile

        protoFile.Enums
        |> Seq.map (TypeGen.createEnum rootScope lookup)
        |> Seq.iter container.AddMember

        protoFile.Messages
        |> Seq.map (TypeGen.createType rootScope lookup)
        |> Seq.iter container.AddMember

        Logger.log "Adding container type %s to the assembly %s" provider.FullName tempAssembly.FullName
        tempAssembly.AddTypes [provider]
        provider