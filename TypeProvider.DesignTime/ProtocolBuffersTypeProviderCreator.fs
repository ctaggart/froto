namespace Froto.TypeProvider

open System
open System.IO
open System.Collections.Concurrent
open System.Reflection

open Froto.TypeProvider
open Froto.TypeProvider.Core
open Froto.TypeProvider.Runtime
open Froto.TypeProvider.Utils
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices


[<TypeProvider>]
type ProtocolBuffersTypeProviderCreator(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("Froto.TypeProvider.DesignTime", "Froto.TypeProvider.Runtime")], addDefaultProbingLocation=true)

    static let typesCache = new ConcurrentDictionary<string, ProvidedTypeDefinition>()
    static let watchersCache = new ConcurrentDictionary<string, IDisposable>()

    let ns = typeof<ProtocolBuffersTypeProviderCreator>.Namespace
    let runtimeAssembly = Assembly.LoadFrom config.RuntimeAssembly

    let protobufProvider =
        let provider = ProvidedTypeDefinition(runtimeAssembly, ns, "ProtocolBuffersTypeProvider", Some typeof<obj>, isErased = false)

        provider.DefineStaticParameters(
                [ProvidedStaticParameter("pathToFile", typeof<string>)],
                fun typeName args ->
                    Logger.log "Generating enclosing type \"%s\" with args %A in namespace %s" typeName args ns

                    let protoPath = args.[0] :?> string
                    let protoPath =
                        if Path.IsPathRooted protoPath then protoPath
                        else config.ResolutionFolder </> protoPath

                    let createProvidedTypes cacheKey =
                        watchersCache.AddOrUpdate(
                            cacheKey,
                            (fun cacheKey ->
                                Logger.log "Watching \"%s\" for changes" protoPath
                                let watch =
                                    FileWatcher.watch true protoPath (fun () ->
                                        Logger.log "Detected change in \"%s\"" protoPath
                                        
                                        typesCache.TryRemove cacheKey |> ignore
                                        this.Invalidate() )
                                
                                watch),
                            fun _ existing -> existing) |> ignore

                        TypeProviderImpl.createProvidedTypes typeName protoPath ns

                    let cacheKey = typeName + "-" + protoPath
                    typesCache.GetOrAdd(cacheKey, createProvidedTypes))

        provider
    do
        this.AddNamespace(ns, [protobufProvider])

[<assembly:TypeProviderAssembly>]
do()

