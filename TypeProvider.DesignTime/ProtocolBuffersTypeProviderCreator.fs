namespace Froto.TypeProvider

open System.IO
open System.Reflection

open Froto.TypeProvider
open Froto.TypeProvider.Core
open Froto.TypeProvider.Runtime
open Froto.TypeProvider.Utils
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices


[<TypeProvider>]
type ProtocolBuffersTypeProviderCreator(config : TypeProviderConfig) as this=

    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("Froto.TypeProvider.DesignTime", "Froto.TypeProvider.Runtime")], addDefaultProbingLocation=true)

    let ns = typeof<ProtocolBuffersTypeProviderCreator>.Namespace
    let runtimeAssembly = Assembly.LoadFrom config.RuntimeAssembly

    let protobufProvider =
        let t = ProvidedTypeDefinition(runtimeAssembly, ns, "ProtocolBuffersTypeProvider", Some typeof<obj>, isErased = false)

        t.DefineStaticParameters(
                [ProvidedStaticParameter("pathToFile", typeof<string>)],
                fun typeName args ->
                    Logger.log "Generating enclosing type \"%s\" with args %A in namespace %s" typeName args ns

                    let protoPath = args.[0] :?> string
                    let protoPath =
                        if Path.IsPathRooted protoPath then protoPath
                        else config.ResolutionFolder </> protoPath

                    let cacheKey = typeName + "-" + protoPath

                    let createProvidedTypes () =

                        Logger.log "Watching \"%s\" for changes" protoPath
                        let watch =
                            FileWatcher.watch true protoPath (fun () ->
                                Cache.remove cacheKey
                                this.Invalidate() )

                        this.Disposing |> Event.add (fun _ ->
                            Logger.log "Disposing \"%s\" watcher" protoPath
                            watch.Dispose())

                        TypeProviderImpl.createProvidedTypes typeName protoPath ns

                    Cache.get cacheKey createProvidedTypes)

        t
    do
        this.AddNamespace(ns, [protobufProvider])

[<assembly:TypeProviderAssembly>]
do()

