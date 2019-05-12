namespace Froto.TypeProvider

open System
open System.IO
open System.Reflection


open Froto.TypeProvider.Core
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
                    TypeProviderImpl.createProvidedTypes typeName protoPath ns)
        t
    do
        this.AddNamespace(ns, [protobufProvider])

    static do
        Logger.log "Initializing type provider..."
        AppDomain.CurrentDomain.UnhandledException
        |> Event.add(fun args -> Logger.log "Unhandled error %O" args.ExceptionObject)

        AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args -> AssemblyResolver.resolve args.Name)

[<assembly:TypeProviderAssembly>]
do()

