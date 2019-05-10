namespace Froto.TypeProvider

open System
open System.IO
open System.Reflection


open Froto.TypeProvider.Core
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

[<TypeProvider>]
type ProtocolBuffersTypeProviderCreator(config : TypeProviderConfig) as this=

    inherit TypeProviderForNamespaces(config)

    let ns = typeof<ProtocolBuffersTypeProviderCreator>.Namespace
    let asm = Assembly.LoadFrom config.RuntimeAssembly

    let protobufProvider =
        ProvidedTypeDefinition(
            asm, ns, "ProtocolBuffersTypeProvider", Some typeof<obj>,
            isErased = false,
            hideObjectMethods = true)
    
    do
        try

            Logger.log "ProtocolBuffersTypeProviderCreator instance initializing"

            protobufProvider.DefineStaticParameters(
                [ProvidedStaticParameter("pathToFile", typeof<string>)],
                fun typeName args ->
                    Logger.log "Generating enclosing type \"%s\" with args %A in namespace %s" typeName args ns

                    let protoPath = args.[0] :?> string
                    let protoPath =
                        if Path.IsPathRooted protoPath then protoPath
                        else config.ResolutionFolder </> protoPath

                    let provided = TypeProviderImpl.createProvidedTypes typeName protoPath asm ns
                    provided)

            this.AddNamespace(ns, [protobufProvider])
        with
        | e ->
            Logger.log "Unhandled error ocurred while generating types: %O" e
            reraise()

    static do
        Logger.log "Initializing type provider..."
        AppDomain.CurrentDomain.UnhandledException
        |> Event.add(fun args -> Logger.log "Unhandled error %O" args.ExceptionObject)

        AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args -> AssemblyResolver.resolve args.Name)

[<assembly:TypeProviderAssembly>]
do()



//                    Logger.log "Watching file '%s' for changes" protoPath
    //                File.watch false protoPath (fun _ ->
    //                    Logger.log "File '%s' has been changed. Type provider %s will be invalidated" protoPath typeName
    //                    cache.Remove(typeName) |> ignore
    //                    this.Invalidate())
    //                |> disposables.Add

