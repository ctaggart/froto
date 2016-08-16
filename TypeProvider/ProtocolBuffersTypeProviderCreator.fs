namespace Froto.TypeProvider

open System
open System.IO
open System.Reflection
open System.Runtime.Caching

open Froto.TypeProvider.Core
open Froto.TypeProvider.Generation
open FSharp.Configuration.Helper
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

    let cache = new MemoryCache("TypeProviderCache")
    let disposables = ResizeArray<_>()

    let createProvidedTypes typeName protoPath = 

        let provider = 
            ProvidedTypeDefinition(
                asm, ns, typeName, Some typeof<obj>, 
                HideObjectMethods = true, 
                IsErased = false)
                    
        let tempAssembly = Provided.assembly()
            
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


            
        tempAssembly.AddTypes [provider]
        provider

    do 
        protobufProvider.DefineStaticParameters(
            [ProvidedStaticParameter("pathToFile", typeof<string>)], 
            fun typeName args ->
                Logger.log "Generating enclosing type \"%s\" with args %A" typeName args
                if cache.Contains(typeName) then
                    Logger.log "Enclosing type found in cache, returning existing."
                    cache.Get(typeName) :?> ProvidedTypeDefinition
                else
                    Logger.log "Enclosing type was not found. Generating a new one."
                    
                    let protoPath = args.[0] :?> string
                    let protoPath = 
                        if Path.IsPathRooted protoPath then protoPath
                        else config.ResolutionFolder </> protoPath
                    
                    Logger.log "Watching file '%s' for changes" protoPath

                    File.watch false protoPath (fun _ ->
                        Logger.log "File '%s' has been changed. Type provider %s will be invalidated" protoPath typeName
                        cache.Remove(typeName) |> ignore
                        this.Invalidate())
                    |> disposables.Add

                    let provided = createProvidedTypes typeName protoPath

                    cache.Add(CacheItem(typeName, provided), CacheItemPolicy(SlidingExpiration = TimeSpan.FromHours(24.0)))|> ignore
                    provided)
        
        tempAssembly.AddTypes [protobufProvider]
        this.AddNamespace(ns, [protobufProvider])

    static do
        AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args -> AssemblyResolver.resolve args.Name)

[<assembly:TypeProviderAssembly>] 
do()
