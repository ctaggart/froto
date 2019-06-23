[<RequireQualifiedAccess>]
module Froto.TypeProvider.Cache

open Froto.TypeProvider.Core
open System.Collections.Concurrent
open ProviderImplementation.ProvidedTypes

let private cache = ConcurrentDictionary<string, ProvidedTypeDefinition>()

let get key (adder: (unit -> ProvidedTypeDefinition)) =
    Logger.log "Trying getting types for \"%s\"" key
    cache.GetOrAdd(key, fun _ ->
        Logger.log "Types for \"%s\" are not in cache. Invoking types generation" key
        adder())

let remove key =
    Logger.log "Removing cached types for \"%s\" from the cache" key
    cache.TryRemove(key) |> ignore
