module Froto.TypeProvider.AssemblyResolver

open System
open System.IO
open System.Reflection

open Froto.TypeProvider.Core

let currentLocation = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

/// Assuming Froto.TypeProvider is installed as NuGet/Paket package, these are locations of its
/// package dependencies.
let private probingPaths =
    [
        "."
        "../../../FParsec/lib/net40-client"
        "../../../Froto.Parser/lib/net45"
        "../../../Froto.Serialization/lib/net45"
        "../../../Froto.TypeProvider/lib/net45"
    ] |> List.map (fun path -> Path.GetFullPath(currentLocation </> path))

let private findAssemblyOnDisk assemblyName =
    try
        Logger.log
            "Assembly \"%s\" has not been found in the current AppDomain. Trying to seach the assembly on in %A."
            assemblyName
            probingPaths

        let name = AssemblyName(assemblyName)

        let asm =
            probingPaths
            |> Seq.choose (fun path ->
                let candidate = path </> (name.Name + ".dll")
                if File.Exists candidate then
                    Logger.log "Found requested assembly \"%s\" - located at \"%s\"" assemblyName candidate
                    Some <| Assembly.LoadFrom candidate
                else None)
            |> Seq.tryHead

        match asm with
        | Some (asm) -> asm
        | None ->
            Logger.log "Assembly \"%s\" has not been resolved." assemblyName
            null
    with
    | e ->
        Logger.log "Failed to load assembly \"%s\" - %O" assemblyName e
        reraise()

/// Attempts to resolve assembly by it's name. Returns assembly already loaded in the current AppDomain
/// if there's one. Otherwise, tries to search for assembly in known probing locations.
let resolve assemblyName =
    Logger.log "Resolving assemlby \"%s\"" assemblyName
    let name = AssemblyName(assemblyName)
    let existingAssembly =
        AppDomain.CurrentDomain.GetAssemblies()
        |> Seq.tryFind(fun a -> AssemblyName.ReferenceMatchesDefinition(name, a.GetName()))
    match existingAssembly with
    | Some assembly ->
        Logger.log "Assembly \"%s\" has been found in the current AppDomtain" assemblyName
        assembly
    | None -> findAssemblyOnDisk assemblyName
