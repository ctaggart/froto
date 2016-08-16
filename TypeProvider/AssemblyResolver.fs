module Froto.TypeProvider.AssemblyResolver

open System
open System.IO
open System.Reflection

open Froto.TypeProvider.Core

/// Assuming Froto.TypeProvider is installed as NuGet/Paket package, these are locations of its
/// package dependencies.
let private probingPaths = 
    [
        "../../../FParsec/lib/net40-client"
        "../../../Froto.Parser/lib/net45"
        "../../../Froto.Serialization/lib/net45"
        "../../../Froto.TypeProvider/lib/net45"
    ]

let private findAssemblyOnDisk assemblyName =
    Logger.log 
        "Assembly \"%s\" has not been found in the current AppDomain. Trying to seach the assembly on in %A." 
        assemblyName
        probingPaths

    let name = AssemblyName(assemblyName)
    let currentLocation = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let asm =
        probingPaths
        |> Seq.choose (fun path ->
            let candidate = currentLocation </> path </> (name.Name + ".dll")
            if File.Exists candidate then
                let asmRef = Assembly.ReflectionOnlyLoadFrom candidate

                if asmRef.FullName = assemblyName 
                then Some <| Assembly.LoadFrom candidate
                else None
            else None)
        |> Seq.tryHead
        
    match asm with 
    | Some (asm) -> asm
    | None -> 
        Logger.log "Assembly \"%s\" has not been resolved." assemblyName
        null

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
