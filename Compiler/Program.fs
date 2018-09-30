module Froto.Program

open Argu
open System

type Arguments =
    | [<Mandatory; AltCommandLine "-p">] Proto of string
    | [<AltCommandLine "-i">] Import of string
    | ProtobufNet_Out of string
with
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Proto _ -> "A proto file to generate output from. May be specififed multiple times."
            | Import _ -> "Specify the directory in which to search for imports. May be specified multiple times; directories will be searched in order. If not given, the current working directory is used."
            | ProtobufNet_Out _ ->  "The directory to put the generated source files for C# protobuf-net. Supports proto2, not proto3."
            // | Another_Out...

[<EntryPoint>]
let main argv = 
    let parser = ArgumentParser.Create<Arguments>()
    try
        let results = parser.ParseCommandLine argv
        let protos = results.GetResults <@ Proto @>
        let imports = results.GetResults <@ Import @>
        let outProtobufNet = results.TryGetResult <@ ProtobufNet_Out @>
        
        // TODO do work here
        printfn "protos: %A" protos
        printfn "imports: %A" imports
        printfn "outProtobufNet: %A" outProtobufNet

        // TODO need to hook up the arguments
        //if outProtobufNet.IsSome then
        //    Froto.Roslyn.ProtoGen.generate @"test\riak.proto" "Riak" "RiakProto" "riak.cs"

        0
    with
    | :? ArgumentException as ex ->
        printfn "%s" ex.Message
        1
