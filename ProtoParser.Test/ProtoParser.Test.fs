
module Froto.ProtoParser.Test

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Froto.ProtoParser
open Froto.ProtoAst

[<Fact>]
let ``can parse field rule of "required"`` () =
    Required |> should equal (parseString pFieldRule "required")

[<Fact>]
let ``can parse field rule of "optional"`` () =
    Optional |> should equal (parseString pFieldRule "optional")

[<Fact>]
let ``can parse required field`` () =
    let field = parseString pField "required string query = 1;"
    Required |> should equal field.Rule
    "string" |> should equal field.Type
    "query" |> should equal field.Name
    1 |> should equal field.Position

[<Fact>]
let ``can parse enum item`` () =
    let enumItem = parseString pEnumItem "UNIVERSAL = 0;"
    "UNIVERSAL" |> should equal enumItem.Name
    0 |> should equal enumItem.Value

[<Fact>]
let ``can parse enum`` () =
    let enum =
        """enum Corpus {
        UNIVERSAL = 0;
        WEB = 1;
        IMAGES = 2;
        LOCAL = 3;
        NEWS = 4;
        PRODUCTS = 5;
        VIDEO = 6;
        }"""
        |> parseString pEnum
    "Corpus" |> should equal enum.Name
    7 |> should equal enum.Items.Length
    ()

[<Fact>]
let ``can parse message`` () =
    let msg =
        """message Result {
          required string url = 1;
          optional string title = 2;
          repeated string snippets = 3;
        }"""
        |> parseString pMessage
    "Result" |> should equal msg.Name
    3 |> should equal msg.Parts.Length

[<Fact>]
let ``can parse package`` () =
    "DAL" |> should equal (parseString pPackage "package DAL;")

/// gets the path for a test file based on the relative path from the executing assembly
let getTestFile file =
     let codeBase = Reflection.Assembly.GetExecutingAssembly().CodeBase
     let assemblyPath = DirectoryInfo (Uri codeBase).LocalPath
     let solutionPath = (assemblyPath.Parent.Parent.Parent.Parent).FullName
     Path.Combine(solutionPath, Path.Combine("test",file))

[<Fact>]
let `` can parse SearchRequest proto`` () =
    let proto = getTestFile "SearchRequest.proto" |> parseFile pProto 
    1 |> should equal proto.Length
    let message =
        match proto.[0] with
        | Message m -> m
        | _ -> failwith "not a Message"
    "SearchRequest" |> should equal message.Name
    3 |> should equal message.Parts.Length

[<Fact>]
// from protobuf-net\Tools\nwind.proto
let ``can parse nwind proto`` () =
    let proto = getTestFile "nwind.proto" |> parseFile pProto 
    4 |> should equal proto.Length
    let orderLine = 
        match proto.[3] with
        | Message m -> m
        | _ -> failwith "not a Message"
    "OrderLine" |> should equal orderLine.Name
    5 |> should equal orderLine.Parts.Length