
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
let ``can parse field options`` () =
    let options = parseString pFieldOptions "[deprecated=true, packed = false]"
    2 |> should equal options.Length
    "deprecated" |> should equal options.[0].Name
    "true" |> should equal options.[0].Value
    "packed" |> should equal options.[1].Name
    "false" |> should equal options.[1].Value

[<Fact>]
let ``can parse field with field options`` () =
    let field = parseString pField "repeated int32 test_packed = 5 [packed = true];"
    Repeated |> should equal field.Rule
    field.Options.IsSome |> should be True
    let options = field.Options.Value
    1 |> should equal options.Length
    "packed" |> should equal options.[0].Name
    "true" |> should equal options.[0].Value

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

[<Fact>]
let ``can parse enum with underscore`` () =
    let enum =
        """enum phone_type { 
        mobile = 0; 
        home = 1; 
        work = 2; 
        }"""
        |> parseString pEnum
    "phone_type" |> should equal enum.Name
    3 |> should equal enum.Items.Length

[<Fact>]
let ``can parse option`` () =
    let option = parseString pOption "option (my_option) = \"Hello world!\";"
    option.IsCustom |> should be True

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
    1 |> should equal proto.Sections.Length
    let messages = proto.Messages
    1 |> should equal messages.Length
    let message = messages.[0]
    "SearchRequest" |> should equal message.Name
    3 |> should equal message.Parts.Length

[<Fact>]
// from protobuf-net\Tools\nwind.proto
let ``can parse nwind proto`` () =
    let proto = getTestFile "nwind.proto" |> parseFile pProto 
    4 |> should equal proto.Sections.Length
    let messages = proto.Messages
    3 |> should equal messages.Length
    let message = messages.[2]
    "OrderLine" |> should equal message.Name
    5 |> should equal message.Parts.Length

[<Fact>]
// from protobuf-net\Examples\ProtoGen\person.proto
let ``can parse person proto`` () =
    let proto = getTestFile "person.proto" |> parseFile pProto
    3 |> should equal proto.Sections.Length
    7 |> should equal proto.Messages.[0].Fields.Length
    1 |> should equal proto.Messages.[0].Messages.Length

[<Fact>]
// from https://developers.google.com/protocol-buffers/docs/javatutorial
let ``can parse javatutorial proto`` () =
    let proto = getTestFile "javatutorial.proto" |> parseFile pProto
    5 |> should equal proto.Sections.Length
    2 |> should equal proto.Options.Length
    "java_package" |> should equal proto.Options.[0].Name
    "com.example.tutorial" |> should equal proto.Options.[0].Value

[<Fact>]
// from https://developers.google.com/protocol-buffers/docs/proto#options
let ``can parse protooptions proto`` () =
    let proto = getTestFile "protooptions.proto" |> parseFile pProto
    3 |> should equal proto.Sections.Length
    "google/protobuf/descriptor.proto" |> should equal proto.Imports.[0]
    2 |> should equal proto.Messages.Length
    let message = proto.Messages.[1]
    "my_option" |> should equal message.Options.[0].Name

[<Fact>]
// from protobuf-net\Examples\ProtoGen\rpc.proto
let ``can parse rpc proto`` () =
    let proto = getTestFile "rpc.proto" |> parseFile pProto
    3 |> should equal proto.Sections.Length
    1 |> should equal proto.Services.Length
    let svc = proto.Services.[0]
    "SearchService" |> should equal svc.Name
    1 |> should equal svc.Rpcs.Length
    let rpc = svc.Rpcs.[0]
    "Search" |> should equal rpc.Name