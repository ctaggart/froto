
[<Xunit.Trait("Kind", "Unit")>]
module Froto.Parser.TestModel

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Froto.Parser.Model

let parseFile s = ProtoFile.ParseFile(s)
let parseStream name stream = ProtoFile.ParseStream(name, stream)
let parseString s = ProtoFile.ParseString(s)

/// gets the path for a test file based on the relative path from the executing assembly
let getTestFile file =
     let codeBase = Reflection.Assembly.GetExecutingAssembly().CodeBase
     let assemblyPath = DirectoryInfo (Uri codeBase).LocalPath
     let solutionPath = (assemblyPath.Parent.Parent.Parent.Parent).FullName
     Path.Combine(solutionPath, Path.Combine("test",file))

[<Fact>]
let `` can parse SearchRequest proto`` () =
    let proto = getTestFile "SearchRequest.proto" |> parseFile 
    1 |> should equal proto.Sections.Length
    let messages = proto.Messages
    1 |> should equal messages.Length
    let message = messages.[0]
    "SearchRequest" |> should equal message.Name
    3 |> should equal message.Parts.Length

[<Fact>]
// from protobuf-net\Tools\nwind.proto
let ``can parse nwind proto`` () =
    let proto = getTestFile "nwind.proto" |> parseFile 
    4 |> should equal proto.Sections.Length
    let messages = proto.Messages
    3 |> should equal messages.Length
    let message = messages.[2]
    "OrderLine" |> should equal message.Name
    5 |> should equal message.Parts.Length

[<Fact>]
// from protobuf-net\Examples\ProtoGen\person.proto
let ``can parse person proto`` () =
    let proto = getTestFile "person.proto" |> parseFile
    3 |> should equal proto.Sections.Length
    7 |> should equal proto.Messages.[0].Fields.Length
    1 |> should equal proto.Messages.[0].Messages.Length

[<Fact>]
// from https://developers.google.com/protocol-buffers/docs/javatutorial
let ``can parse javatutorial proto`` () =
    let proto = getTestFile "javatutorial.proto" |> parseFile
    5 |> should equal proto.Sections.Length
    2 |> should equal proto.Options.Length
    "java_package" |> should equal proto.Options.[0].Name.Value
    "\"com.example.tutorial\"" |> should equal proto.Options.[0].Value

[<Fact>]
// from https://developers.google.com/protocol-buffers/docs/proto#options
let ``can parse protooptions proto`` () =
    let proto = getTestFile "protooptions.proto" |> parseFile
    3 |> should equal proto.Sections.Length
    "google/protobuf/descriptor.proto" |> should equal proto.Imports.[0]
    1 |> should equal proto.Imports.Length
    1 |> should equal proto.Extends.Length
    1 |> should equal proto.Messages.Length
    let message = proto.Messages.[0]
    "my_option" |> should equal message.Options.[0].Name.Value

[<Fact>]
// from protobuf-net\Examples\ProtoGen\rpc.proto
let ``can parse rpc proto`` () =
    let proto = getTestFile "rpc.proto" |> parseFile
    3 |> should equal proto.Sections.Length
    1 |> should equal proto.Services.Length
    let svc = proto.Services.[0]
    "SearchService" |> should equal svc.Name
    1 |> should equal svc.Rpcs.Length
    let rpc = svc.Rpcs.[0]
    "Search" |> should equal rpc.Name

[<Fact>]
// from https://developers.google.com/protocol-buffers/docs/proto#options
let ``can parse FooOptions proto`` () =
    let proto = getTestFile "FooOptions.proto" |> parseFile
    3 |> should equal proto.Sections.Length
    "Bar" |> should equal proto.Messages.[1].Name

[<Fact>]
let ``test parseStream`` () =
    let path = getTestFile "FooOptions.proto"
    use stream = File.OpenRead path
    let proto = parseStream "FooOptions.proto" stream
    3 |> should equal proto.Sections.Length
    "Bar" |> should equal proto.Messages.[1].Name

[<Fact>]
// from http://code.google.com/p/protobuf-csharp-port/wiki/GettingStarted
let ``can parse addressbook proto`` () =
    let proto = getTestFile "addressbook.proto" |> parseFile
    7 |> should equal proto.Sections.Length
    3 |> should equal proto.Options.Length

[<Fact>]
// from https://github.com/basho/riak_pb/blob/master/src/riak.proto
let ``can parse riak proto`` () =
    let proto = getTestFile "riak.proto" |> parseFile
    5 |> should equal proto.Sections.Length
    2 |> should equal proto.Options.Length
    3 |> should equal proto.Messages.Length
    let kv = getTestFile "riak_kv.proto" |> parseFile
    23 |> should equal kv.Sections.Length
    let search = getTestFile "riak_search.proto" |> parseFile
    6 |> should equal search.Sections.Length