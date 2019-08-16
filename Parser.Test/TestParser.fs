namespace TestParser

open Xunit
open FsUnit.Xunit

open System

open FParsec

open Froto.Parser
open Froto.Parser.Ast
open Froto.Parser.Parse.Parsers

module TestHelpers =
    open System.IO

    // Detect Mono runtime
    let isMono = System.Type.GetType "Mono.Runtime" |> isNull |> not

    /// Path of the test directory
    let testPath =
        let solutionPath =
            if isMono then
                "../../../"
            else
                let codeBase = Reflection.Assembly.GetExecutingAssembly().CodeBase
                let codeBase' = Uri.EscapeUriString codeBase 
                let codeBase'' = codeBase'.Replace("#", "%23") // handle paths like ../F#/..
                let uri = Uri codeBase''
                let assemblyPath = DirectoryInfo uri.LocalPath
                (assemblyPath.Parent.Parent.Parent.Parent.Parent).FullName
        Path.Combine(solutionPath, "test")

    /// gets the path for a test file based on the relative path from the executing assembly
    let getTestFilePath file =
        Path.Combine(testPath, file)


[<Xunit.Trait("Kind", "Unit")>]
module Identifiers =
    [<Fact>]
    let ``Identifier can be parsed`` () =
        Parse.fromStringWithParser pIdent_ws "test" |> should equal "test"

    [<Fact>]
    let ``Identifier must start with a letter`` () =
        fun () -> Parse.fromStringWithParser pIdent_ws "1st" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Identifier can contain A-Za-z0-9_`` () =
        let ident = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"
        Parse.fromStringWithParser pIdent_ws ident |> should equal ident

    [<Fact>]
    let ``Identifier cannot contain '.'`` () =
        fun () -> Parse.fromStringWithParser pIdent_ws "Test.Value" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Full identifier can contain '.'`` () =
        let fullIdent = "abc.def"
        Parse.fromStringWithParser pFullIdent_ws fullIdent
        |> should equal fullIdent

    [<Fact>]
    let ``Message Type can start with '.'`` () =
        let fullyQualified = ".full.message.type"
        Parse.fromStringWithParser pMessageType_ws fullyQualified
        |> should equal fullyQualified

    [<Fact>]
    let ``Group name can be parsed`` () =
        let groupName = "MyGroup"
        Parse.fromStringWithParser pGroupName_ws groupName
        |> should equal groupName

    [<Fact>]
    let ``Group name must begin with upper`` () =
        fun () -> Parse.fromStringWithParser pGroupName_ws "myGroup" |> ignore
        |> should throw typeof<System.FormatException>

[<Xunit.Trait("Kind", "Unit")>]
module Literals =

    [<Fact>]
    let ``Given dec, can parse intLit`` () =
        Parse.fromStringWithParser pNumLit "123"
        |> should equal (TIntLit 123)

    [<Fact>]
    let ``Given hex, can parse intLit`` () =
        Parse.fromStringWithParser pNumLit "0x80"
        |> should equal (TIntLit 128)

    [<Fact>]
    let ``Given oct, can parse intLit`` () =
        Parse.fromStringWithParser pNumLit "0777"
        |> should equal (TIntLit 511)

    [<Fact>]
    let ``Verify negative intLit is supported`` () =
        Parse.fromStringWithParser pNumLit "-123"
        |> should equal (TIntLit -123)

    [<Fact>]
    let ``Verify fieldNum < 1 is unsupported`` () =
        fun () -> Parse.fromStringWithParser pFieldNum "0" |> ignore
        |> should throw typeof<System.FormatException>
        fun () -> Parse.fromStringWithParser pFieldNum "-123" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Verify fieldNum >= 2^29 is unsupported`` () =
        fun () -> Parse.fromStringWithParser pFieldNum "536870912" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Given float, can parse floatLit`` () =
        Parse.fromStringWithParser pNumLit "123.456"
        |> should equal (TFloatLit 123.456)

    [<Fact>]
    let ``Given float with exp, can parse floatLit`` () =
        Parse.fromStringWithParser pNumLit "1.23456E2"
        |> should equal (TFloatLit 123.456)

    [<Fact>]
    let ``Verify negative floatLit is supported`` () =
        Parse.fromStringWithParser pNumLit "-123.456"
        |> should equal (TFloatLit -123.456)

    [<Fact>]
    let ``Verify negative exp is supported`` () =
        Parse.fromStringWithParser pNumLit "123456e-3"
        |> should equal (TFloatLit 123.456)

    [<Fact>]
    let ``Given list of bools, can parse boolLit`` () =
        [ "true", (TBoolLit true)
          "false", (TBoolLit false)  ]
        |> List.iter ( fun (given, expected) ->
            Parse.fromStringWithParser pBoolLit given
            |> should equal expected
            )

    [<Fact>]
    let ``Simple string literal can be parsed`` () =
        Parse.fromStringWithParser pStrLit "\"Test\""
        |> should equal (TStrLit "Test")

    [<Fact>]
    let ``Simple string between squotes can be parsed`` () =
        Parse.fromStringWithParser pStrLit "'Test'"
        |> should equal (TStrLit "Test")

    [<Fact>]
    let ``String with quote between squotes can be parsed`` () =
        Parse.fromStringWithParser pStrLit "'Te\"st'"
        |> should equal (TStrLit "Te\"st")

    [<Fact>]
    let ``String with squote between quotes can be parsed`` () =
        Parse.fromStringWithParser pStrLit "\"Test's\""
        |> should equal (TStrLit "Test's")

    [<Fact>]
    let ``Escaped characters can be parsed`` () =
        Parse.fromStringWithParser pStrLit ("\"" + @"\a\b\f\n\r\t\v\\\'\" + "\"\"")
        |> should equal (TStrLit "\a\b\f\n\r\t\v\\\'\"")

        Parse.fromStringWithParser pStrLit (@"'\a\b\f\n\r\t\v\\\'\" + "\"'")
        |> should equal (TStrLit "\a\b\f\n\r\t\v\\\'\"")

    [<Fact>]
    let ``Hex escape can be parsed`` () =
        Parse.fromStringWithParser pStrLit @"'\x30\x31\x32'"
        |> should equal (TStrLit "012")

    [<Fact>]
    let ``Hex throws on invalid digit`` () =
        fun () -> Parse.fromStringWithParser pStrLit @"'\xFG'" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Octal escape can be parsed`` () =
        Parse.fromStringWithParser pStrLit @"'\101\040\102'"
        |> should equal (TStrLit "A B")

    [<Fact>]
    let ``Octal throws on invalid digit`` () =
        fun () -> Parse.fromStringWithParser pStrLit @"'\008'" |> ignore
        |> should throw typeof<System.FormatException>
        fun () -> Parse.fromStringWithParser pStrLit @"'\009'" |> ignore
        |> should throw typeof<System.FormatException>

[<Xunit.Trait("Kind", "Unit")>]
module SyntaxStatement =

    [<Fact>]
    let ``Syntax 'proto2' parses`` () =
        Parse.fromStringWithParser pSyntax @"syntax = 'proto2';"
        |> should equal (TSyntax TProto2)

    [<Fact>]
    let ``Syntax 'proto3' parses`` () =
        Parse.fromStringWithParser pSyntax @"syntax = 'proto3';"
        |> should equal (TSyntax TProto3)

    [<Fact>]
    let ``Invalid syntax throws`` () =
        fun () -> Parse.fromStringWithParser pSyntax @"syntax = 'protox';" |> ignore
        |> should throw typeof<System.FormatException>

[<Xunit.Trait("Kind", "Unit")>]
module ImportStatement =

    [<Fact>]
    let ``Import parses`` () =
        Parse.fromStringWithParser pImport @"import 'test.proto';"
        |> should equal (TImport ("test.proto", TNormal))

    [<Fact>]
    let ``Import public parses`` () =
        Parse.fromStringWithParser pImport @"import public 'test.proto';"
        |> should equal (TImport ("test.proto", TPublic))

    [<Fact>]
    let ``Import weak parses`` () =
        Parse.fromStringWithParser pImport @"import weak 'test.proto';"
        |> should equal (TImport ("test.proto", TWeak))

[<Xunit.Trait("Kind", "Unit")>]
module PackageStatement =

    [<Fact>]
    let ``Package parses`` () =
        Parse.fromStringWithParser pPackage @"package abc.def;"
        |> should equal (TPackage "abc.def")

[<Xunit.Trait("Kind", "Unit")>]
module OptionStatement =

    [<Fact>]
    let ``Simple option parses`` () =
        Parse.fromStringWithParser pOptionStatement @"option test=1;"
        |> should equal (TOption ("test", TIntLit 1))

    [<Fact>]
    let ``Option handles int literal`` () =
        Parse.fromStringWithParser pOptionStatement @"option test = 1;"
        |> should equal (TOption ("test", TIntLit 1))

    [<Fact>]
    let ``Option handles float literal`` () =
        Parse.fromStringWithParser pOptionStatement @"option test = 1.2;"
        |> should equal (TOption ("test", TFloatLit 1.2))

    [<Fact>]
    let ``Option handles bool literal`` () =
        Parse.fromStringWithParser pOptionStatement @"option test = false;"
        |> should equal (TOption ("test", TBoolLit false))

    [<Fact>]
    let ``Option handles string literal`` () =
        Parse.fromStringWithParser pOptionStatement @"option test = 'Hello!';"
        |> should equal (TOption ("test", TStrLit "Hello!"))

    [<Fact>]
    let ``Option handles enumeration value (literal)`` () =
        Parse.fromStringWithParser pOptionStatement @"option test = MY_ENUM_VALUE;"
        |> should equal (TOption ("test", TEnumLit "MY_ENUM_VALUE"))

    [<Fact>]
    let ``Option handles qualified enumeration value (literal)`` () =
        Parse.fromStringWithParser pOptionStatement @"option test = Fully.Qualified.MY_ENUM_VALUE;"
        |> should equal (TOption ("test", TEnumLit "Fully.Qualified.MY_ENUM_VALUE"))

    [<Fact>]
    let ``Custom option parses`` () =
        Parse.fromStringWithParser pOptionStatement @"option (test)=true;"
        |> should equal (TOption ("test", TBoolLit true))

    [<Fact>]
    let ``Custom option field parses`` () =
        Parse.fromStringWithParser pOptionStatement @"option (test).field=true;"
        |> should equal (TOption ("test.field", TBoolLit true))

    [<Fact>]
    let ``Custom option dotted field parses`` () =
        Parse.fromStringWithParser pOptionStatement @"option (test).field.more = true;"
        |> should equal (TOption ("test.field.more", TBoolLit true))

    [<Fact>]
    let ``Alternate option syntax parses`` () =
        let option = """option (google.api.http) = { put: "/v1/{name=projects/*/subscriptions/*}" body: "*" };"""
        let result = Parse.fromStringWithParser Parse.Parsers.pOptionStatement option

        let expectedResults =
            [ ("put",  TStrLit "/v1/{name=projects/*/subscriptions/*}");
              ("body", TStrLit "*") ]
        let expectedResult = TOption( "google.api.http", PConstant.TAggregateOptionsLit expectedResults )

        result
        |> should equal (expectedResult)

    [<Fact>]
    let ``Numeric option parses`` () =
        let option = """option (google.api.http) = { put: "/v1/{name=projects/*/subscriptions/*}" body: 1 };"""
        let result = Parse.fromStringWithParser Parse.Parsers.pOptionStatement option

        let expectedResults =
            [ ("put",  TStrLit "/v1/{name=projects/*/subscriptions/*}");
              ("body", TIntLit 1) ]
        let expectedResult = TOption( "google.api.http", PConstant.TAggregateOptionsLit expectedResults )

        result
        |> should equal (expectedResult)

    [<Fact>]
    let ``Recursive option parses`` () =
        let option = """option (google.api.http) = {
              get: "/v1/messages/{message_id}"
              additional_bindings {
                get: "/v1/users/{user_id}/messages/{message_id}"
              }
            };"""
        let result = Parse.fromStringWithParser Parse.Parsers.pOptionStatement option

        let expectedResults =
            [ ("get",  TStrLit "/v1/messages/{message_id}");
              ("additional_bindings", TAggregateOptionsLit [ ("get",  TStrLit "/v1/users/{user_id}/messages/{message_id}") ]) ]
        let expectedResult = TOption( "google.api.http", PConstant.TAggregateOptionsLit expectedResults )

        result
        |> should equal (expectedResult)

    [<Fact>]
    let ``Two recursive options parses`` () =
        let option = """option (google.api.http) = {
              get: "/v1/messages/{message_id}"
              additional_bindings {
                get: "/v1/users/{user_id}/messages/{message_id}"
              }
              additional_bindings {
                put: "/v1/users/{user_id}/messages/{message_id}"
              }
            };"""
        let result = Parse.fromStringWithParser Parse.Parsers.pOptionStatement option

        let expectedResults =
            [ ("get",  TStrLit "/v1/messages/{message_id}");
              ("additional_bindings", TAggregateOptionsLit [ ("get",  TStrLit "/v1/users/{user_id}/messages/{message_id}") ]) 
              ("additional_bindings", TAggregateOptionsLit [ ("put",  TStrLit "/v1/users/{user_id}/messages/{message_id}") ]) ]
        let expectedResult = TOption( "google.api.http", PConstant.TAggregateOptionsLit expectedResults )

        result
        |> should equal (expectedResult)

    [<Fact>]
    let ``Two recursive options with empty statements parses`` () =
        let option = """option (google.api.http) = {
              ;
              get: "/v1/messages/{message_id}"
              ;
              additional_bindings {
                get: "/v1/users/{user_id}/messages/{message_id}"
              }
              ;
              ;
              additional_bindings {
                put: "/v1/users/{user_id}/messages/{message_id}"
              };
            };"""
        let result = Parse.fromStringWithParser Parse.Parsers.pOptionStatement option

        let expectedResults =
            [ ("get",  TStrLit "/v1/messages/{message_id}");
              ("additional_bindings", TAggregateOptionsLit [ ("get",  TStrLit "/v1/users/{user_id}/messages/{message_id}") ]) 
              ("additional_bindings", TAggregateOptionsLit [ ("put",  TStrLit "/v1/users/{user_id}/messages/{message_id}") ]) ]
        let expectedResult = TOption( "google.api.http", PConstant.TAggregateOptionsLit expectedResults )

        result
        |> should equal (expectedResult)

    [<Fact>]
    let ``Option with only empty statement parses`` () =
        let option = """option (google.api.http) = {
              ;
            };"""
        let result = Parse.fromStringWithParser Parse.Parsers.pOptionStatement option

        let expectedResults =
            List.Empty
        let expectedResult = TOption( "google.api.http", PConstant.TAggregateOptionsLit expectedResults )

        result
        |> should equal (expectedResult)

[<Xunit.Trait("Kind", "Unit")>]
module Message =

    [<Fact>]
    let ``Label parses`` () =
        [ "required", TRequired; "optional", TOptional; "repeated", TRepeated]
        |> List.iter (fun (given,expected) ->
            Parse.fromStringWithParser pLabel given
            |> should equal expected)

    [<Fact>]
    let ``Field parses`` () =
        Parse.fromStringWithParser pField @"optional int32 test = 1 ;"
        |> should equal <| TField ("test", TOptional, TInt32, 1u, [])
        Parse.fromStringWithParser pField @"required int32 test = 1 ;"
        |> should equal <| TField ("test", TRequired, TInt32, 1u, [])
        Parse.fromStringWithParser pField @"repeated int32 test = 1 ;"
        |> should equal <| TField ("test", TRepeated, TInt32, 1u, [])

    [<Fact>]
    let ``Field with options parses`` () =
        Parse.fromStringWithParser pField @"repeated sint32 samples=2 [packed=true,(custom).option=2];"
        |> should equal
        <| TField( "samples",
                    TRepeated,
                    TSInt32,
                    2u,
                    [ ("packed", TBoolLit(true)); ("custom.option", TIntLit(2)) ]
                    )

    [<Fact>]
    let ``Field with alt syntax options parses`` () =
        Parse.fromStringWithParser pField @"repeated sint32 samples=2 [(custom) = { option:2 another:false }];"
        |> should equal
           (TField( "samples",
                    TRepeated,
                    TSInt32,
                    2u,
                    [ "custom",TAggregateOptionsLit( [ ("option",TIntLit 2); ("another",TBoolLit false) ] ) ]
                    ))

    [<Fact>]
    let ``Parse simple message`` () =
        Parse.fromStringWithParser (ws >>. pMessage .>> ws)
            """message Echo {
                    required string msg=1;
                    required bytes  blob=2 [(myopt)="yes"] ;
                }"""
        |> should equal
        <| TMessage ("Echo",
            [   TField( "msg", TRequired, TString, 1u, [] )
                TField( "blob", TRequired, TBytes, 2u, [ ("myopt",TStrLit("yes")) ])
            ])

    [<Fact>]
    let ``Parse simple message with option`` () =
        Parse.fromStringWithParser (ws >>. pMessage .>> ws)
            """message Echo {
                    option (foo) = "bar";
                    required string msg=1;
                    required bytes  blob=2 [(myopt)="yes"] ;
                }"""
        |> should equal
        <| TMessage ("Echo",
            [   TMessageOption( "foo", TStrLit "bar")
                TField( "msg", TRequired, TString, 1u, [] )
                TField( "blob", TRequired, TBytes, 2u, [ ("myopt",TStrLit("yes")) ])
            ])

    [<Fact>]
    let ``Parse group`` () =
        Parse.fromStringWithParser (ws >>. pGroup) """
            optional group MyGroup = 41 {
                optional string abc = 42;
                }"""
        |> should equal
        <| TGroup( "MyGroup", TOptional, 41u,
            [ TField( "abc", TOptional, TString, 42u, [] ) ]
            )

    [<Fact>]
    let ``Parse group with option`` () =
        Parse.fromStringWithParser (ws >>. pGroup) """
            optional group MyGroup = 41 {
                optional string abc = 42;
                option (foo) = "bar";
                }"""
        |> should equal
        <| TGroup( "MyGroup", TOptional, 41u,
            [ TField( "abc", TOptional, TString, 42u, [] )
              TMessageOption( "foo", TStrLit "bar" )
            ]
            )

    [<Fact>]
    let ``Parse oneof`` () =
        Parse.fromStringWithParser (ws >>. pOneOf) """
            oneof MyOneof {
                string name = 1;
                }"""
        |> should equal
        <| TOneOf("MyOneof", [ TOneOfField("name",TString,1u,[]) ])

    [<Fact>]
    let ``Parse oneof with field option`` () =
        Parse.fromStringWithParser (ws >>. pOneOf) """
            oneof MyOneof {
                string name = 1 [(foo)="bar"];
                }"""
        |> should equal
        <| TOneOf("MyOneof", [ TOneOfField("name",TString,1u,[("foo",TStrLit("bar"))]) ])

    [<Fact>]
    let ``Parse oneof with multiple cases`` () =
        Parse.fromStringWithParser (ws >>. pOneOf) """
            oneof test_oneof {
                string name = 4;
                int32 age = 9;
            }
            """
        |> should equal
        <| TOneOf("test_oneof", [ TOneOfField("name",TString,4u,[]); TOneOfField("age",TInt32,9u,[]) ])

    [<Fact>]
    let ``Parse map`` () =
        Parse.fromStringWithParser (ws >>. pMap) """
            map<string,Project> projects = 3;"""
        |> should equal
        <| TMap ("projects", TKString, TIdent("Project"), 3u, [])

    [<Fact>]
    let ``Parse map with option`` () =
        Parse.fromStringWithParser (ws >>. pMap) """
            map<string,Project> projects = 3 [(foo)="bar"];"""
        |> should equal
        <| TMap ("projects", TKString, TIdent("Project"), 3u, ["foo",TStrLit("bar")])

    [<Fact>]
    let ``Parse extensions`` () =
        Parse.fromStringWithParser (ws >>. pExtensions) """
            extensions 100 to 199;"""
        |> should equal
        <| TExtensions [ (100u, Some(199u)) ]

        Parse.fromStringWithParser (ws >>. pExtensions) """
            extensions 42, 999 to max;"""
        |> should equal
        <| TExtensions [
             42u, None
             999u, Some(UInt32.MaxValue)]

    [<Fact>]
    let ``Parse reserved ranges`` () =
        Parse.fromStringWithParser (ws >>. pReserved) """
            reserved 42, 100 to 199, 999 to max;"""
        |> should equal
        <| TReservedRanges [
            42u, None
            100u, Some(199u)
            999u, Some(UInt32.MaxValue) ]

    [<Fact>]
    let ``Parse reserved names`` () =
        Parse.fromStringWithParser (ws >>. pReserved) """
            reserved "foo", "bar";"""
        |> should equal
        <| TReservedNames [
            "foo"
            "bar" ]

    [<Fact>]
    let ``Parse enum with option`` () =
        Parse.fromStringWithParser (ws >>. pMessageEnum) """
            enum EnumAllowingAlias {
                option allow_alias = true;
                UNKNOWN = 0;
                STARTED = 1;
                RUNNING = 2 [(custom_option) = "hello world"];
                ;
            }"""
        |> should equal (
            TMessageEnum ("EnumAllowingAlias",
                [
                TEnumOption ("allow_alias", TBoolLit(true) )
                TEnumField ("UNKNOWN", 0, [])
                TEnumField ("STARTED", 1, [])
                TEnumField ("RUNNING", 2, [ ("custom_option", TStrLit "hello world") ])
            ]))

    [<Fact>]
    let ``Parse extend`` () =
        Parse.fromStringWithParser (ws >>. pExtend) """
            extend Foo {
                optional int32 bar = 126;
            }"""
        |> should equal (
            TExtend("Foo",
                [TExtendField("bar", TOptional, TInt32, 126u, List.empty)]))

    [<Fact>]
    let ``Parse group in message`` () =
        Parse.fromStringWithParser (ws >>. pMessage) """
            message foo {
              optional group GroupMessage = 1 {
                optional int32 a = 2;
              }
            }"""
        |> should equal (
            TMessage("foo",
                [
                    TGroup("GroupMessage", TOptional, 1u,
                        [
                            TField("a", TOptional, TInt32, 2u, [])
                        ])
                ]))

[<Xunit.Trait("Kind", "Unit")>]
module Service =

    [<Fact>]
    let ``Parse rpc`` () =
        Parse.fromStringWithParser (ws >>. pRpc) """
            rpc TestMethod (outer) returns (foo);"""
        |> should equal (TRpc ("TestMethod", "outer", false, "foo", false, []))

    [<Fact>]
    let ``Parse rpc with optional option syntax`` () =
        let expectedResults =
            [ ("get", TStrLit "/v1/{name=projects/*/subscriptions/*}") ]

        let actual = 
            Parse.fromStringWithParser pRpc ("""
            rpc TestMethod (outer) returns (foo) {
                option (google.api.http) = { get: "/v1/{name=projects/*/subscriptions/*}" };
            }
            """.Trim())

        let expected = TRpc ("TestMethod", "outer", false, "foo", false, 
                        [ "google.api.http", PConstant.TAggregateOptionsLit expectedResults ])

        actual |> should equal expected

    [<Fact>]
    let ``Parse service`` () =
        Parse.fromStringWithParser (ws >>. pService) """
            service TestService {
                rpc TestMethod (outer) returns (foo);
                }"""
        |> should equal (
            TService ("TestService",
                [
                    TRpc ("TestMethod", "outer", false, "foo", false, [])
                ]))

    [<Fact>]
    let ``Parse service with rpc, options, and empty statements`` () =
        Parse.fromStringWithParser (ws >>. pService) """
            service TestService {
                ; // empty
                option (foo).bar = "fee";
                rpc TestMethod (outer) returns (foo) {
                    ; // empty
                    option (foo.baz) = "fie";
                    ;  // empty
                    option foo = { bat : "foe" qux : "foo" }
                    ; // empty
                    }
                ; // empty
                }"""
        |> should equal (
            TService ("TestService",
                [
                    TServiceOption ( "foo.bar", TStrLit "fee")
                    TRpc ("TestMethod", "outer", false, "foo", false,
                        [
                            "foo.baz", TStrLit "fie"
                            "foo", TAggregateOptionsLit [
                                "bat", TStrLit "foe"
                                "qux", TStrLit "foo"
                                ]
                        ])
                ]))



[<Xunit.Trait("Kind", "Unit")>]
module Proto =

    [<Fact>]
    let ``Parse proto syntax, import, and option`` () =
        Parse.fromStringWithParser pProto """
            /* Comment */
            /* Followed by comment */
            syntax = "proto2";

            import public "other.proto";

            option java_package = "com.example.foo";

            """
        |> should equal (
            [
                TSyntax TProto2
                TImport ("other.proto", TPublic)
                TOption ("java_package", TStrLit "com.example.foo")
            ]
        )

    [<Fact>]
    let ``Parse proto enum`` () =
        Parse.fromStringWithParser pProto """
            enum EnumAllowingAlias {
              option allow_alias = true;
              UNKNOWN = 0;
              STARTED = 1;
              RUNNING = 2 [(custom_option) = "hello world"];
            }
            """
        |> should equal (
            [
                TEnum ("EnumAllowingAlias",
                    [
                        TEnumOption ( "allow_alias", TBoolLit true)
                        TEnumField  ( "UNKNOWN", 0, [])
                        TEnumField  ( "STARTED", 1, [])
                        TEnumField  ( "RUNNING", 2, [ ("custom_option", TStrLit "hello world") ])
                    ])
            ]
        )

    [<Fact>]
    let ``Parse proto message`` () =
        Parse.fromStringWithParser pProto """
            message outer {
              option (my_option).a = true;
              message inner { // Level 2
                required int64 ival = 1;
              }
              repeated inner inner_message = 2;
              optional EnumAllowingAlias enum_field =3;
              map<int32, string> my_map = 4;
              extensions 20 to 30;
            }
            """
        |> should equal (
            [
                TMessage ("outer",
                    [
                        TMessageOption ("my_option.a", TBoolLit true)
                        TMessageMessage ( "inner", [ TField ("ival", TRequired, TInt64, 1u, [] ) ])
                        TField ("inner_message", TRepeated, TIdent "inner", 2u, [])
                        TField ("enum_field", TOptional, TIdent "EnumAllowingAlias", 3u, [])
                        TMap ("my_map", TKInt32, TString, 4u, [])
                        TExtensions ( [ (20u,Some(30u)) ])
                    ])
            ]
        )

    [<Fact>]
    let ``Parse proto group`` () =
        Parse.fromStringWithParser pProto """
            message foo {
              optional group GroupMessage = 1 {
                optional int32 a = 2;
              }
            }
            """
        |> should equal (
            [
                TMessage ( "foo",
                    [
                        TGroup("GroupMessage", TOptional, 1u,
                            [
                                TField ("a", TOptional, TInt32, 2u, [])
                            ])
                    ])
            ]
        )

    [<Fact>]
    let ``Parse proto full`` () =
        Parse.fromStringWithParser pProto """
            /* Comment */
            /* Followed by comment */
            syntax = "proto2";

            import public "other.proto";

            option java_package = "com.example.foo";

            enum EnumAllowingAlias {
              option allow_alias = true;
              UNKNOWN = 0;
              STARTED = 1;
              RUNNING = 2 [(custom_option) = "hello world"];
            }

            message outer {
              option (my_option).a = true;
              message inner { // Level 2
                required int64 ival = 1;
              }
              repeated inner inner_message = 2;
              optional EnumAllowingAlias enum_field =3;
              map<int32, string> my_map = 4;
              extensions 20 to 30;
            }

            message foo {
              optional group GroupMessage = 1 {
                optional int32 a = 2;
              }
            }

            service TestService {
                rpc TestMethod (outer) returns (foo);
                }
            """
        |> should equal (
            [
                TSyntax TProto2
                TImport ("other.proto", TPublic)
                TOption ("java_package", TStrLit "com.example.foo")
                TEnum ("EnumAllowingAlias",
                    [
                        TEnumOption ( "allow_alias", TBoolLit true)
                        TEnumField  ( "UNKNOWN", 0, [])
                        TEnumField  ( "STARTED", 1, [])
                        TEnumField  ( "RUNNING", 2, [ ("custom_option", TStrLit "hello world") ])
                    ])
                TMessage ("outer",
                    [
                        TMessageOption ("my_option.a", TBoolLit true)
                        TMessageMessage ( "inner", [ TField ("ival", TRequired, TInt64, 1u, [] ) ])
                        TField ("inner_message", TRepeated, TIdent "inner", 2u, [])
                        TField ("enum_field", TOptional, TIdent "EnumAllowingAlias", 3u, [])
                        TMap ("my_map", TKInt32, TString, 4u, [])
                        TExtensions ( [ (20u,Some(30u)) ])
                    ])
                TMessage ( "foo",
                    [
                        TGroup("GroupMessage", TOptional, 1u,
                            [
                                TField ("a", TOptional, TInt32, 2u, [])
                            ])
                    ])
                TService ("TestService",
                    [
                        TRpc ("TestMethod", "outer", false, "foo", false, [])
                    ])
            ]
        )

    [<Fact>]
    let ``Parse proto with optional option syntax`` () =
        let expectedResults =
            [ ("put",  TStrLit "/v1/{name=projects/*/subscriptions/*}");
              ("body", TStrLit "*") ]

        Parse.fromStringWithParser pProto """
            syntax = "proto3";

            service TestService {
                rpc TestMethod (outer) returns (foo) {
                    option (google.api.http) = { put: "/v1/{name=projects/*/subscriptions/*}" body: "*" };
                }
            }
            """
        |> should equal (
            [
                TSyntax TProto3
                TService ("TestService",
                    [
                        TRpc ("TestMethod", "outer", false, "foo", false, 
                            [
                                "google.api.http", PConstant.TAggregateOptionsLit expectedResults
                            ])
                    ])
            ]
        )


    [<Fact>]
    let ``Parse Google protobuf 'descriptor.proto' without error`` () =
        Parse.fromFile <| TestHelpers.getTestFilePath "google/protobuf/descriptor.proto"
        |> ignore

[<Xunit.Trait("Kind", "Unit")>]
module Proto3 =

    [<Fact>]
    let ``Proto3 fields parse without optional`` () =
        Parse.fromStringWithParser pProto """
            syntax = "proto3";
            message Test {
                int32 field1 = 1;
                string field2 = 2;
                repeated bool field3 = 3;
                }"""
        |> should equal (
            [
                TSyntax TProto3
                TMessage ("Test",
                    [
                        TField ("field1", TOptional, TInt32, 1u, [])
                        TField ("field2", TOptional, TString, 2u, [])
                        TField ("field3", TRepeated, TBool, 3u, [])
                    ])
            ])

    [<Fact>]
    let ``Proto3 accepts stream keyword on RPC`` () =
        Parse.fromStringWithParser pProto """
            syntax="proto3";
            service S {
                rpc M (stream A) returns (stream B);
                }"""
        |> should equal (
            [
                TSyntax TProto3
                TService( "S",
                    [
                        TRpc ("M", "A", true, "B", true, [])
                    ])
            ])

    [<Fact>]
    let ``Proto3 rejects optional field`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto3";
            message Test {
                optional int32 field1 = 1;
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects required field`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto3";
            message Test {
                required int32 field1 = 1;
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects message group`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto3";
            message Test {
                repeated group MyGroup = 1 {
                    repeated int32 a = 2;
                    }
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects message extensions`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto3";
            message Test {
                extensions 1 to 10;
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects message extend`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto3";
            message Test {
	            extensions 1;
	            extend Test {
		            optional bool b = 1;
		            }
	            }
            """ |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects top-level extend`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto3";
            message Test {
	            extensions 1;
	            }
	        extend Test {
		        optional bool b = 1;
		        }
            """ |> ignore
        |> should throw typeof<System.FormatException>


[<Xunit.Trait("Kind", "Unit")>]
module Proto2 =

    [<Fact>]
    let ``Proto2 fields don't parse without optional`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto2";
            message Test {
                int32 field1 = 1;
                string field2 = 2;
                repeated bool field3 = 3;
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto2 rejects stream keyword on RPC`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax="proto2";
            service S {
                rpc M (stream A) returns (stream B);
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto2 accepts optional field`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto2";
            message Test {
                optional int32 field1 = 1;
                }""" |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts required field`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto2";
            message Test {
                required int32 field1 = 1;
                }""" |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts message group`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto2";
            message Test {
                repeated group MyGroup = 1 {
                    repeated int32 a = 2;
                    }
                }""" |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts message extensions`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto2";
            message Test {
                extensions 1 to 10;
                }""" |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts message extend`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto2";
            message Test {
	            extensions 1;
	            extend Test {
		            optional bool b = 1;
		            }
	            }
            """ |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts top-level extend`` () =
        fun () -> Parse.fromStringWithParser pProto """
            syntax = "proto2";
            message Test {
	            extensions 1;
	            }
	        extend Test {
		        optional bool b = 1;
		        }
            """ |> ignore
        |> should not' (throw typeof<System.FormatException>)

[<Xunit.Trait("Kind", "Unit")>]
module RegressionTests =

    [<Fact>]
    let ``proto3 oneof type doesn't parse (#88)`` () =
        Parse.fromStringWithParser pProto """
            syntax = "proto3";

            message SampleMessage {
                oneof test_oneof {
                  string name = 4;
                  int32 age = 9;
                  }
            }
        """
        |> should equal (
            [
                TSyntax TProto3
                TMessage ("SampleMessage",
                    [
                        TOneOf("test_oneof",
                            [
                                TOneOfField("name",TString,4u,[])
                                TOneOfField("age",TInt32,9u,[])
                            ])

                    ])
            ])

    [<Fact>]
    let ``sample from proto3 users guide doesn't parse (#93)`` () =
      Parse.fromStringWithParser pProto """
        syntax = "proto3";

        message SearchRequest {
          string query = 1;
          int32 page_number = 2;
          int32 result_per_page = 3;
          enum Corpus {
            UNIVERSAL = 0;
            WEB = 1;
            IMAGES = 2;
            LOCAL = 3;
            NEWS = 4;
            PRODUCTS = 5;
            VIDEO = 6;
          }
          Corpus corpus = 4;
        }
        """
        |> should equal (
            [
                TSyntax TProto3
                TMessage ("SearchRequest",
                    [
                        TField("query", TOptional, TString, 1u, [])
                        TField("page_number", TOptional, TInt32, 2u, [])
                        TField("result_per_page", TOptional, TInt32, 3u, [])
                        TMessageEnum( "Corpus", [
                            TEnumField("UNIVERSAL", 0, [])
                            TEnumField("WEB", 1, [])
                            TEnumField("IMAGES", 2, [])
                            TEnumField("LOCAL", 3, [])
                            TEnumField("NEWS", 4, [])
                            TEnumField("PRODUCTS", 5, [])
                            TEnumField("VIDEO", 6, [])
                            ])
                        TField("corpus", TOptional, TIdent("Corpus"), 4u, [])
                    ])
            ])
module StringImport =

    [<Fact>]
    let ``Resolve Import Statement`` () =
        let files =
            [
                "test.proto",
                        """
                        syntax = "proto2";

                        import "import.proto";

                        message Test {
                            optional MyEnum a = 1;
                            }
                        """

                "import.proto",
                        """
                        enum MyEnum {
                            DEFAULT = 0;
                            ONE = 1;
                            }
                        """
            ] |> Map.ofList
        
        let ast = files |> Parse.loadFromString "test.proto"
        
        ast |> should equal (
            [ ("test.proto", [
                TSyntax TProto2
                TMessage ("Test",
                    [
                        TField ("a", TOptional, TIdent("MyEnum"), 1u, [])
                    ])
              ]);
              ("import.proto", [
                TEnum ("MyEnum",
                    [
                       TEnumField ("DEFAULT", 0, [])
                       TEnumField ("ONE", 1, []) 
                    ])
              ])
            ])

    [<Fact>]
    let ``Resolve Recursive Import Statements`` () =
        let files =
            [
                "test.proto",
                        """
                        syntax = "proto2";

                        import "import.proto";

                        message Test {
                            optional MyEnum a = 1;
                            }
                        """
                "import.proto",
                        """
                        import "inner.proto";
                        """
                "inner.proto",
                        """
                        enum MyEnum {
                            DEFAULT = 0;
                            ONE = 1;
                            }
                        """
            ] |> Map.ofList

        let ast = files |> Parse.loadFromString "test.proto"

        ast |> should equal (
            [ ("test.proto", [
                TSyntax TProto2
                TMessage ("Test",
                    [
                        TField ("a", TOptional, TIdent("MyEnum"), 1u, [])
                    ])
              ]);
              ("import.proto", []);
              ("inner.proto", [
                TEnum ("MyEnum",
                    [
                       TEnumField ("DEFAULT", 0, [])
                       TEnumField ("ONE", 1, []) 
                    ])
              ])
            ])

    [<Fact>]
    let ``Missing import throws`` () =

        let files =
            [
                "test.proto",
                    """
                    import public "missing.proto";
                    """
            ] |> Map.ofList

        fun () ->
            files
            |> Parse.loadFromString "test.proto"
            |> ignore
        |> should throw typeof<System.IO.FileNotFoundException>

    [<Fact>]
    let ``Resolve Public Import Statement`` () =
        let files =
            [
                "test.proto",
                    """
                    syntax = "proto2";

                    import public "import.proto";

                    message Test {
                        optional MyEnum a = 1;
                        }
                    """
                "import.proto",
                    """
                    enum MyEnum {
                        DEFAULT = 0;
                        ONE = 1;
                        }
                    """
            ] |> Map.ofList

        let ast = files |> Parse.loadFromString "test.proto"

        ast |> should equal (
            [ ("test.proto", [
                TSyntax TProto2
                TEnum ("MyEnum",
                    [
                       TEnumField ("DEFAULT", 0, [])
                       TEnumField ("ONE", 1, []) 
                    ])
                TMessage ("Test",
                    [
                        TField ("a", TOptional, TIdent("MyEnum"), 1u, [])
                    ])
              ])
            ])

    [<Fact>]
    let ``Resolve recursive Public Import Statement`` () =
        let files =
            [
                "test.proto",
                    """
                    syntax = "proto2";

                    import public "import.proto";

                    message Test {
                        optional MyEnum a = 1;
                        }
                    """
                "import.proto",
                    """
                    import public "inner.proto";
                    """
                "inner.proto",
                    """
                    enum MyEnum {
                        DEFAULT = 0;
                        ONE = 1;
                        }
                        """
            ] |> Map.ofList

        let ast = files |> Parse.loadFromString "test.proto"

        ast |> should equal (
            [ ("test.proto", [
                TSyntax TProto2
                TEnum ("MyEnum",
                    [
                       TEnumField ("DEFAULT", 0, [])
                       TEnumField ("ONE", 1, []) 
                    ])
                TMessage ("Test",
                    [
                        TField ("a", TOptional, TIdent("MyEnum"), 1u, [])
                    ])
              ])
            ])

    [<Fact>]
    let ``Missing public import throws`` () =
        let files =
            [
                "test.proto",
                    """
                    import "missing.proto";
                    """
            ] |> Map.ofList

        fun () ->
            files
            |> Parse.loadFromString "test.proto"
            |> ignore
        |> should throw typeof<System.IO.FileNotFoundException>


    [<Fact>]
    let ``Resolve Weak Import Statement and ignore missing weak import`` () =
        let files =
            [
                "test.proto",
                    """
                    syntax = "proto2";

                    import weak "import.proto";
                    import weak "missing.proto";

                    message Test {
                        optional MyEnum a = 1;
                        }
                    """
                "import.proto",
                    """
                    enum MyEnum {
                        DEFAULT = 0;
                        ONE = 1;
                        }
                    """
            ] |> Map.ofList

        let ast = files |> Parse.loadFromString "test.proto"

        ast |> should equal (
            [ ("test.proto", [
                TSyntax TProto2
                TMessage ("Test",
                    [
                        TField ("a", TOptional, TIdent("MyEnum"), 1u, [])
                    ])
              ]);
              ("import.proto", [
                TEnum ("MyEnum",
                    [
                       TEnumField ("DEFAULT", 0, [])
                       TEnumField ("ONE", 1, []) 
                    ])
              ])
            ])


[<Xunit.Trait("Kind", "Unit")>]
module FileImport =

    open System
    open System.IO

    [<Fact>]
    let ``Resolve File Import`` () =
        let dirs =
            [
                TestHelpers.testPath
            ]

        let ast =
            dirs |> Parse.loadFromFile "riak_kv.proto"

        // riak_kv.proto includes riak.proto
        ast
        |> List.length
        |> should equal 2

        // riak.proto contains a message definition for RpbGetServerInfoResp
        let _, riakAst = ast.[1]
        riakAst
        |> List.exists(function
            | TMessage( "RpbGetServerInfoResp", _ ) -> true
            | _ -> false
            )
        |> should equal true
