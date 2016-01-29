module TestParser

open Xunit
open FsUnit.Xunit

open System

open FParsec

open Froto.Parser.Proto
open Froto.Parser.Ast

module Identifiers =
    [<Fact>]
    let ``Identifier can be parsed`` () =
        parseString pIdent_ws "test" |> should equal "test"

    [<Fact>]
    let ``Identifier must start with a letter`` () =
        fun () -> parseString pIdent_ws "1st" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Identifier can contain A-Za-z0-9_`` () =
        let ident = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"
        parseString pIdent_ws ident |> should equal ident

    [<Fact>]
    let ``Identifier cannot contain '.'`` () =
        fun () -> parseString pIdent_ws "Test.Value" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Full identifier can contain '.'`` () =
        let fullIdent = "abc.def"
        parseString pFullIdent_ws fullIdent
        |> should equal fullIdent

    [<Fact>]
    let ``Message Type can start with '.'`` () =
        let fullyQualified = ".full.message.type"
        parseString pMessageType_ws fullyQualified
        |> should equal fullyQualified

    [<Fact>]
    let ``Group name can be parsed`` () =
        let groupName = "MyGroup"
        parseString pGroupName_ws groupName
        |> should equal groupName

    [<Fact>]
    let ``Group name must begin with upper`` () =
        fun () -> parseString pGroupName_ws "myGroup" |> ignore
        |> should throw typeof<System.FormatException>

module Literals =

    [<Fact>]
    let ``Given dec, can parse intLit`` () =
        parseString pNumLit "123"
        |> should equal (TIntLit 123)

    [<Fact>]
    let ``Given hex, can parse intLit`` () =
        parseString pNumLit "0x80"
        |> should equal (TIntLit 128)

    [<Fact>]
    let ``Given oct, can parse intLit`` () =
        parseString pNumLit "0777"
        |> should equal (TIntLit 511)

    [<Fact>]
    let ``Verify negative intLit is supported`` () =
        parseString pNumLit "-123"
        |> should equal (TIntLit -123)

    [<Fact>]
    let ``Verify fieldNum < 1 is unsupported`` () =
        fun () -> parseString pFieldNum "0" |> ignore
        |> should throw typeof<System.FormatException>
        fun () -> parseString pFieldNum "-123" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Verify fieldNum >= 2^29 is unsupported`` () =
        fun () -> parseString pFieldNum "536870912" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Given float, can parse floatLit`` () =
        parseString pNumLit "123.456"
        |> should equal (TFloatLit 123.456)

    [<Fact>]
    let ``Given float with exp, can parse floatLit`` () =
        parseString pNumLit "1.23456E2"
        |> should equal (TFloatLit 123.456)

    [<Fact>]
    let ``Verify negative floatLit is supported`` () =
        parseString pNumLit "-123.456"
        |> should equal (TFloatLit -123.456)

    [<Fact>]
    let ``Verify negative exp is supported`` () =
        parseString pNumLit "123456e-3"
        |> should equal (TFloatLit 123.456)

    [<Fact>]
    let ``Given list of bools, can parse boolLit`` () =
        [ "true", (TBoolLit true)
          "false", (TBoolLit false)  ]
        |> List.iter ( fun (given, expected) ->
            parseString pBoolLit given
            |> should equal expected
            )

    [<Fact>]
    let ``Simple string literal can be parsed`` () =
        parseString pStrLit "\"Test\""
        |> should equal (TStrLit "Test")

    [<Fact>]
    let ``Simple string between squotes can be parsed`` () =
        parseString pStrLit "'Test'"
        |> should equal (TStrLit "Test")

    [<Fact>]
    let ``String with quote between squotes can be parsed`` () =
        parseString pStrLit "'Te\"st'"
        |> should equal (TStrLit "Te\"st")

    [<Fact>]
    let ``String with squote between quotes can be parsed`` () =
        parseString pStrLit "\"Test's\""
        |> should equal (TStrLit "Test's")

    [<Fact>]
    let ``Escaped characters can be parsed`` () =
        parseString pStrLit ("\"" + @"\a\b\f\n\r\t\v\\\'\" + "\"\"")
        |> should equal (TStrLit "\a\b\f\n\r\t\v\\\'\"")

        parseString pStrLit (@"'\a\b\f\n\r\t\v\\\'\" + "\"'")
        |> should equal (TStrLit "\a\b\f\n\r\t\v\\\'\"")

    [<Fact>]
    let ``Hex escape can be parsed`` () =
        parseString pStrLit @"'\x30\x31\x32'"
        |> should equal (TStrLit "012")

    [<Fact>]
    let ``Hex throws on invalid digit`` () =
        fun () -> parseString pStrLit @"'\xFG'" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Octal escape can be parsed`` () =
        parseString pStrLit @"'\101\040\102'"
        |> should equal (TStrLit "A B")

    [<Fact>]
    let ``Octal throws on invalid digit`` () =
        fun () -> parseString pStrLit @"'\008'" |> ignore
        |> should throw typeof<System.FormatException>
        fun () -> parseString pStrLit @"'\009'" |> ignore
        |> should throw typeof<System.FormatException>

module SyntaxStatement =

    [<Fact>]
    let ``Syntax 'proto2' parses`` () =
        parseString pSyntax @"syntax = 'proto2';"
        |> should equal (TSyntax TProto2)

    [<Fact>]
    let ``Syntax 'proto3' parses`` () =
        parseString pSyntax @"syntax = 'proto3';"
        |> should equal (TSyntax TProto3)

    [<Fact>]
    let ``Invalid syntax throws`` () =
        fun () -> parseString pSyntax @"syntax = 'protox';" |> ignore
        |> should throw typeof<System.FormatException>

module ImportStatement =

    [<Fact>]
    let ``Import parses`` () =
        parseString pImport @"import 'test.proto';"
        |> should equal (TImport ("test.proto", TNormal))

    [<Fact>]
    let ``Import public parses`` () =
        parseString pImport @"import public 'test.proto';"
        |> should equal (TImport ("test.proto", TPublic))

    [<Fact>]
    let ``Import weak parses`` () =
        parseString pImport @"import weak 'test.proto';"
        |> should equal (TImport ("test.proto", TWeak))

module PackageStatement =

    [<Fact>]
    let ``Package parses`` () =
        parseString pPackage @"package abc.def;"
        |> should equal (TPackage "abc.def")

module OptionStatement =

    [<Fact>]
    let ``Simple option parses`` () =
        parseString pOptionStatement @"option test=1;"
        |> should equal (TOption ("test", TIntLit 1))

    [<Fact>]
    let ``Option handles int literal`` () =
        parseString pOptionStatement @"option test = 1;"
        |> should equal (TOption ("test", TIntLit 1))

    [<Fact>]
    let ``Option handles float literal`` () =
        parseString pOptionStatement @"option test = 1.2;"
        |> should equal (TOption ("test", TFloatLit 1.2))

    [<Fact>]
    let ``Option handles bool literal`` () =
        parseString pOptionStatement @"option test = false;"
        |> should equal (TOption ("test", TBoolLit false))

    [<Fact>]
    let ``Option handles string literal`` () =
        parseString pOptionStatement @"option test = 'Hello!';"
        |> should equal (TOption ("test", TStrLit "Hello!"))

    [<Fact>]
    let ``Option handles enumeration value (literal)`` () =
        parseString pOptionStatement @"option test = MY_ENUM_VALUE;"
        |> should equal (TOption ("test", TEnumLit "MY_ENUM_VALUE"))

    [<Fact>]
    let ``Option handles qualified enumeration value (literal)`` () =
        parseString pOptionStatement @"option test = Fully.Qualified.MY_ENUM_VALUE;"
        |> should equal (TOption ("test", TEnumLit "Fully.Qualified.MY_ENUM_VALUE"))

    [<Fact>]
    let ``Custom option parses`` () =
        parseString pOptionStatement @"option (test)=true;"
        |> should equal (TOption ("test", TBoolLit true))

    [<Fact>]
    let ``Custom option field parses`` () =
        parseString pOptionStatement @"option (test).field=true;"
        |> should equal (TOption ("test.field", TBoolLit true))

    [<Fact>]
    let ``Custom option dotted field parses`` () =
        parseString pOptionStatement @"option (test).field.more = true;"
        |> should equal (TOption ("test.field.more", TBoolLit true))

module Message =

    [<Fact>]
    let ``Label parses`` () =
        [ "required", TRequired; "optional", TOptional; "repeated", TRepeated]
        |> List.iter (fun (given,expected) ->
            parseString pLabel given
            |> should equal expected)

    [<Fact>]
    let ``Field parses`` () =
        parseString pField @"required int32 test = 1 ;"
        |> should equal <| TField ("test", TRequired, TInt32, 1u, [])

    [<Fact>]
    let ``Field with options parses`` () =
        parseString pField @"repeated sint32 samples=2 [packed=true,(custom).option=2];"
        |> should equal
        <| TField( "samples",
                    TRepeated,
                    TSInt32,
                    2u,
                    [ ("packed", TBoolLit(true)); ("custom.option", TIntLit(2)) ]
                    )

    [<Fact>]
    let ``Parse simple message`` () =
        parseString (ws >>. pMessage .>> ws)
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
    let ``Parse group`` () =
        parseString (ws >>. pGroup) """
            optional group MyGroup = 41 {
                optional string abc = 42;
                }"""
        |> should equal
        <| TGroup( "MyGroup", TOptional, 41u,
            [ TField( "abc", TOptional, TString, 42u, [] ) ]
            )

    [<Fact>]
    let ``Parse oneof`` () =
        parseString (ws >>. pOneOf) """
            oneof MyOneof {
                string name = 1;
                }"""
        |> should equal
        <| TOneOf("MyOneof", [ TOneOfField("name",TString,1u,[]) ])

    [<Fact>]
    let ``Parse map`` () =
        parseString (ws >>. pMap) """
            map<string,Project> projects = 3;"""
        |> should equal
        <| TMap ("projects", TKString, TIdent("Project"), 3u)

    [<Fact>]
    let ``Parse extensions`` () =
        parseString (ws >>. pExtensions) """
            extensions 100 to 199;"""
        |> should equal
        <| TExtensions [ (100u, Some(199u)) ]

        parseString (ws >>. pExtensions) """
            extensions 42, 999 to max;"""
        |> should equal
        <| TExtensions [
             42u, None
             999u, Some(UInt32.MaxValue)]

    [<Fact>]
    let ``Parse reserved ranges`` () =
        parseString (ws >>. pReserved) """
            reserved 42, 100 to 199, 999 to max;"""
        |> should equal
        <| TReservedRanges [
            42u, None
            100u, Some(199u)
            999u, Some(UInt32.MaxValue) ]

    [<Fact>]
    let ``Parse reserved names`` () =
        parseString (ws >>. pReserved) """
            reserved "foo", "bar";"""
        |> should equal
        <| TReservedNames [
            "foo"
            "bar" ]

    [<Fact>]
    let ``Parse enum`` () =
        parseString (ws >>. pMessageEnum) """
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
        parseString (ws >>. pExtend) """
            extend Foo {
                optional int32 bar = 126;
            }"""
        |> should equal (
            TExtend("Foo",
                [TExtendField("bar", TOptional, TInt32, 126u, List.empty)]))

    [<Fact>]
    let ``Parse group in message`` () =
        parseString (ws >>. pMessage) """
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

module Service =

    [<Fact>]
    let ``Parse rpc`` () =
        parseString (ws >>. pRpc) """
            rpc TestMethod (outer) returns (foo);"""
        |> should equal (TRpc ("TestMethod", "outer", false, "foo", false, []))

    [<Fact>]
    let ``Parse service`` () =
        parseString (ws >>. pService) """
            service TestService {
                rpc TestMethod (outer) returns (foo);
                }"""
        |> should equal (
            TService ("TestService",
                [
                    TRpc ("TestMethod", "outer", false, "foo", false, [])
                ]))

module Proto =

    [<Fact>]
    let ``Parse proto syntax, import, and option`` () =
        parseString pProto """
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
        parseString pProto """
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
        parseString pProto """
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
                        TMap ("my_map", TKInt32, TString, 4u)
                        TExtensions ( [ (20u,Some(30u)) ])
                    ])
            ]
        )

    [<Fact>]
    let ``Parse proto group`` () =
        parseString pProto """
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
        parseString pProto """
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
                        TMap ("my_map", TKInt32, TString, 4u)
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
    let ``Parse Google protobuf 'descriptor.proto' without error`` () =
        parseFile pProto "data/google/protobuf/descriptor.proto"
        |> ignore

module Proto3 =

    [<Fact>]
    let ``Proto3 fields parse without optional`` () =
        parseString pProto """
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
        parseString pProto """
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
        fun () -> parseString pProto """
            syntax = "proto3";
            message Test {
                optional int32 field1 = 1;
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects required field`` () =
        fun () -> parseString pProto """
            syntax = "proto3";
            message Test {
                required int32 field1 = 1;
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects message group`` () =
        fun () -> parseString pProto """
            syntax = "proto3";
            message Test {
                repeated group MyGroup = 1 {
                    repeated int32 a = 2;
                    }
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects message extensions`` () =
        fun () -> parseString pProto """
            syntax = "proto3";
            message Test {
                extensions 1 to 10;
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto3 rejects message extend`` () =
        fun () -> parseString pProto """
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
        fun () -> parseString pProto """
            syntax = "proto3";
            message Test {
	            extensions 1;
	            }
	        extend Test {
		        optional bool b = 1;
		        }
            """ |> ignore
        |> should throw typeof<System.FormatException>

module Proto2 =

    [<Fact>]
    let ``Proto2 fields don't parse without optional`` () =
        fun () -> parseString pProto """
            syntax = "proto2";
            message Test {
                int32 field1 = 1;
                string field2 = 2;
                repeated bool field3 = 3;
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto2 rejects stream keyword on RPC`` () =
        fun () -> parseString pProto """
            syntax="proto2";
            service S {
                rpc M (stream A) returns (stream B);
                }""" |> ignore
        |> should throw typeof<System.FormatException>

    [<Fact>]
    let ``Proto2 accepts optional field`` () =
        fun () -> parseString pProto """
            syntax = "proto2";
            message Test {
                optional int32 field1 = 1;
                }""" |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts required field`` () =
        fun () -> parseString pProto """
            syntax = "proto2";
            message Test {
                required int32 field1 = 1;
                }""" |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts message group`` () =
        fun () -> parseString pProto """
            syntax = "proto2";
            message Test {
                repeated group MyGroup = 1 {
                    repeated int32 a = 2;
                    }
                }""" |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts message extensions`` () =
        fun () -> parseString pProto """
            syntax = "proto2";
            message Test {
                extensions 1 to 10;
                }""" |> ignore
        |> should not' (throw typeof<System.FormatException>)

    [<Fact>]
    let ``Proto2 accepts message extend`` () =
        fun () -> parseString pProto """
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
        fun () -> parseString pProto """
            syntax = "proto2";
            message Test {
	            extensions 1;
	            }
	        extend Test {
		        optional bool b = 1;
		        }
            """ |> ignore
        |> should not' (throw typeof<System.FormatException>)

