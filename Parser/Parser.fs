﻿namespace Froto.Parser

#nowarn "40"  // Turn off warning about defining recursive objects

module Parse =

    module Parsers =

        open System
        open FParsec
        open Froto.Parser.Ast

        // Parser State
        type State = { Syntax : PSyntax }
            with
                static member Default = { Syntax = TProto2 }

        let internal isProto2 =
            userStateSatisfies (fun us -> us.Syntax = TProto2)

        let internal isProto3 =
            userStateSatisfies (fun us -> us.Syntax = TProto3)

        let internal setProto2 =
            updateUserState (fun us -> {us with Syntax = TProto2 })

        let internal setProto3 =
            updateUserState (fun us -> {us with Syntax = TProto3 })

        // Generic helpers

        let internal defArg def =
            let flip f b a = f a b
            flip defaultArg def

        // Define our own whitespace, so we can treat comments as ws
        let internal skipWhiteSpace : Parser<unit,State> =
            pchar ' ' <|> tab <|> newline
            |>> ignore

        /// Parse single-line comment
        let internal pSingleLineComment =
            pstring "//" >>. restOfLine true

        /// Parse multi-line comment
        let internal  pMultiLineComment =
            (pstring "/*") >>. (charsTillString "*/" true Int32.MaxValue)

        /// Skip comment
        let internal skipComment : Parser<unit,State> =
            pSingleLineComment <|> pMultiLineComment
            |>> ignore

        /// Skip whitespace OR comment
        let ws  = skipMany (skipWhiteSpace <|> skipComment)

        /// Skip at least 1 space followed by whitespace or comment
        let ws1 = spaces1 >>. ws

        /// Alias for pstring
        let str = pstring

        /// Parse string followed by zero or more whitespace
        let str_ws s = str s .>> ws

        /// Parse string followed by one or more whitespace
        let str_ws1 s = str s .>> ws1

        /// Parse between curly braces { ... }
        let inline betweenCurly p =
            between (str_ws "{") (str_ws "}") p

        /// Parse between square brackets [ ... ]
        let inline betweenSquare p =
            between (str_ws "[") (str_ws "]") p

        /// Parse between parens ( ... )
        let inline betweenParens p =
            between (str_ws "(") (str_ws ")") p


        // Run parser on string between quote (") or single-quote (')
        let inline betweenQuotes p =
            let dquote = between (str "\"") (str "\"")
            let squote = between (str "'") (str "'")
            dquote p <|> squote p

        // Parsers for identifiers
        let internal pUndottedIdent : Parser<string,State> =
            identifier (IdentifierOptions(
                            isAsciiIdStart    = isAsciiLetter,
                            isAsciiIdContinue = (fun c -> isAsciiLetter c || isDigit c || c = '_')))

        let internal pDottedIdent : Parser<string,State> =
            identifier (IdentifierOptions(
                            isAsciiIdStart    = isAsciiLetter,
                            isAsciiIdContinue = (fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '.')))

        let internal pFullyQualifiedIdent : Parser<string,State> =
            identifier (IdentifierOptions(
                            isAsciiIdStart    = (fun c -> isAsciiLetter c || c = '.'),
                            isAsciiIdContinue = (fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '.')))

        let internal pUpperIdent : Parser<string,State> =
            identifier (IdentifierOptions(
                            isAsciiIdStart    = isAsciiUpper,
                            isAsciiIdContinue = (fun c -> isAsciiLetter c || isDigit c || c = '_')))

        // Identifier aliases, used by Protobuf Language Specification
        let pIdent_ws           = pUndottedIdent .>> ws
        let pFullIdent_ws       = pDottedIdent .>> ws
        let pMessageName_ws     = pIdent_ws
        let pEnumName_ws        = pIdent_ws
        let pFieldName_ws       = pIdent_ws
        let pOneOfName_ws       = pIdent_ws
        let pMapName_ws         = pIdent_ws
        let pServiceName_ws     = pIdent_ws
        let pRpcName_ws         = pIdent_ws
        let pStreamName_ws      = pIdent_ws
        let pMessageType_ws     = pFullyQualifiedIdent .>> ws
        let pEnumType_ws        = pFullyQualifiedIdent .>> ws
        let pGroupName_ws       = pUpperIdent .>> ws

        /// Parser for int or float literal
        let pNumLit : Parser<PConstant,State> =
            let numFormat =
                    NumberLiteralOptions.AllowHexadecimal
                ||| NumberLiteralOptions.AllowExponent
                ||| NumberLiteralOptions.AllowFraction
                ||| NumberLiteralOptions.AllowMinusSign
                ||| NumberLiteralOptions.DefaultInteger

            numberLiteral numFormat "Numeric literal"
            |>> fun nl ->
                if nl.IsInteger
                then
                    if nl.IsDecimal && nl.String.[0] = '0'
                    then TIntLit (int32 ("0o" + nl.String))
                    else TIntLit (int32 nl.String)
                else TFloatLit (float <| nl.String)

        /// Parser for int
        let pInt : Parser<int32,State> =
            let numFormat =
                    NumberLiteralOptions.AllowHexadecimal
                ||| NumberLiteralOptions.AllowMinusSign
                ||| NumberLiteralOptions.DefaultInteger

            numberLiteral numFormat "Integer"
            |>> fun nl ->
                if nl.IsDecimal && nl.String.[0] = '0'
                then int32 ("0o" + nl.String)
                else int32 nl.String
        let pInt_ws     = pInt .>> ws
        let pEq_Int_ws  = str_ws "=" >>. pInt_ws

        /// Parser for field number
        let rec pFieldNum : Parser<uint32,State> =
            let numFormat =
                    NumberLiteralOptions.AllowHexadecimal
                ||| NumberLiteralOptions.DefaultUnsignedInteger

            numberLiteral numFormat "Field number (integer between 1 .. 2^29)"
            >>= ( fun nl -> parse {
                let n =
                    if nl.IsDecimal && nl.String.[0] = '0'
                    then uint32 ("0o" + nl.String)
                    else uint32 nl.String

                if n > 0u && n < (1u <<< 29)
                then return n
                else return! fail "Field number must be between 1 .. 2^29"
                })

        let pFieldNum_ws        = pFieldNum .>> ws
        let pEq_FieldNum_ws     = str_ws "=" >>. pFieldNum_ws

        /// Parser for bool
        let pBoolLit : Parser<PConstant,State> =
                stringReturn "true"  (TBoolLit true)
            <|> stringReturn "false" (TBoolLit false)
            <?> "Boolean literal"

        // String literal
        let internal strLit =
            let escape =  anyOf "abfnrtv\\\'\""
                          |>> function
                              | 'a' -> "\a"
                              | 'b' -> "\b"
                              | 'f' -> "\u000C"
                              | 'n' -> "\n"
                              | 'r' -> "\r"
                              | 't' -> "\t"
                              | 'v' -> "\v"
                              | c   -> string c // every other char is mapped to itself

            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9
            let oct2int c = (int c &&& 7)

            let unicodeEscape =
                str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                    |> char |> string
                )

            let hexEscape =
                str "x" >>. pipe2 hex hex (fun h1 h0 ->
                    (hex2int h1)*16 + hex2int h0
                    |> char |> string
                )

            let octEscape =
                let octDigit = anyOf "01234567"
                pipe3 octDigit octDigit octDigit (fun o2 o1 o0 ->
                    (oct2int o2)*64 + (oct2int o1)*8 + oct2int o0
                    |> char |> string
                )

            let escapedCharSnippet = str "\\" >>. (escape <|> hexEscape <|> octEscape)
            let normalCharSnippet  quote = manySatisfy (fun c -> c <> quote && c <> '\\')

            between (str "\"") (str "\"")
                    (stringsSepBy (normalCharSnippet '"') escapedCharSnippet)
                <|>
            between (str "'") (str "'")
                    (stringsSepBy (normalCharSnippet '\'') escapedCharSnippet)
        let internal strLit_ws = strLit .>> ws

        /// Parser for string literal bracketed by either quotes (") or single-quotes (')
        let pStrLit =
            strLit |>> TStrLit
            <?> "String literal"

        /// Parser for enumeration value
        let pEnumLit =
            pFullyQualifiedIdent
            |>> TEnumLit

        /// Parser for constant: (boolLit | strLit | intLit | floatLit | Ident)
        let pConstant = pBoolLit <|> pStrLit <|> pNumLit <|> pEnumLit
        let pConstant_ws = pConstant .>> ws

        // Parser for end-of-statement
        let internal eostm = str ";" .>> ws // note the implicit ws

        (*--- Statement Parsers ---*)

        /// Parser for syntax: "syntax" "=" quote ("proto2" | "proto3") quote ";"
        /// Also updates parser state with Syntax=(TProto2|TProto3)
        let pSyntax =
            str_ws "syntax" >>. str_ws "=" >>. betweenQuotes
                ( (str_ws "proto2" .>> setProto2) <|> (str_ws "proto3" .>> setProto3) )
                 .>> eostm
            |>> function
                | "proto2" -> TSyntax TProto2
                | "proto3" -> TSyntax TProto3
                | _ -> failwithf "unexpected"

        // Import parsers
        let internal import =
            str_ws1 "import" >>. strLit_ws .>> eostm
            |>> fun s -> (s,TNormal)

        let internal importPublic =
            str_ws1 "import public" >>. strLit_ws .>> eostm
            |>> fun s -> (s,TPublic)

        let internal importWeak =
            str_ws1 "import weak" >>. strLit_ws .>> eostm
            |>> fun s -> (s,TWeak)

        /// Parser for import: "import" [ "public" | "weak" ] strLit ";"
        let pImport =
                attempt import
            <|> attempt importPublic
            <|> attempt importWeak
            |>> TImport
            <?> "Expecting 'import [public|weak] string-literal;'"

        /// Parser for package: "package" fullIdent ";"
        let pPackage =
            str_ws1 "package" >>. pFullIdent_ws .>> eostm
            |>> TPackage

        /// Parser for optionName: (ident | "(" fullIdent ")") {"." ident}
        let pOptionName =
            let pIdentCustom_ws =
                betweenParens pFullIdent_ws .>>. opt (many (str "." >>. pIdent_ws))
                |>> fun (left, optRight) ->
                        match optRight with
                        | None -> left
                        | Some(xs) -> String.Join(".", left :: xs)

            pIdent_ws <|> pIdentCustom_ws

        /// Parser for optionName + ws
        let pOptionName_ws = pOptionName .>> ws

        /// Parser for optionClause: optionName "=" constant
        let pOption_ws =
            pOptionName_ws .>> str_ws "=" .>>. pConstant_ws

        /// Parser for option: "option" optionClause ";"
        let pOptionStatement =
            str_ws1 "option" >>. pOption_ws .>> eostm
            |>> TOption

        let pLabel : Parser<PLabel,State> =
            let labelMap =
                [
                    "required", TRequired
                    "optional", TOptional
                    "repeated", TRepeated
                ] |> Map.ofList

            let toLabel s = labelMap.[s]

            ( str "required" <|> str "optional" <|> str "repeated" )
            |>> toLabel

        let internal pLabel_P3 =
            // TODO: figure out how to return an error if 'optional' is specified
            // TODO: same for 'required'
            //let todoMsg = "Explicit 'optional' fields are prohibited in proto3 syntax.  Since all fields are optional by default, the 'optional' label should simply be removed."
            (opt (str_ws1 "repeated" >>% TRepeated)) |>> defArg TOptional

        /// Parser for proto2 lable + ws
        let pLabel_ws1 = pLabel .>> ws1

        /// Parser for types: double | int32 | etc...
        let pType : Parser<PType,State> =
            let typeMap =
                [
                    "double",   TDouble;    "float",    TFloat
                    "int32",    TInt32;     "int64",    TInt64
                    "uint32",   TUInt32;    "uint64",   TUInt64
                    "sint32",   TSInt32;    "sint64",   TSInt64
                    "fixed32",  TFixed32;   "fixed64",  TFixed64
                    "sfixed32", TSFixed32;  "sfixed64", TSFixed64
                    "bool",     TBool
                    "string",   TString;    "bytes",    TBytes
                ] |> Map.ofList

            let toType s = typeMap.[s]

            let pPrimitiveType =
                ( choice
                    [ str "double"  ; str "float"     ;
                      str "int32"   ; str "int64"     ;
                      str "uint32"  ; str "uint64"    ;
                      str "sint32"  ; str "sint64"    ;
                      str "fixed32" ; str "fixed64"   ;
                      str "sfixed32"; str "sfixed64"  ;
                      str "bool"    ;
                      str "string"  ; str "bytes"
                    ]
                ) .>> ws |>> toType

            let pIdentType =
                pMessageType_ws
                |>> TIdent

            pPrimitiveType <|> pIdentType

        /// Parser for type + ws
        let pType_ws = pType .>> ws

        /// Parser for field options
        let pFieldOption_ws : Parser<POption list,State> =
            betweenSquare <| sepBy pOption_ws (str_ws ",")

        let internal pFieldCommon =
            pipe5
                ((isProto2 >>. pLabel_ws1) <|> (isProto3 >>. pLabel_P3))
                pType_ws
                pFieldName_ws
                pEq_FieldNum_ws
                (opt pFieldOption_ws |>> defArg [])
                (fun lbl typ ident num opts ->
                    (ident,lbl,typ,num,opts) )
            .>> eostm

        /// Parser for field: proto2: ("required" | "optional" | "repeated") ident "=" intLit [ "[" { fieldOptions } "]" ] ";"
        let pField =
          pFieldCommon |>> TField

        let pExtendField =
          pFieldCommon |>> TExtendField

        // Message Statements and Body

        // Recursive parser declarations
        let internal pMessageDef, internal pMessageDefR = createParserForwardedToRef<String*PMessageStatement list,State>()
        let internal pGroupCommon, internal pGroupCommonR = createParserForwardedToRef<TIdent * PLabel * FieldNum * PMessageStatement list,State>()

        // Top Level Statements

        let internal skipEmptyStmts =
            skipMany eostm

        let internal pEnumCommon =

            let pEnumOption_ws =
                str_ws1 "option" >>. pOption_ws
                |>> TEnumOption

            let pEnumField_ws =
                pipe3
                    (pIdent_ws)
                    (pEq_Int_ws)
                    (opt pFieldOption_ws |>> defArg [])
                    (fun name num opts ->
                        TEnumField(name, num, opts)
                    )

            let pEnumStatement =
                ( pEnumOption_ws <|> pEnumField_ws ) .>> eostm
                .>> skipEmptyStmts

            let pEnumBody =
                betweenCurly (skipEmptyStmts >>. many pEnumStatement)

            str_ws1 "enum" >>. pEnumName_ws .>>. pEnumBody

        /// Parse enum defined in a message body
        let pMessageEnum =
          pEnumCommon |>> TMessageEnum

        /// Parse an enum defined at the top level
        let pEnum =
          pEnumCommon |>> TEnum

        /// Parse message: "message" messageName messageBody
        ///       messageBody: proto2: "{" { field | enum | message | extend | extensions | group | option | oneof | mapfield | reserved | emptyStatement } "}"
        ///       messageBody: proto3: "{" { field | enum | message | option | oneof | mapfield | reserved | emptyStatement } "}"
        let rec pMessage =
            pMessageDef |>> TMessage

        // Implementation of fwd decl pMessageDef
        and internal pMessageDefImpl =
            str_ws1 "message" >>. pMessageName_ws .>>. pMessageBody

        // Parse messageBody
        and internal pMessageBody =
            betweenCurly (many (skipEmptyStmts >>. pMessageStatement .>> skipEmptyStmts))

        // Message statement: field | enum | etc.
        and internal pMessageStatement =
            let pMessageMessage =
                pMessageDef |>> TMessageMessage

            choice [
                (isProto2 >>. pGroup) // must be parsed first to avoid confusion
                pField
                pMessageEnum
                pMessageMessage
                (isProto2 >>. pMessageExtend)
                (isProto2 >>. pExtensions)
                pMessageOption
                pOneOf
                pMap
                pReserved
                ]

        /// Parse message option: "option" (ident | "(" fullIdent ")" { "." ident }
        and pMessageOption =
            str_ws1 "option" >>. pOption_ws .>> eostm
            |>> TMessageOption

        /// Parse top-level group: label "group" groupName "=" fieldNumber messageBody
        and pGroup =
            pGroupCommon |>> TGroup

        // Parse extend group
        and internal pExtendGroup =
            pGroupCommon |>> TExtendGroup

        // Implementation for fwd decl of pGroupCommon
        and internal pGroupCommonImpl =
            attempt <| pipe4
                pLabel_ws1
                (str_ws1 "group" >>. pGroupName_ws)
                pEq_FieldNum_ws
                pMessageBody
                (fun lbl gname fldnum body ->
                    (gname,lbl,fldnum,body))

        /// Parse oneof: "oneof" oneOfName "{" { oneofField | emptyStatement } "}"
        ///       oneOfField: type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";'
        and pOneOf =
            (str_ws1 "oneof" >>. pOneOfName_ws) .>>.
            (betweenCurly (many (skipEmptyStmts >>. pOneOfField .>> skipEmptyStmts)) )
            |>> TOneOf

        and internal pOneOfField =
            pipe4
                pType_ws
                pFieldName_ws
                pEq_FieldNum_ws
                (opt pFieldOption_ws)
                (fun typ ident num opts->
                    let opts =
                        match opts with
                        | None -> []
                        | Some(os) -> os
                    TOneOfField (ident,typ,num,opts) )
            .>> eostm

        /// Parse map: "map" "<" keyType "," type ">" manName "=" fieldNumber [ "[" fieldOptions "]" ] ";'
        and pMap =
            let ktypeMap =
                [
                    "int32",    TKInt32;     "int64",    TKInt64
                    "uint32",   TKUInt32;    "uint64",   TKUInt64
                    "sint32",   TKSInt32;    "sint64",   TKSInt64
                    "fixed32",  TKFixed32;   "fixed64",  TKFixed64
                    "sfixed32", TKSFixed32;  "sfixed64", TKSFixed64
                    "bool",     TKBool
                    "string",   TKString
                ] |> Map.ofList
            let toKType s = ktypeMap.[s]

            let pKeyType_ws =
                ( choice [
                    str "int32"   ; str "int64"     ;
                    str "uint32"  ; str "uint64"    ;
                    str "sint32"  ; str "sint64"    ;
                    str "fixed32" ; str "fixed64"   ;
                    str "sfixed32"; str "sfixed64"  ;
                    str "bool"    ;
                    str "string"
                    ]
                ) .>> ws |>> toKType

            pipe4
                (str_ws "map" >>. str_ws "<" >>. pKeyType_ws)
                (str_ws "," >>. pType_ws .>> str_ws ">")
                pMapName_ws
                pEq_FieldNum_ws
                (fun ktype vtype name num -> TMap(name,ktype,vtype,num) )
            .>> eostm

        /// Parse extensions: "extensions" ranges ";"
        ///       ranges: range { "," range }
        ///       range: intLit | "to" ( intLit | "max" ) ]
        and pExtensions =
            str_ws1 "extensions" >>. pRanges .>> str_ws ";"
            |>> (fun xs -> TExtensions xs)

        /// Parse reserved: "reserved" ranges ";"
        ///       ranges: range { "," range }
        ///       range: intLit | "to" ( intLit | "max" ) ]
        and pReserved =
            let pResRanges =
                pRanges
                    |>> (fun xs -> TReservedRanges xs)

            let pResNames =
                sepBy1 (betweenQuotes pFieldName_ws) (str_ws ",")
                    |>> (fun xs -> TReservedNames xs)

            str_ws1 "reserved" >>. (pResRanges <|> pResNames) .>> eostm

        // Parse list of ranges
        and internal pRanges =
            sepBy1 pRange (str_ws ",")

        // Parse range
        and internal pRange =
            let pNumOrMax_ws =
                pFieldNum_ws
                    <|>
                (str_ws "max" |>> (fun _ -> UInt32.MaxValue))

            attempt (pipe2
                pFieldNum_ws
                (opt (str_ws "to" >>. pNumOrMax_ws))
                (fun b e -> (b,e)))
            <?> "expecting '<fieldNum> [to <fieldNum> | max]'"

        /// Parse message-level extend: "extend" messageType "{" {field | group | emptyStatement} "}"
        and pMessageExtend =
            pExtendCommon |>> TMessageExtend

        /// Parse top-level extend: "extend" messageType "{" {field | group | emptyStatement} "}"
        and pExtend =
            pExtendCommon |>> TExtend

        and internal pExtendCommon =
            (str_ws1 "extend" >>. pMessageType_ws)
                .>>. betweenCurly (many (skipEmptyStmts >>. (attempt pExtendField <|> pExtendGroup) .>> skipEmptyStmts))

        /// Parse service: "service" serviceName "{" { option | rpc | emptyStatement } "}"
        and pService : Parser<PStatement,State> =
            str_ws1 "service" >>. pServiceName_ws .>>.
                (betweenCurly (many (skipEmptyStmts >>. (attempt pServiceOption <|> pRpc) .>> skipEmptyStmts)))
            |>> TService

        /// Parse service options
        and pServiceOption =
            pOption_ws |>> TServiceOption

        /// Parse rpc: proto2: "rpc" rpcName "{" messageType ")" "returns" "(" messageType ")" [ "{" { option | emptyStatement } "}" ] ";"
        /// Parse rpc: proto3: "rpc" rpcName "{" ["stream"] messageType ")" "returns" "(" ["stream"] messageType ")" [ "{" { option | emptyStatement } "}" ] ";"
        and pRpc =
            let pStrMessageType_P2 =
                betweenParens pMessageType_ws
                |>> fun id -> (false, id)

            let pStrMessageType_P3 =
                betweenParens
                    ( (opt <| str_ws1 "stream") .>>. pMessageType_ws )
                |>> fun (o,id) -> (o.IsSome, id)

            let pStrMessageType =
                (isProto2 >>. pStrMessageType_P2)
                    <|>
                (isProto3 >>. pStrMessageType_P3)

            let pRpcOptions =
                betweenCurly
                    (sepBy pOption_ws (str_ws ","))

            pipe4
                (str_ws1 "rpc" >>. pRpcName_ws)
                pStrMessageType
                (str_ws "returns" >>. pStrMessageType)
                (opt pRpcOptions |>> defArg [])
                (fun ident (bsReq,req) (bsResp,resp) opts ->
                    TRpc( ident, req, bsReq, resp, bsResp, opts)
                    )
            .>> eostm


        /// Parse protobuf: proto2: syntax | import | package | option | message | enum | extend | service | emptyStatement
        /// Parse protobuf: proto2: syntax | import | package | option | message | enum | service | emptyStatement
        let rec pProto =
            ws >>.
            many (skipEmptyStmts >>. pProtoStatement .>> skipEmptyStmts)
            .>> eof

        and internal pProtoStatement =
            choice [
                pSyntax
                pImport
                pPackage
                pOptionStatement
                pMessage
                pEnum
                (isProto2 >>. pExtend)
                pService
                ]

        // Resolve recursive parsers
        do pMessageDefR := pMessageDefImpl
        do pGroupCommonR := pGroupCommonImpl

(* Parse module *)

    open FParsec

    let internal resultOrFail parserResult =
        match parserResult with
        | Success (result, _, _) -> result
        | Failure (errMsg, _, _) -> raise <| System.FormatException(errMsg)

    /// Parse proto from a string using the specified parser.  Throws System.FormatException on failure.
    let fromStringWithParser parser str =
        runParserOnString (parser .>> eof) Parsers.State.Default System.String.Empty str
        |> resultOrFail

    /// Parse proto from a stream using the specified parser.  Throws System.FormatException on failure.
    let fromStreamWithParser parser streamName stream =
        runParserOnStream (parser .>> eof) Parsers.State.Default streamName stream System.Text.Encoding.UTF8
        |> resultOrFail

    /// Parse proto from a file using the specified parser.  Throws System.FormatException on failure.
    let fromFileWithParser parser fileName =
        runParserOnFile (parser .>> eof) Parsers.State.Default fileName System.Text.Encoding.UTF8
        |> resultOrFail

    /// Parse proto from a string.  Throws System.FormatException on failure.
    let fromString str =
        fromStringWithParser Parsers.pProto str

    /// Parse proto from a stream.  Throws System.FormatException on failure.
    let fromStream streamName stream =
        fromStreamWithParser Parsers.pProto streamName stream

    /// Parse proto from a file.  Throws System.FormatException on failure.
    let fromFile fileName =
        fromFileWithParser Parsers.pProto fileName
