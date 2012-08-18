
module Froto.ProtoParser

// http://code.google.com/apis/protocolbuffers/docs/proto.html
// http://www.quanttec.com/fparsec/

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

/// testing xUnit setup :)
let capitalize (s:string) = s.ToUpper()

let whitespaceChars = [|' '; '\t'|]
let isWhitespaceChar c = c = ' ' || c = '\t'
let trim (s:string) = s.Trim whitespaceChars

/// the character is not a newline character
/// a carriage return character or a line feed character
/// http://en.wikipedia.org/wiki/Newline
let isNewLine c = c = '\r' || c = '\n'
let isNewLineFalse c = isNewLine c = false

// overriding spaces, newlines are important for comments
// http://www.quanttec.com/fparsec/#spaces
/// skip zero or more whitespace characters, never fails
//let spaces : State<unit> -> Reply<unit,State<unit>> =
let spaces : Parser<unit,unit> = skipManySatisfy isWhitespaceChar
let spaces1 = skipMany1Satisfy isWhitespaceChar

// perhaps add comments?
let isEndOfLineChar c = c = '\n' || c = '\r'
let pEndOfLine =
  skipManySatisfy isEndOfLineChar

let pWord = many1Chars (asciiLetter <|> digit <|> anyOf "_")

type ScalarType =
  | ProtoDouble
  | ProtoFloat
  | ProtoInt32
  | ProtoInt64
  | ProtoUInt32
  | ProtoUInt64
  | ProtoSInt32
  | ProtoSInt64
  | ProtoFixed32
  | ProtoFixed64
  | ProtoSFixed32
  | ProtoSFixed64
  | ProtoBool
  | ProtoString
  | ProtoBytes
  
let scalarTypeMap =
  [
    "double", ProtoDouble;
    "float", ProtoFloat;
    "int32", ProtoInt32;
    "int64", ProtoInt64;
    "uint32", ProtoUInt32;
    "uint64", ProtoUInt64;
    "sint32", ProtoSInt32;
    "sint64", ProtoSInt64;
    "fixed32", ProtoFixed32;
    "fixed64", ProtoFixed64;
    "sfixed32", ProtoSFixed32;
    "sfixed64", ProtoSFixed64;
    "bool", ProtoBool;
    "string", ProtoString;
  ]
  |> Map.ofList

/// http://code.google.com/apis/protocolbuffers/docs/proto.html#scalar
let toScalarType s = scalarTypeMap.[s]
let isScalarType s = scalarTypeMap.ContainsKey s

let pScalarType =
  pWord >>= fun w ->
    if isScalarType w then
      toScalarType w |> preturn
    else
      //Reply(Error, expectedError (sprintf "unknown scalar type: %s" w))
//      expected "scalar type"
      fun stream -> Reply(Error, expected "scalar type")

type FieldRule =
  | Required
  | Optional
  | Repeated

let fieldRuleMap =
  [
    "required", Required;
    "optional", Optional;
    "repeated", Repeated;
  ]
  |> Map.ofList

let toFieldRule s = fieldRuleMap.[s]
let isFieldRule s = fieldRuleMap.ContainsKey s

let pFieldRule =
  pWord >>= fun w ->
    if isFieldRule w then
      toFieldRule w |> preturn
    else
      fun stream -> Reply(Error, expected "field type")

let pMessageName = pstring "message" >>. spaces1 >>. pWord

/// a line with a field
let pField =
  parse {
    do! spaces;
    let! ft = pFieldRule
    do! spaces1
    let! st = pScalarType
    do! spaces1
    let! name = pWord
    do! spaces
    do! skipChar '='
    do! spaces
    let! order = pint32
    do! spaces
    do! skipChar ';'
    do! pEndOfLine
    return ft, st, name, order
  }

let pMessage =
  parse {
    let! mn = pMessageName
    do! spaces1
    do! skipChar '{'
    do! pEndOfLine
    let! f = many1 pField
    do! skipChar '}'
    return mn, f
  }