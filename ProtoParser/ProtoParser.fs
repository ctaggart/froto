#light
namespace Froto
module ProtoParser

// http://code.google.com/apis/protocolbuffers/docs/proto.html
// http://www.quanttec.com/fparsec/

open FParsec.Primitives
open FParsec.CharParser

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
let spaces : State<unit> -> Reply<unit,State<unit>> =
  skipManySatisfy isWhitespaceChar
let spaces1 : State<unit> -> Reply<unit,State<unit>> =
  skipMany1Satisfy isWhitespaceChar

let pWord : State<unit> -> Reply<string,State<unit>> = 
  many1Chars (asciiLetter <|> digit <|> anyOf "_")

let pMessageName = pstring "message" >>. spaces1 >>. many1Chars (asciiLetter <|> digit)

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
  |> Map.of_list

/// http://code.google.com/apis/protocolbuffers/docs/proto.html#scalar
let toScalarType s = scalarTypeMap.[s]
let isScalarType s = scalarTypeMap.ContainsKey s

//let pScalarType =
//  pWord |>> fun w ->
//    if isScalarType w then
//      toScalarType w
//    else 
//      failwithf "unknown scalar type: %s" w

let pScalarType =
  parse {
    let! w = pWord
//    if isScalarType w then
//      return toScalarType(w)
//    else
//      //fail ("unknown scalar type: %s" w)
    return toScalarType(w)
  }
  