
module Froto.ProtoParser

open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error
open ProtoAst

let pWord : Parser<string,unit> = many1Chars (asciiLetter <|> digit <|> anyOf "_.")
    
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

let pField =
    pFieldRule .>> spaces1 .>>. pWord .>> spaces1 .>>. pWord .>> spaces .>> pchar '=' .>> spaces .>>. pint32 .>> spaces .>> pchar ';' .>> spaces
    |>> (fun (((rule, tp), name), position) -> {Rule=rule; Type=tp; Name=name; Position=position})

let pEnumItem =
    pWord .>> spaces .>> pchar '=' .>> spaces .>>. pint32 .>> spaces .>> pchar ';' .>> spaces
    |>> (fun (name, value) -> {Name=name; Value=value})

let pEnum =
    pstring "enum" >>. (spaces1 >>. pWord .>> spaces .>> pchar '{' .>> spaces .>>. many1 pEnumItem .>> (pchar '}')
    |>> (fun (name, items) -> {Name=name; Items=items}))

let pMessage =
    let pFieldPart = pField |>> Field
    let pEnumPart = pEnum |>> Enum
    pstring "message" >>. (spaces1 >>. pWord .>> spaces .>> pchar '{' .>> spaces .>>. many1 (pFieldPart <|> pEnumPart) .>> (pchar '}') .>> spaces
    |>> (fun (name, parts) -> {Name=name; Parts=parts}))

let pPackage =
    pstring "package" >>. (spaces1 >>. pWord .>> spaces .>> pchar ';' .>> spaces)

let pProto =
    let pPackageSection = pPackage |>> Package
    let pMessageSection = pMessage |>> Message
    spaces >>. many (pPackageSection <|> pMessageSection)

let resultOrFail parserResult =
    match parserResult with
    | Success (result, _, _) -> result
    | Failure (errMsg, _, _) -> failwith errMsg

let parseString parser str =
    runParserOnString (parser .>> eof) () String.Empty str
    |> resultOrFail

let parseFile parser path =
    runParserOnFile (parser .>> eof) () path Text.Encoding.UTF8
    |> resultOrFail