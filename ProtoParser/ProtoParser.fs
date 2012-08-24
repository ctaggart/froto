
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

/// allows the result of a successful parse to be mapped to another type
let mapResult f (parser:Parser<'Result,'UserState>) =
    fun stream ->
        let reply = parser stream
        if reply.Status = Ok then
            Reply(f reply.Result)
        else
            Reply(reply.Status, reply.Error)

let pField =
    pFieldRule .>> spaces1 .>>. pWord .>> spaces1 .>>. pWord .>> spaces .>> pchar '=' .>> spaces .>>. pint32 .>> spaces .>> pchar ';' .>> spaces
    |> mapResult (fun (((rule, tp), name), position) -> {Rule=rule; Type=tp; Name=name; Position=position})

let pEnumItem =
    pWord .>> spaces .>> pchar '=' .>> spaces .>>. pint32 .>> spaces .>> pchar ';' .>> spaces
    |> mapResult (fun (name, value) -> {Name=name; Value=value})

let pEnum =
    pstring "enum" >>. spaces1 >>. pWord .>> spaces .>> pchar '{' .>> spaces .>>. many1Till pEnumItem (pchar '}')
    |> mapResult (fun (name, items) -> {Name=name; Items=items})

let pMessage =
    let pFieldPart = pField |> mapResult Field
    let pEnumPart = pEnum |> mapResult Enum
    pstring "message" >>. spaces1 >>. pWord .>> spaces .>> pchar '{' .>> spaces .>>. many1Till (pFieldPart <|> pEnumPart) (pchar '}') .>> spaces
    |> mapResult (fun (name, parts) -> {Name=name; Parts=parts})

let pPackage =
    pstring "package" >>. spaces1 >>. pWord .>> spaces .>> pchar ';' .>> spaces

let pProto =
    let pPackageSection = pPackage |> mapResult Package
    let pMessageSection = pMessage |> mapResult Message
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