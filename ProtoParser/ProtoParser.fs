
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

let pFieldOption =
    pWord .>> spaces .>> pchar '=' .>> spaces .>>. pWord
    |>> fun (name, value) -> ProtoFieldOption(name, value)

let pFieldOptions =
    pchar '[' >>. spaces >>. sepBy pFieldOption (pchar ',' .>> spaces) .>> (pchar ']') .>> spaces

let pField =
    pFieldRule .>> spaces1 .>>. pWord .>> spaces1 .>>. pWord .>> spaces .>> pchar '=' .>> spaces .>>. pint32 .>> spaces .>>. (opt pFieldOptions) .>> pchar ';' .>> spaces
    |>> fun ((((rule, tp), name), position), options) -> ProtoField(rule, tp, name, position, options)

let pEnumItem =
    pWord .>> spaces .>> pchar '=' .>> spaces .>>. pint32 .>> spaces .>> pchar ';' .>> spaces
    |>> fun (name, value) -> ProtoEnumItem(name, value)

let pEnum =
    pstring "enum" >>. (spaces1 >>. pWord .>> spaces .>> pchar '{' .>> spaces .>>. many1 pEnumItem .>> (pchar '}') .>> spaces
    |>> fun (name, items) -> ProtoEnum(name, items))

let pMessageRec, pMessageRecRef = createParserForwardedToRef()

let pMessage : Parser<ProtoMessage,unit> =
    let pMessageBox = pMessageRec |>> (fun message -> ProtoMessagePart.Message, box message)
    let pEnumBox = pEnum |>> (fun enum -> ProtoMessagePart.Enum, box enum)
    let pFieldBox = pField |>> (fun field -> ProtoMessagePart.Field, box field)
    pstring "message" >>. (spaces1 >>. pWord .>> spaces .>> pchar '{'
    .>> spaces .>>. many1 (pMessageBox <|> pEnumBox <|> pFieldBox)
    .>> (pchar '}') .>> spaces
    |>> fun (name, parts) -> ProtoMessage(name, parts))

do pMessageRecRef := pMessage

let pPackage =
    pstring "package" >>. (spaces1 >>. pWord .>> spaces .>> pchar ';' .>> spaces)

let pProto =
    let pPackageSection = pPackage |>> (fun package -> ProtoSection.Package, box package)
    let pMessageSection = pMessage |>> (fun message -> ProtoSection.Message, box message)
    spaces >>. many (pPackageSection <|> pMessageSection)
    |>> fun sections -> ProtoFile(sections)

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