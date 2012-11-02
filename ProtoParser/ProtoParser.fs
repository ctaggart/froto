
module Froto.ProtoParser

open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error
open ProtoAst

let pWord : Parser<string,unit> = many1Chars (asciiLetter <|> digit <|> anyOf "_.")
let pWordQuotes : Parser<string,unit> = pchar '"' >>. many1Chars (asciiLetter <|> digit <|> anyOf "_./! ") .>> pchar '"'
let pWordParens = pchar '(' >>. pWord .>> pchar ')'

let fieldRuleMap =
    [
    "required", Required;
    "optional", Optional;
    "repeated", Repeated;
    ]
    |> Map.ofList

let toFieldRule s = fieldRuleMap.[s]

let pFieldRule =
    (pstring "required" <|> pstring "optional" <|> pstring "repeated") |>> toFieldRule

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

let pOption  =
    let pCustom = pWordParens |>> fun s -> s, true
    let pNotCustom = pWord |>> fun s -> s, false
    pstring "option" >>. (spaces >>. (pCustom <|> pNotCustom) .>> spaces .>> pchar '=' .>> spaces .>>. (pWordQuotes <|> pWord) .>> spaces .>> pchar ';' .>> spaces
    |>> fun ((name, isCustom), value) -> ProtoOption(name, value, isCustom))

let pImport  =
    pstring "import" >>. (spaces >>. pWordQuotes .>> spaces .>> pchar ';' .>> spaces)

let pMessageRec, pMessageRecRef = createParserForwardedToRef()

let pMessage : Parser<ProtoMessage,unit> =
    let pMessageBox = pMessageRec |>> fun message -> ProtoMessagePart.Message, box message
    let pEnumBox = pEnum |>> fun enum -> ProtoMessagePart.Enum, box enum
    let pFieldBox = pField |>> fun field -> ProtoMessagePart.Field, box field
    let pOptionBox = pOption |>> fun option -> ProtoMessagePart.Option, box option
    (pstring "message" <|> pstring "extend") .>> spaces1 .>>. pWord .>> spaces .>> pchar '{'
    .>> spaces .>>. many1 (pMessageBox <|> pEnumBox <|> pFieldBox <|> pOptionBox)
    .>> (pchar '}') .>> spaces
    |>> fun ((tp, name), parts) -> ProtoMessage(name, parts, (tp = "extend"))

do pMessageRecRef := pMessage

let pPackage =
    pstring "package" >>. (spaces1 >>. pWord .>> spaces .>> pchar ';' .>> spaces)

let pProto =
    let pPackageBox = pPackage |>> fun package -> ProtoSection.Package, box package
    let pMessageBox = pMessage |>> fun message -> ProtoSection.Message, box message
    let pOptionBox = pOption |>> fun option -> ProtoSection.Option, box option
    let pImportBox = pImport |>> fun import -> ProtoSection.Import, box import
    spaces >>. many (pPackageBox <|> pOptionBox <|> pImportBox <|> pMessageBox)
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