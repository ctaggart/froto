#light
namespace Froto.ProtoParser
module Test

open System
open Xunit
open FParsec.Primitives
open FParsec.CharParser
open Froto.ProtoParser

let equal expected actual = Assert.Equal(expected, actual)
let notEqual expected actual = Assert.NotEqual(expected, actual)
let isTrue condition = Assert.True(condition)
let isFalse condition = Assert.False(condition)

let throwParserFailure pr =
  Assert.ThrowsDelegate(fun () ->
    match pr with
    | Success(_) -> () 
    | Failure(errorMsg,_) -> failwith errorMsg
  )

let assertParseSuccess (pr:ParserResult<_>) = Assert.DoesNotThrow(throwParserFailure pr)
let assertParseFailure (pr:ParserResult<_>) = Assert.Throws<FailureException>(throwParserFailure pr)

/// test a parser on a string
let parseString p s = runParserOnString p () String.Empty s
let canParse p s = assertParseSuccess(parseString p s)
let canNotParse p s = assertParseFailure(runParserOnString p () String.Empty s) |> ignore

let isParseResult p s expected =
  isTrue(
    match parseString p s with
    | Success actual -> actual = expected
    | Failure (_,_) -> false
  )

/// testing xUnit setup :)
[<Fact>]
let capitalizeTest() =
  equal (capitalize "a") "A"
  equal (capitalize "a") "A"
  notEqual (capitalize "a") "a"
  notEqual (capitalize "b") "A"

[<Fact>]
let canParseTest() =
  let pDuck = pstring "duck"
  canParse pDuck "duck"
  canNotParse pDuck "goose"

[<Fact>]
let isNewLineTest() =
  isTrue (isNewLine '\r')
  isTrue (isNewLine '\n')
  isTrue (isNewLineFalse 'c')

[<Fact>]
let spacesTest() =
  canParse spaces1 "   " // 3 spaces
  canParse spaces1 "      " // 3 tabs
  canNotParse spaces1 "f"
  canNotParse spaces1 "fail"
  canParse spaces "     spaces tab space tab before this"
  
[<Fact>]
let pMessageNameTest() =
  canParse pMessageName "message SearchRequest"
  canNotParse pMessageName "message"
  isParseResult pMessageName "message SearchRequest" "SearchRequest"
  
[<Fact>]
let pWordTest() =
  canParse pWord "word"
  canNotParse pWord " "
  isParseResult pWord "word " "word"
  
[<Fact>]
let pScalarType() =
  isParseResult pScalarType "double" ProtoDouble
  isParseResult pScalarType "float" ProtoFloat
  //canNotParse pScalarType "dbl"