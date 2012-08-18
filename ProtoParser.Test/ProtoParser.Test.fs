
module Froto.ProtoParser.Test

open System
open System.IO
open Xunit
open FParsec.Primitives
open FParsec.CharParsers
open Froto.ProtoParser

let equal expected actual = Assert.Equal(expected, actual)
let notEqual expected actual = Assert.NotEqual(expected, actual)
let isTrue condition = Assert.True(condition)
let isFalse condition = Assert.False(condition)

let throwParserFailure pr =
  Assert.ThrowsDelegate(fun () ->
    match pr with
    | Success(_) -> () 
    | Failure(errorMsg, _, _) -> failwith errorMsg
  )

let assertParseSuccess (pr:ParserResult<_,_>) = Assert.DoesNotThrow(throwParserFailure pr)
let assertParseFailure (pr:ParserResult<_,_>) = Assert.Throws<Exception>(throwParserFailure pr)

/// test a parser on a string
let parseString p s = runParserOnString p () String.Empty s
let canParse p s = assertParseSuccess(parseString p s)
let canNotParse p s = assertParseFailure(runParserOnString p () String.Empty s) |> ignore

/// gets the path for a test file
/// looking for froto\test\file when
/// current assembly is froto\Project\bin\Configuration\Project.dll
let getTestFile file =
   let codeBase = Reflection.Assembly.GetExecutingAssembly().CodeBase
   let assemblyPath = DirectoryInfo (Uri codeBase).LocalPath
   let solutionPath = (assemblyPath.Parent.Parent.Parent.Parent).FullName
   Path.Combine(solutionPath, Path.Combine("test",file))

let parseFile p f = runParserOnFile p () (getTestFile f) Text.Encoding.UTF8 
let canParseFile p f = assertParseSuccess(parseFile p f)
let canNotParseFile p f = assertParseFailure(parseFile p f)

let isParseResult p s expected =
  isTrue(
    match parseString p s with
    | Success (actual, _, _) -> actual = expected
    | Failure (_, _, _) -> false
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
let pScalarTypeTest() =
  isParseResult pScalarType "double" ProtoDouble
  isParseResult pScalarType "float" ProtoFloat
  canNotParse pScalarType "dbl"

[<Fact>]
let pFieldRuleTest() =
  isParseResult pFieldRule "required" Required
  isParseResult pFieldRule "optional" Optional
  isParseResult pFieldRule "repeated" Repeated
  canNotParse pFieldRule "rep"

[<Fact>]
let pFieldTest() =
  canParse pField "required string query = 1;"
  canParse pField "required   string   query=34245   ;"

[<Fact>]  
let findTestFileTest() =
  isTrue (File.Exists (getTestFile "SearchRequest.proto"))

[<Fact>]
let pMessageTest() =
  canParseFile pMessage "SearchRequest.proto"