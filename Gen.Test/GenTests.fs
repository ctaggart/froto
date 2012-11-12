
module Froto.Gen.GenTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open System.Reflection
open System.Diagnostics

/// gets the path for a test file based on the relative path from the executing assembly
let getTestFile file =
     let codeBase = Reflection.Assembly.GetExecutingAssembly().CodeBase
     let assemblyPath = DirectoryInfo (Uri codeBase).LocalPath
     let solutionPath = (assemblyPath.Parent.Parent.Parent.Parent).FullName
     Path.Combine(solutionPath, Path.Combine("test",file))

//[<Fact>]
//let ``createCompilation`` () =

type Person = Froto.Gen.ProtoGen< @"C:\Users\taggartc\froto\froto\test\addressbook1.proto">
