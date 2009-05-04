#light
namespace Froto.ProtoParser
module Test

open Xunit
open Froto.ProtoParser

[<Fact>]    
let capitalize() =     
  Assert.Equal(capitalize "a", "A")
  Assert.NotEqual(capitalize "a", "a")
  Assert.NotEqual(capitalize "b", "A")