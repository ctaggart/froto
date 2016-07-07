[<AutoOpen>]
module Froto.TypeProvider.Core.Prelude

open System
open System.IO

open Printf

/// Raises NotSupportedException with given string as message
let notsupportedf fmt = ksprintf (NotSupportedException >> raise) fmt

/// Concatenates two paths using System.IO.Path.Combine
let (</>) path1 path2 = Path.Combine(path1, path2)

/// Concatenates two namespaces/class names and separates them with "."
let (+.+) scope1 scope2 = (scope1 + "." + scope2).Trim('.')

let x<'T> : 'T = Unchecked.defaultof<'T>

/// Calls default constructor of type 'T. Useful when constructor can't be called directly,
/// e.g. when creating an expression that creates an instance of generic type where generic argument
/// is a generated type (e.g. ResizeArray<SomeProtoMessage>)
let create<'T when 'T: (new: unit -> 'T)>() = new 'T()