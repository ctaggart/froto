[<AutoOpen>]
module Froto.TypeProvider.Runtime.Prelude

open System.IO

open Printf

/// Raises InvalidOperationException with given formatted string as message
let invalidOpf fmt = ksprintf invalidOp fmt

/// Concatenates two paths using System.IO.Path.Combine
let (</>) path1 path2 = Path.Combine(path1, path2)

/// Concatenates two namespaces/class names and separates them with "."
let (+.+) scope1 scope2 = (scope1 + "." + scope2).Trim('.')

let x<'T> : 'T = Unchecked.defaultof<'T>

/// Calls default constructor of type 'T. Useful when constructor can't be called directly,
/// e.g. when creating an expression that creates an instance of generic type where generic argument
/// is a generated type (e.g. ResizeArray<SomeProtoMessage>)
let create<'T when 'T: (new: unit -> 'T)>() = new 'T()