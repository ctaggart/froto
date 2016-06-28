namespace ProtoTypes.Core

open System
open System.IO

open Printf

[<AutoOpen>]
module Prelude =

    /// Raises NotSupportedException with given string as message
    let notsupportedf fmt = ksprintf (NotSupportedException >> raise) fmt

    /// Concatenates two paths using System.IO.Path.Combine
    let (</>) path1 path2 = Path.Combine(path1, path2)

    /// Concatenates two namespaces/class names and separates them with "."
    let (+.+) scope1 scope2 = (scope1 + "." + scope2).Trim('.')

    let x<'T> : 'T = Unchecked.defaultof<'T>

    let notNull x = not <| isNull x

    let create<'T when 'T: (new: unit -> 'T)>() = new 'T()