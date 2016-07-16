[<RequireQualifiedAccess>]
module Froto.TypeProvider.Generation.Ensure

open System

let argNotNull name (value: obj) =
    if isNull value
    then raise <| ArgumentNullException(name)