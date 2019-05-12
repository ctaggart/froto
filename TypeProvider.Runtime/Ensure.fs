[<RequireQualifiedAccess>]
module Froto.TypeProvider.Runtime.Ensure

open System

let argNotNull name (value: obj) =
    if isNull value
    then raise <| ArgumentNullException(name)