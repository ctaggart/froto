[<RequireQualifiedAccess>]
module internal Froto.TypeProvider.Core.Naming

open System

let private mapFirstChar f (input: string) =
    if String.IsNullOrEmpty input then input
    else
        let first = input.[0] |> f |> string
        if input.Length > 1 then first + input.[1..]
        else first

/// Converts "name_like_that" to "NameLikeThat"
let snakeToPascal (identifier: string) =
    identifier.Split('_')
    |> Seq.map (mapFirstChar Char.ToUpper)
    |> String.concat String.Empty

/// Converts "NameLikeThat" to "nameLikeThat"
let pascalToCamel = mapFirstChar Char.ToLower

/// Converts "nameLikeThat" to "NameLikeThat"
let camelToPascal = mapFirstChar Char.ToUpper

/// Converts "name_like_that" to "nameLikeThat"
let snakeToCamel = snakeToPascal >> pascalToCamel

/// Converts "NAME_LIKE_THAT" to "nameLikeThat"
let upperSnakeToPascal (identifier: string) =
    snakeToPascal <| identifier.ToLower()
