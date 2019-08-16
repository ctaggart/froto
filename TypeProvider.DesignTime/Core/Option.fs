[<RequireQualifiedAccess>]
module Froto.TypeProvider.Core.Option

/// Invokes function if provided option is None, otherwise returns original value of the provided option
let otherwise f opt =
    match opt with
    | None -> f()
    | x -> x 
    
/// If provided option is Some - it's value is returned, otherwise an exception with provided error message is thrown
let require msg = function
    | Some(x) -> x
    | None -> failwith msg
    
/// If given option has some value, this value is returned, otherwise 'alternative' is used
let getOrElse alternative = function
    | Some(x) -> x
    | None -> alternative
    
/// Unwraps nested option to plain option<'T>
let unwrap = function
    | Some(Some(x)) -> Some x
    | _ -> None
