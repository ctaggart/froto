module Froto.TypeProvider.Generation.Checks

open System

type RequiredPropertyNotSpecified(typeName: string, propertyName: string) =
    inherit Exception(sprintf "Property %s.%s corresponding to required protobuf field was not specified" typeName propertyName)

    member __.TypeName = typeName
    member __.PropertyName = propertyName

type NullValueIsNotAllowedException(typeName: string, propertyName: string) =
    inherit Exception(sprintf "Null is not allowed value for property %s.%s" typeName propertyName)

let ensureRequiredPropertySpecified typeName propertyName (value: obj) =
    if isNull value 
    then raise <| RequiredPropertyNotSpecified(typeName, propertyName)

let ensureValueIsNotNull typeName propertyName (value: obj) =
    if isNull value
    then raise <| NullValueIsNotAllowedException(typeName, propertyName)