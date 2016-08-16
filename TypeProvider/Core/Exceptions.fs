namespace Froto.TypeProvider.Core

open System

/// Base type for exceptions thrown by Froto.TypeProvider
type ProtobufTypeProviderException(message: string) =
    inherit Exception(message)

/// Indicates that given protobuf type is not supported.
type TypeNotSupportedException(typeName: string) =
    inherit ProtobufTypeProviderException(sprintf "Type \"%s\" is not supported" typeName)

    member __.TypeName = typeName

/// Indicates that proto message can't be found.
type MessageDefinitionNotFoundException(messageName: string) =
    inherit ProtobufTypeProviderException(sprintf "Definition of message \"%s\" can't be found." messageName)

    member __.MessageName = messageName