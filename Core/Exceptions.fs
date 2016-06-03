namespace Froto.Serialization

/// Exception encoding or decoding a Protobuf wireformat

type ProtobufException(message:string, ?innerException:exn) =
    inherit System.ApplicationException( message, defaultArg innerException null )

type ProtobufWireFormatException (message:string, ?innerException:exn) =
    inherit ProtobufException(message, defaultArg innerException null)

type ProtobufSerializerException (message:string, ?innerException:exn) =
    inherit ProtobufException(message, defaultArg innerException null)
