namespace Froto.Serialization

/// General Exception while processing a protobuf message
type ProtobufException(message:string, ?innerException:exn) =
    inherit System.ApplicationException( message, defaultArg innerException null )

/// Exception at the WireFormat level while packing or unpacking a protobuf message
type ProtobufWireFormatException (message:string, ?innerException:exn) =
    inherit ProtobufException(message, defaultArg innerException null)

/// Exception while encoding or decoding a protobuf field, or when serializing
/// or deserializing a message.
type ProtobufSerializerException (message:string, ?innerException:exn) =
    inherit ProtobufException(message, defaultArg innerException null)
