namespace Froto.Serialization

/// General Exception while processing a protobuf message
type SerializationException(message:string, ?innerException:exn) =
    inherit System.ApplicationException( message, defaultArg innerException null )

/// Exception at the WireFormat level while packing or unpacking a protobuf message
type WireFormatException (message:string, ?innerException:exn) =
    inherit SerializationException(message, defaultArg innerException null)

/// Exception while encoding or decoding a protobuf field.
type EncoderException (message:string, ?innerException:exn) =
    inherit SerializationException(message, defaultArg innerException null)

/// Exception while serializing or deserializing a message.
type SerializerException (message:string, ?innerException:exn) =
    inherit SerializationException(message, defaultArg innerException null)
