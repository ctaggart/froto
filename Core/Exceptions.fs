namespace Froto.Core

/// Exception encoding or decoding a Protobuf wireformat

type ProtobufWireFormatException (message:string, ?innerException:exn) =
    inherit System.ApplicationException( message, defaultArg innerException null )

