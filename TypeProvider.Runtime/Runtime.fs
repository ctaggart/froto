namespace Froto.TypeProvider.Runtime

open Froto.Serialization

// Eventually, this class might be replaced by Froto.Core.Encoding.MessageBase, but so far
// this interface looks simpler and satisfies all needs.

/// Base class for types generated from proto messages.
[<AbstractClass>]
type Message() =

    member this.SerializedLength =
        let buffer = NullWriteBuffer()
        this.Serialize buffer
        buffer.Length

    abstract Serialize: ZeroCopyBuffer -> unit
    
    abstract ReadFrom: ZeroCopyBuffer -> unit

[<assembly:CompilerServices.TypeProviderAssembly("Froto.TypeProvider.DesignTime.dll")>]
do ()
