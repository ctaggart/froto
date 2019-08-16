namespace Froto.TypeProvider.Runtime.Types

open System
open System.Collections.Generic

open Froto.Serialization
open Froto.Serialization.Encoding
open Froto.TypeProvider.Runtime

// type aliases based on https://developers.google.com/protocol-buffers/docs/proto3#scalar
type proto_double = float
type proto_float = float32
type proto_int32 = int
type proto_int64 = int64
type proto_uint32 = uint32
type proto_uint64 = uint64
type proto_sint32 = int
type proto_sint64 = int64
type proto_fixed32 = uint32
type proto_fixed64 = uint64
type proto_sfixed32 = int
type proto_sfixed64 = int64
type proto_bool = bool
type proto_string = string
type proto_bytes = ArraySegment<byte>
type proto_repeated<'T> = ResizeArray<'T>
type proto_map<'Key, 'Value> = Dictionary<'Key, 'Value>

type Writer<'T> = FieldNum -> ZeroCopyBuffer -> 'T -> unit
type Reader<'T> = RawField -> 'T

/// Base class for types generated from proto messages.
[<AbstractClass>]
type Message() =

    member this.SerializedLength =
        let buffer = NullWriteBuffer()
        this.Serialize buffer
        buffer.Length

    abstract Serialize: ZeroCopyBuffer -> unit
    
    abstract ReadFrom: ZeroCopyBuffer -> unit
    
/// Simple type used to simplify maps serialization and deserialization
type MapItem<'Key, 'Value>
    ( keyReader: Reader<'Key>,
      valueReader: Reader<'Value>,
      keyWriter: Writer<'Key>,
      valueWriter: Writer<'Value> ) =
    inherit Message()
    
    member val Key = Unchecked.defaultof<'Key> with get, set
    member val Value = Unchecked.defaultof<'Value> with get, set
    
    override this.Serialize(buffer) =
        keyWriter 1 buffer this.Key
        valueWriter 2 buffer this.Value
        
    override this.ReadFrom(buffer) =
        for field in ZeroCopyBuffer.allFields buffer do
            if field.FieldNum = 1 then
                this.Key <- keyReader field
            elif field.FieldNum = 2 then
                this.Value <- valueReader field
