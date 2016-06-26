namespace ProtoTypes.Core

open System
open System.Collections.Generic

open Froto.Core
open Froto.Core.Encoding

// scalar type aliases based on https://developers.google.com/protocol-buffers/docs/proto3#scalar
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
type proto_map<'Key, 'Value> = IReadOnlyDictionary<'Key, 'Value>
type proto_concrete_map<'Key, 'Value> = Dictionary<'Key, 'Value>

type Writer<'T> = FieldNum -> ZeroCopyBuffer -> 'T -> unit
type Reader<'T> = RawField -> 'T