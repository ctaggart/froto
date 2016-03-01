(*
 * Types used for encoding and decoding raw fields, at the wire format level.
 *)
namespace Froto.Core.Encoding

open System

/// Protobuf field wire-type
type WireType =
    | Varint = 0
    | Fixed64 = 1
    | LengthDelimited = 2
    | StartGroup = 3
    | EndGroup = 4
    | Fixed32 = 5

/// Protobuf field number
type FieldNum = int32

/// Raw unencoded wire-format field
type RawField =
    | Varint of FieldNum * uint64
    | Fixed32 of FieldNum * uint32
    | Fixed64 of FieldNum * uint64
    | LengthDelimited of FieldNum * ArraySegment<byte>
//    | Group is deprecated & not supported
    with
        member x.FieldNum =
            match x with
            | Varint(n,_) -> n
            | Fixed32(n,_) -> n
            | Fixed64(n,_) -> n
            | LengthDelimited(n,_) -> n
        // Need a place to hang this, because it cannot go on FieldNum
        // (a type alias cannot have methods or properties).
        static member MaxFieldNum = (2<<<28) - 1
