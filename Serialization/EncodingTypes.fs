(*
 * Types used for encoding and decoding raw fields, at the wire format level.
 *)
namespace Froto.Serialization.Encoding

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

        member x.WireType =
            match x with
            | Varint _ -> WireType.Varint
            | Fixed32 _ -> WireType.Fixed32
            | Fixed64 _ -> WireType.Fixed64
            | LengthDelimited _ -> WireType.LengthDelimited

        static member raiseMismatch expected actual =
            let extractNumAndType = function
                | RawField.Varint (n,_)  -> n, "Varint"
                | RawField.Fixed32 (n,_) -> n, "Fixed32"
                | RawField.Fixed64 (n,_) -> n, "Fixed64"
                | RawField.LengthDelimited (n,_) -> n, "LengthDelimited"
            let (n, found) = actual |> extractNumAndType
            let s = sprintf "Encoding failure: wiretype mismatch for field %d: expected %s, found %s" n expected found
            raise <| Froto.Serialization.EncoderException(s)

