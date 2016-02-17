namespace Froto.Core.Encoding

open System

type FieldNum = int32

type RawField =
    | Varint of FieldNum * uint64
    | Fixed32 of FieldNum * uint32
    | Fixed64 of FieldNum * uint64
    | LengthDelimited of FieldNum * ArraySegment<byte>
//    | Group of FieldNum * WireType (deprecated)
    with
        member x.FieldNum =
            match x with
            | Varint(n,_) -> n
            | Fixed32(n,_) -> n
            | Fixed64(n,_) -> n
            | LengthDelimited(n,_) -> n
        // Need a place to hang this, because it cannot go on Tag:
        // a type alias cannot have methods or properties.
        static member MaxTag = (2<<<28) - 1
