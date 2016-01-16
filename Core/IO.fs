module Froto.IO

open System
open System.IO

let zigZag32 (n:int32) = (n <<< 1) ^^^ (n >>> 31)
let zigZag64 (n:int64) = (n <<< 1) ^^^ (n >>> 63)     

type WireType =
    | Varint = 0
    | Fixed64 = 1
    | LengthDelimited = 2
    | StartGroup = 3
    | EndGroup = 4
    | Fixed32 = 5

//type ProtoType =
//    | Double of double option
//    | Float of float option
//    | Int32 of int32 option
//    | Int64 of int64 option
//    | UInt32 of uint32 option
//    | SInt32 of int32 option
//    | SInt64 of int64 option
//    | Fixed32 of uint32 option
//    | Fixed64 of uint64 option
//    | SFixed32 of int32 option
//    | SFixed64 of int64 option
//    | Bool of bool option
//    | String of string option
//    | Bytes of byte[] option

let decodeVarintUInt32 (b:byte[]) : uint32 =
    match b.Length with
    | 0 -> 0u
    | 1 -> 
        uint32 b.[0]
    | 2 ->
        let b0 = uint32 (b.[0] &&& 0x7Fuy)
        let b1 = uint32 (b.[1])
        b0 ||| (b1 <<< 7)
    | 3 ->
        let b0 = uint32 (b.[0] &&& 0x7Fuy)
        let b1 = uint32 (b.[1] &&& 0x7Fuy)
        let b2 = uint32 (b.[2])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14)
    | 4 ->
        let b0 = uint32 (b.[0] &&& 0x7Fuy)
        let b1 = uint32 (b.[1] &&& 0x7Fuy)
        let b2 = uint32 (b.[2] &&& 0x7Fuy)
        let b3 = uint32 (b.[3])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14) ||| (b3 <<< 21)
    | _ -> // 5
        let b0 = uint32 (b.[0] &&& 0x7Fuy)
        let b1 = uint32 (b.[1] &&& 0x7Fuy)
        let b2 = uint32 (b.[2] &&& 0x7Fuy)
        let b3 = uint32 (b.[3] &&& 0x7Fuy)
        let b4 = uint32 (b.[4])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14) ||| (b3 <<< 21) ||| (b4 <<< 28)

let decodeVarintUInt64 (b:byte[]) : uint64 =
    match b.Length with
    | 0 -> 0UL
    | 1 | 2 | 3 | 4 -> uint64 (decodeVarintUInt32 b)
    | 5 ->
        let b0 = uint64 (b.[0] &&& 0x7Fuy)
        let b1 = uint64 (b.[1] &&& 0x7Fuy)
        let b2 = uint64 (b.[2] &&& 0x7Fuy)
        let b3 = uint64 (b.[3] &&& 0x7Fuy)
        let b4 = uint64 (b.[4])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14) ||| (b3 <<< 21) ||| (b4 <<< 28)
    | 6 ->
        let b0 = uint64 (b.[0] &&& 0x7Fuy)
        let b1 = uint64 (b.[1] &&& 0x7Fuy)
        let b2 = uint64 (b.[2] &&& 0x7Fuy)
        let b3 = uint64 (b.[3] &&& 0x7Fuy)
        let b4 = uint64 (b.[4] &&& 0x7Fuy)
        let b5 = uint64 (b.[5])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14) ||| (b3 <<< 21) ||| (b4 <<< 28) ||| (b5 <<< 35)
    | 7 ->
        let b0 = uint64 (b.[0] &&& 0x7Fuy)
        let b1 = uint64 (b.[1] &&& 0x7Fuy)
        let b2 = uint64 (b.[2] &&& 0x7Fuy)
        let b3 = uint64 (b.[3] &&& 0x7Fuy)
        let b4 = uint64 (b.[4] &&& 0x7Fuy)
        let b5 = uint64 (b.[5] &&& 0x7Fuy)
        let b6 = uint64 (b.[6])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14) ||| (b3 <<< 21) ||| (b4 <<< 28) ||| (b5 <<< 35) ||| (b6 <<< 42) 
    | 8 ->
        let b0 = uint64 (b.[0] &&& 0x7Fuy)
        let b1 = uint64 (b.[1] &&& 0x7Fuy)
        let b2 = uint64 (b.[2] &&& 0x7Fuy)
        let b3 = uint64 (b.[3] &&& 0x7Fuy)
        let b4 = uint64 (b.[4] &&& 0x7Fuy)
        let b5 = uint64 (b.[5] &&& 0x7Fuy)
        let b6 = uint64 (b.[6] &&& 0x7Fuy)
        let b7 = uint64 (b.[7])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14) ||| (b3 <<< 21) ||| (b4 <<< 28) ||| (b5 <<< 35) ||| (b6 <<< 42) ||| (b7 <<< 49)
    | 9 ->
        let b0 = uint64 (b.[0] &&& 0x7Fuy)
        let b1 = uint64 (b.[1] &&& 0x7Fuy)
        let b2 = uint64 (b.[2] &&& 0x7Fuy)
        let b3 = uint64 (b.[3] &&& 0x7Fuy)
        let b4 = uint64 (b.[4] &&& 0x7Fuy)
        let b5 = uint64 (b.[5] &&& 0x7Fuy)
        let b6 = uint64 (b.[6] &&& 0x7Fuy)
        let b7 = uint64 (b.[7] &&& 0x7Fuy)
        let b8 = uint64 (b.[8])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14) ||| (b3 <<< 21) ||| (b4 <<< 28) ||| (b5 <<< 35) ||| (b6 <<< 42) ||| (b7 <<< 49) ||| (b8 <<< 56)
    | _ -> // 10
        let b0 = uint64 (b.[0] &&& 0x7Fuy)
        let b1 = uint64 (b.[1] &&& 0x7Fuy)
        let b2 = uint64 (b.[2] &&& 0x7Fuy)
        let b3 = uint64 (b.[3] &&& 0x7Fuy)
        let b4 = uint64 (b.[4] &&& 0x7Fuy)
        let b5 = uint64 (b.[5] &&& 0x7Fuy)
        let b6 = uint64 (b.[6] &&& 0x7Fuy)
        let b7 = uint64 (b.[7] &&& 0x7Fuy)
        let b8 = uint64 (b.[8] &&& 0x7Fuy)
        let b9 = uint64 (b.[9])
        b0 ||| (b1 <<< 7) ||| (b2 <<< 14) ||| (b3 <<< 21) ||| (b4 <<< 28) ||| (b5 <<< 35) ||| (b6 <<< 42) ||| (b7 <<< 49) ||| (b8 <<< 56) ||| (b8 <<< 63)

let encodeVarintUInt32 (i:uint32) : byte[] =
    if i < (2u <<< 6) then // fits in 7 bits
        let b0 = byte (i)
        [| b0 |]
    else if i < (2u <<< 13) then    // fits in 14 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7)
        [| b0; b1 |]
    else if i < (2u <<< 20) then    // fits in 21 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14)
        [| b0; b1; b2 |]
    else if i < (2u <<< 27) then    // fits in 28 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14) ||| 0x80uy
        let b3 = byte (i >>> 21)
        [| b0; b1; b2; b3 |]
    else // fits in 35 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14) ||| 0x80uy
        let b3 = byte (i >>> 21) ||| 0x80uy
        let b4 = byte (i >>> 28)
        [| b0; b1; b2; b3; b4 |]

let encodeVarintUInt64 (i:uint64) : byte[] =
    if i < (2UL <<< 31) then // fits in 32 bits
        encodeVarintUInt32 (uint32 i)
    else if i < (2UL <<< 34) then    // fits in 35 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14) ||| 0x80uy
        let b3 = byte (i >>> 21) ||| 0x80uy
        let b4 = byte (i >>> 28)
        [| b0; b1; b2; b3; b4 |]
    else if i < (2UL <<< 41) then    // fits in 42 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14) ||| 0x80uy
        let b3 = byte (i >>> 21) ||| 0x80uy
        let b4 = byte (i >>> 28) ||| 0x80uy
        let b5 = byte (i >>> 35)
        [| b0; b1; b2; b3; b4; b5 |]
    else if i < (2UL <<< 48) then    // fits in 49 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14) ||| 0x80uy
        let b3 = byte (i >>> 21) ||| 0x80uy
        let b4 = byte (i >>> 28) ||| 0x80uy
        let b5 = byte (i >>> 35) ||| 0x80uy
        let b6 = byte (i >>> 42)
        [| b0; b1; b2; b3; b4; b5; b6 |]
    else if i < (2UL <<< 55) then    // fits in 56 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14) ||| 0x80uy
        let b3 = byte (i >>> 21) ||| 0x80uy
        let b4 = byte (i >>> 28) ||| 0x80uy
        let b5 = byte (i >>> 35) ||| 0x80uy
        let b6 = byte (i >>> 42) ||| 0x80uy
        let b7 = byte (i >>> 49)
        [| b0; b1; b2; b3; b4; b5; b6; b7 |]
    else if i < (2UL <<< 62) then    // fits in 63 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14) ||| 0x80uy
        let b3 = byte (i >>> 21) ||| 0x80uy
        let b4 = byte (i >>> 28) ||| 0x80uy
        let b5 = byte (i >>> 35) ||| 0x80uy
        let b6 = byte (i >>> 42) ||| 0x80uy
        let b7 = byte (i >>> 49) ||| 0x80uy
        let b8 = byte (i >>> 56)
        [| b0; b1; b2; b3; b4; b5; b6; b7; b8 |]
    else // fits in 70 bits
        let b0 = byte (i) ||| 0x80uy
        let b1 = byte (i >>> 7) ||| 0x80uy
        let b2 = byte (i >>> 14) ||| 0x80uy
        let b3 = byte (i >>> 21) ||| 0x80uy
        let b4 = byte (i >>> 28) ||| 0x80uy
        let b5 = byte (i >>> 35) ||| 0x80uy
        let b6 = byte (i >>> 42) ||| 0x80uy
        let b7 = byte (i >>> 49) ||| 0x80uy
        let b8 = byte (i >>> 56) ||| 0x80uy
        let b9 = byte (i >>> 63)
        [| b0; b1; b2; b3; b4; b5; b6; b7; b8; b9 |]

let encodeVarintInt32 (i:int32) = encodeVarintUInt32 (uint32 i)
let encodeVarintInt64 (i:int64) = encodeVarintUInt64 (uint64 i)
let encodeVarintSInt32 (i:int32) = encodeVarintUInt32 (uint32 (zigZag32 i))
let encodeVarintSInt64 (i:int64) = encodeVarintUInt64 (uint64 (zigZag64 i))
let encodeVarintBool (b:bool) = encodeVarintUInt32 (if b then 1u else 0u)

let encodeUInt32 (i:uint32) =
    let b0 = byte i
    let b1 = byte (i >>> 8)
    let b2 = byte (i >>> 16)
    let b3 = byte (i >>> 24)
    [| b0; b1; b2; b3 |]
    
let encodeInt32 (i:int32) = encodeUInt32 (uint32 i)

let decodeUInt32 (b:byte[]) =
    let b0 = uint32 b.[0]
    let b1 = uint32 b.[1]
    let b2 = uint32 b.[2]
    let b3 = uint32 b.[3]
    b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)

let decodeInt32 b = int32 (decodeUInt32 b)

//type Varint =
//    | Int32 of int32
//    | Int64 of int64
//    | UInt32 of uint32
//    | UInt64 of uint64
//    | SInt32 of int32
//    | SInt64 of int64
//    | Bool of bool
//
//let encodeVarint (i:Varint) : byte[] =
//    [||]

// a tag is a field number and a wire type
/// gets the field number from a tag
let parseTag (b:byte[]) =
    let i = decodeVarintUInt32 b
    let fieldNumber = i >>> 3
    let wireType = i &&& 0x7u
    fieldNumber, wireType

let writeBytes (s:Stream) (b:byte[]) =
    s.Write(b, 0, b.Length)

let writeTag s (fieldNumber:uint32) (wireType:WireType) =
    let tag = (fieldNumber <<< 3) ||| (uint32 wireType)
    writeBytes s (encodeVarintUInt32 tag)

let writeFieldString s n (v:string) =
    writeTag s n WireType.LengthDelimited
    let b = Text.Encoding.UTF8.GetBytes v
    writeBytes s (encodeVarintUInt32 (uint32 b.Length))
    writeBytes s b

//let readFieldString (s:Stream)

let writeFieldInt32 s n (v:int32) =
    writeTag s n WireType.Fixed32
    writeBytes s (encodeInt32 v)

let writeFieldUInt32 s n (v:uint32) =
    writeTag s n WireType.Fixed32
    writeBytes s (encodeUInt32 v)

let readFieldUInt32 (s:Stream) =
    let mutable b = Array.zeroCreate<byte> 4
    s.Read(b, 0, 4) |> ignore
    decodeUInt32 b

/// Make the stream a sequence of bytes.
let streamAsBytes (s:Stream) = 
    seq {
        let i = ref 0
        while (i := s.ReadByte(); !i <> -1) do
            yield byte !i
    }

// Is the most significant bit set?
let msb (b:byte) = Convert.ToBoolean (b &&& 0x80uy)

let readVarintBytes (s:Stream) =
    let bytes = seq { 
        use e = (streamAsBytes s).GetEnumerator()
        let b = ref 0uy
        while e.MoveNext() && (b := e.Current; msb !b) do 
            yield !b
        yield !b
    }
    Seq.toArray bytes
