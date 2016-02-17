module Froto.Core.WireFormat

open System
open Froto.Core.Encoding

let MAX_FIELD_LEN = 64u * 1024u * 1024u

/// Decode a Varint
let decodeVarint (src:ZeroCopyReadBuffer) =

    // note: can't remember if using the outer `src` causes a new
    // lambda to be created on each call to capture `src`, so be
    // safe and pass it in as a parameter.
    let get (src:ZeroCopyReadBuffer) =
        let b = src.ReadByte()
        let bMore = (b &&& 0x80uy) <> 0uy
        let lower7 = uint64 (b &&& 0x7Fuy)
        (bMore,lower7)

    let rec loop acc n (src:ZeroCopyReadBuffer) =
        let maxLen = 9
        if n <= maxLen then
            let bMore,lower7 = get src
            let acc = (lower7 <<< (7 * n)) ||| acc
            if bMore then
                loop acc (n+1) src
            else
                acc 
        else
            raise <| ProtobufWireFormatException( "Unterminated VarInt" )

    loop 0UL 0 src

let rec internal decodeFixedLoop acc len n (src:ZeroCopyReadBuffer) =
    if n < len
    then
        let byt = src.ReadByte()
        let acc = (uint64 byt <<< (8*n)) ||| acc
        decodeFixedLoop acc len (n+1) src
    else
        acc
let decodeFixed32 src = uint32 (decodeFixedLoop 0UL 4 0 src)
let decodeFixed64 src = uint64 (decodeFixedLoop 0UL 8 0 src)

let decodeSingle src =
    let u = decodeFixed32 src
    // TODO: eliminate the Array allocation,
    //       perhaps using CIL (MSIL) to load float from a register
    let bytes = BitConverter.GetBytes(u)
    if not BitConverter.IsLittleEndian then Array.Reverse bytes
    BitConverter.ToSingle(bytes,0)

let decodeDouble src =
    let u = decodeFixed64 src
    // TODO: eliminate the Array allocation,
    //       perhaps using CIL (MSIL) to load float from a register
    let bytes = BitConverter.GetBytes(u)
    if not BitConverter.IsLittleEndian then Array.Reverse bytes
    BitConverter.ToDouble(bytes,0)

let decodeLengthDelimited src =
    let len = decodeVarint src
    if len < uint64 MAX_FIELD_LEN then
        src.ReadByteSegment(uint32 len)
    else
        raise <| ProtobufWireFormatException( "Maximum field length exceeded" )


let encodeVarint (u:uint64) (dest:ZeroCopyWriteBuffer) =

    let rec loop acc (dest:ZeroCopyWriteBuffer) =
        let bMore  = acc > 0x7FUL
        let lower7 = byte ( acc &&& 0x7FUL )
        let b = if not bMore
                then lower7
                else lower7 ||| 0x80uy
        dest.WriteByte b
        if bMore
        then loop (acc >>> 7) dest
        else ()

    loop u dest
    dest

let encodeFixed32 (u:uint32) (dest:ZeroCopyWriteBuffer) =
    dest.WriteByte (byte u)
    dest.WriteByte (byte (u >>> 8))
    dest.WriteByte (byte (u >>> 16))
    dest.WriteByte (byte (u >>> 24))
    dest

let encodeFixed64 (u:uint64) dest =
    dest
    |> encodeFixed32 (uint32 u)
    |> encodeFixed32 (uint32 (u >>> 32))
    
let encodeSingle (f:float32) =
    let bytes = BitConverter.GetBytes(f)
    if not BitConverter.IsLittleEndian then Array.Reverse bytes
    let u = BitConverter.ToUInt32(bytes,0)
    encodeFixed32 u

let encodeDouble (d:double) =
    let bytes = BitConverter.GetBytes(d)
    if not BitConverter.IsLittleEndian then Array.Reverse bytes
    let UL = BitConverter.ToUInt64(bytes,0)
    encodeFixed64 UL

let encodeLengthDelimited (len:uint32) (emplace:ArraySegment<byte>->unit) (dest:ZeroCopyWriteBuffer) =
    dest |> encodeVarint (uint64 len) |> ignore
    dest.WriteByteSegment len emplace
    dest

type WireType =
    | Varint = 0
    | Fixed64 = 1
    | LengthDelimited = 2
    | StartGroup = 3
    | EndGroup = 4
    | Fixed32 = 5

// a tag is a field number and a wire type
/// gets the field number from a tag
let decodeTag src =
    let u = decodeVarint src
    let fieldNum = u >>> 3
    // theoretic range is [0, UInt64.MaxValue>>>3],
    // but descriptor.proto defines it as an int32
    // in the range [1,2^28)
    if fieldNum > 0UL && fieldNum <= uint64 RawField.MaxTag then
        let fieldNum = int32 fieldNum
        let wireType = enum<WireType> (int32 u &&& 0x07)
        (fieldNum, wireType)
    else
        raise <| ProtobufWireFormatException("Decode failure: field number must be in range [1, 2^28)")

let decodeField src =
    let fieldNum, wireType = decodeTag src
    match wireType with
    | WireType.Varint ->
        Varint (fieldNum, decodeVarint src)
    | WireType.Fixed64 ->
        Fixed64 (fieldNum, decodeFixed64 src)
    | WireType.LengthDelimited ->
        LengthDelimited (fieldNum, decodeLengthDelimited src)
    | WireType.Fixed32 ->
        Fixed32 (fieldNum, decodeFixed32 src)

    | WireType.StartGroup
    | WireType.EndGroup
    | _ -> raise <| ProtobufWireFormatException(sprintf "Decode failure: unsupported wiretype: %A" wireType)

let encodeTag (fieldNum:int32) (wireType:WireType) =
    if fieldNum > 0 && fieldNum <= RawField.MaxTag then
        let tag = (fieldNum <<< 3) ||| (int32 wireType)
        encodeVarint (uint64 tag)
    else
        raise <| ProtobufWireFormatException("Encode failure: field numbeer must be in range [1, 2^28)")

let encodeFieldVarint (fieldNum:int32) (u:uint64) =
    encodeTag fieldNum WireType.Varint
    >> encodeVarint u

let encodeFieldFixed32 (fieldNum:int32) (u:uint32) =
    encodeTag fieldNum WireType.Fixed32
    >> encodeFixed32 u

let encodeFieldFixed64 (fieldNum:int32) (u:uint64) =
    encodeTag fieldNum WireType.Fixed64
    >> encodeFixed64 u

let encodeFieldSingle (fieldNum:int32) (f:float32) =
    encodeTag fieldNum WireType.Fixed32
    >> encodeSingle f

let encodeFieldDouble (fieldNum:int32) (d:double) =
    encodeTag fieldNum WireType.Fixed64
    >> encodeDouble d

let encodeFieldLengthDelimited (fieldNum:int32) len (emplace:ArraySegment<byte>->unit) =
    encodeTag fieldNum WireType.LengthDelimited
    >> encodeLengthDelimited len emplace

let encodeFieldBytes (fieldNum:int32) (source:ArraySegment<byte>) =
    encodeFieldLengthDelimited
        (int32 fieldNum)
        (uint32 source.Count)
        (fun dest -> Array.Copy( source.Array, source.Offset, dest.Array, dest.Offset, source.Count))

let encodeFieldString fieldNum (s:string) =
    let utf8 = System.Text.Encoding.UTF8
    let len = utf8.GetByteCount(s) |> uint32
    encodeFieldLengthDelimited fieldNum
        len
        (fun dest -> utf8.GetBytes( s, 0, s.Length, dest.Array, dest.Offset) |> ignore)

