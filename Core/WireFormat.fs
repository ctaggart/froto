module Froto.Core.WireFormat

open System
open System.IO

let MAX_FIELD_LEN = 64u * 1024u * 1024u

let zigZag32 (n:int32) = (n <<< 1) ^^^ (n >>> 31)
let zigZag64 (n:int64) = (n <<< 1) ^^^ (n >>> 63)
let zagZig32 (n:int32) = int32(uint32 n >>> 1) ^^^ (if n&&&1  = 0  then 0  else -1 )
let zagZig64 (n:int64) = int64(uint64 n >>> 1) ^^^ (if n&&&1L = 0L then 0L else -1L)


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


let encodeVarint (i:uint64) (dest:ZeroCopyWriteBuffer) =

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

    loop i dest
    dest

let encodeFixed32 (i:uint32) (dest:ZeroCopyWriteBuffer) =
    dest.WriteByte (byte i)
    dest.WriteByte (byte (i >>> 8))
    dest.WriteByte (byte (i >>> 16))
    dest.WriteByte (byte (i >>> 24))
    dest

let encodeFixed64 (i:uint64) dest =
    dest
    |> encodeFixed32 (uint32 i)
    |> encodeFixed32 (uint32 (i >>> 32))
    
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

type WireField =
    | Varint of uint64
    | Fixed32 of uint32
    | Fixed64 of uint64
    | LengthDelimited of ArraySegment<byte>
//    | Group of WireType (deprecated)

// a tag is a field number and a wire type
/// gets the field number from a tag
let readTag src =
    let i = decodeVarint src
    let fieldNumber = i >>> 3
    let wireType = enum<WireType> (int32 i &&& 0x07)
    (fieldNumber, wireType)

let readField src =
    let fieldNum, wireType = readTag src
    let field = 
        match wireType with
        | WireType.Varint -> Varint (decodeVarint src)
        | WireType.Fixed64 -> Fixed64 (decodeFixed64 src)
        | WireType.LengthDelimited -> LengthDelimited (decodeLengthDelimited src)
        | WireType.Fixed32 -> Fixed32 (decodeFixed32 src)

        | WireType.StartGroup
        | WireType.EndGroup
        | _ -> raise <| ProtobufWireFormatException(sprintf "Decode failure: unsupported wiretype: %A" wireType)

    (fieldNum, field)

let writeTag (fieldNumber:uint32) (wireType:WireType) =
    let tag = (fieldNumber <<< 3) ||| (uint32 wireType)
    encodeVarint (uint64 tag)

let writeFieldVarint (fieldNumber:uint32) (i:uint64) =
    writeTag fieldNumber WireType.Varint
    >> encodeVarint i

let writeFieldFixed64 (fieldNumber:uint32) (i:uint64) =
    writeTag fieldNumber WireType.Fixed64
    >> encodeFixed64 i

let writeFieldLengthDelimited (fieldNumber:uint32) len (emplace:ArraySegment<byte>->unit) =
    writeTag fieldNumber WireType.LengthDelimited
    >> encodeLengthDelimited len emplace

let writeFieldBytes (fieldNum:uint32) (source:ArraySegment<byte>) =
    writeFieldLengthDelimited fieldNum
        (uint32 source.Count)
        (fun dest -> Array.Copy( source.Array, source.Offset, dest.Array, dest.Offset, source.Count))

let writeFieldString fieldNum (s:string) =
    let utf8 = System.Text.Encoding.UTF8
    let len = utf8.GetByteCount(s) |> uint32
    writeFieldLengthDelimited fieldNum
        len
        (fun dest -> utf8.GetBytes( s, 0, s.Length, dest.Array, dest.Offset) |> ignore)

let writeFieldFixed32 (fieldNumber:uint32) (i:uint32) =
    writeTag fieldNumber WireType.Fixed32
    >> encodeFixed32 i

