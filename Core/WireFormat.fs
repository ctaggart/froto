module Froto.Core.WireFormat

open System
open Froto.Core.Encoding

/// Maximum length of a length-delimited field (64Mb)
let MAX_FIELD_LEN = 64u * 1024u * 1024u


/// Decode a varint-encoded uint64 (1-9 bytes)
let decodeVarint (src:ZeroCopyBuffer) =

    // note: can't remember if using the outer `src` will cause a new
    // lambda to be created (on each call) to capture `src`, so be
    // safe and pass it in as a parameter.
    let getNext (src:ZeroCopyBuffer) =
        let b = src.ReadByte()
        let bMore = (b &&& 0x80uy) <> 0uy
        let lower7 = uint64 (b &&& 0x7Fuy)
        (bMore,lower7)

    let rec loop acc n (src:ZeroCopyBuffer) =
        let maxLen = 9
        if n <= maxLen then
            let bMore,lower7 = getNext src
            let acc = (lower7 <<< (7 * n)) ||| acc
            if bMore then
                loop acc (n+1) src
            else
                acc 
        else
            raise <| ProtobufWireFormatException( "Unterminated VarInt" )

    loop 0UL 0 src

let rec internal decodeFixedLoop acc len n (src:ZeroCopyBuffer) =
    if n < len
    then
        let byt = src.ReadByte()
        let acc = (uint64 byt <<< (8*n)) ||| acc
        decodeFixedLoop acc len (n+1) src
    else
        acc

/// Decode fixed-size uint32 (8-bytes)
let decodeFixed32 src = uint32 (decodeFixedLoop 0UL 4 0 src)

/// Decode fized-size uint64 (8-bytes)
let decodeFixed64 src = uint64 (decodeFixedLoop 0UL 8 0 src)

/// Decode fixed-size float (4-bytes)
let decodeSingle src =
    let u = decodeFixed32 src
    // TODO: eliminate the Array allocation,
    //       perhaps using CIL (MSIL) to load float from a register
    let bytes = BitConverter.GetBytes(u)
    if not BitConverter.IsLittleEndian then Array.Reverse bytes
    BitConverter.ToSingle(bytes,0)

/// Decode fized-size double (8-bytes)
let decodeDouble src =
    let u = decodeFixed64 src
    // TODO: eliminate the Array allocation,
    //       perhaps using CIL (MSIL) to load float from a register
    let bytes = BitConverter.GetBytes(u)
    if not BitConverter.IsLittleEndian then Array.Reverse bytes
    BitConverter.ToDouble(bytes,0)

/// Decode length delimited field [0,MAX_FIELD_LEN) bytes.
///
/// NOTE: Returns an ArraySegement which references a section of the
/// underlying source Array, rather than a copy.  Changes to that source
/// Array will therefore impact this function's returned value.
let decodeLengthDelimited src =
    let len = decodeVarint src
    if len < uint64 MAX_FIELD_LEN then
        src.ReadByteSegment(uint32 len)
    else
        raise <| ProtobufWireFormatException( "Maximum field length exceeded" )

/// Encode uint64 as a varint (1-9 bytes)
let encodeVarint (u:uint64) (dest:ZeroCopyBuffer) =

    let rec loop acc (dest:ZeroCopyBuffer) =
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

/// Encode uint32 as fixed32 (4 bytes)
let encodeFixed32 (u:uint32) (dest:ZeroCopyBuffer) =
    dest.WriteByte (byte u)
    dest.WriteByte (byte (u >>> 8))
    dest.WriteByte (byte (u >>> 16))
    dest.WriteByte (byte (u >>> 24))
    dest

/// Encode uint64 as fixed64 (8 bytes)
let encodeFixed64 (u:uint64) dest =
    dest
    |> encodeFixed32 (uint32 u)
    |> encodeFixed32 (uint32 (u >>> 32))
   
/// Encode float as fixed32 (4 bytes)
let encodeSingle (f:single) =
    let bytes = BitConverter.GetBytes(f)
    if not BitConverter.IsLittleEndian then Array.Reverse bytes
    let u = BitConverter.ToUInt32(bytes,0)
    encodeFixed32 u

/// Encode double as fixed64 (8-bytes)
let encodeDouble (d:double) =
    let bytes = BitConverter.GetBytes(d)
    if not BitConverter.IsLittleEndian then Array.Reverse bytes
    let UL = BitConverter.ToUInt64(bytes,0)
    encodeFixed64 UL

/// Encode [0,MAX_FIELD_LEN) bytes as length delimited field
///
/// <param name="len">
///     Specifies the length of data to be encoded.  This is needed up-front,
///     because the length is written as a varint (variable number of bytes)
///     before the data bytes.
/// </param>
/// <param name="emplace">
///     Function used to copy or emplace data directly into the output buffer,
///     rather than requiring copies to be passed around.  Especially useful to
///     encode UTF-8 strings directly into the output buffer.
/// </param>
/// <param name="dest">
///     Destination buffer.
///     Generally, this parameter is omitted when defining a set of serialization
///     functions; several serialization functions are then chained together to
///     produce a single function which serializes the entire Message.
/// </param>
let encodeLengthDelimited (len:uint32) (emplace:ArraySegment<byte>->unit) (dest:ZeroCopyBuffer) =
    dest |> encodeVarint (uint64 len) |> ignore
    dest.WriteByteSegment len emplace
    dest

/// Decode the field number and wire-type from a tag.
/// Note: a tag is a field number with a wire type packed in the lower 3 bits.
let decodeTag src =
    let u = decodeVarint src
    let fieldNum = u >>> 3
    // theoretic range is [0, UInt64.MaxValue>>>3],
    // but descriptor.proto defines it as an int32
    // in the range [1,2^28)
    if fieldNum > 0UL && fieldNum <= uint64 RawField.MaxFieldNum then
        let fieldNum = int32 fieldNum
        let wireType = enum<WireType> (int32 u &&& 0x07)
        (fieldNum, wireType)
    else
        raise <| ProtobufWireFormatException("Decode failure: field number must be in range [1, 2^28)")

/// Decode a field into a RawField Discriminated Union.
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

/// Encode a Tag (consisting of a field number and wire-type) as a single varint
let encodeTag (fieldNum:int32) (wireType:WireType) =
    if fieldNum > 0 && fieldNum <= RawField.MaxFieldNum then
        let tag = (fieldNum <<< 3) ||| (int32 wireType)
        encodeVarint (uint64 tag)
    else
        raise <| ProtobufWireFormatException("Encode failure: field numbeer must be in range [1, 2^28)")

/// Encode a varint-based field
let encodeFieldVarint (fieldNum:int32) (u:uint64) =
    encodeTag fieldNum WireType.Varint
    >> encodeVarint u

/// Encode a fixed32-based field
let encodeFieldFixed32 (fieldNum:int32) (u:uint32) =
    encodeTag fieldNum WireType.Fixed32
    >> encodeFixed32 u

/// Encode a fixed64-based field
let encodeFieldFixed64 (fieldNum:int32) (u:uint64) =
    encodeTag fieldNum WireType.Fixed64
    >> encodeFixed64 u

/// Encode a floating point single field
let encodeFieldSingle (fieldNum:int32) (f:single) =
    encodeTag fieldNum WireType.Fixed32
    >> encodeSingle f

/// Encode a floating point double field
let encodeFieldDouble (fieldNum:int32) (d:double) =
    encodeTag fieldNum WireType.Fixed64
    >> encodeDouble d

/// Encode a length delimited field
let encodeFieldLengthDelimited (fieldNum:int32) len (emplace:ArraySegment<byte>->unit) =
    encodeTag fieldNum WireType.LengthDelimited
    >> encodeLengthDelimited len emplace

/// Encode a bytes field
let encodeFieldBytes (fieldNum:int32) (source:ArraySegment<byte>) =
    encodeFieldLengthDelimited
        (int32 fieldNum)
        (uint32 source.Count)
        (fun dest -> Array.Copy( source.Array, source.Offset, dest.Array, dest.Offset, source.Count))

/// Encode a string field
let encodeFieldString fieldNum (s:string) =
    let utf8 = System.Text.Encoding.UTF8
    let len = utf8.GetByteCount(s) |> uint32
    encodeFieldLengthDelimited fieldNum
        len
        (fun dest -> utf8.GetBytes( s, 0, s.Length, dest.Array, dest.Offset) |> ignore)

