namespace Froto.Serialization.Encoding

module WireFormat =

    open System
    open Froto.Serialization
    open Froto.Serialization.Encoding

    /// Maximum length of a length-delimited field (64Mb)
    let MAX_FIELD_LEN = 64u * 1024u * 1024u
   
    /// Unpack a varint-encoded uint64 (1-9 bytes)
    let unpackVarint (src:ZeroCopyBuffer) =

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
                raise <| ProtobufWireFormatException( "Unpack: Unterminated VarInt" )

        loop 0UL 0 src

    let rec internal unpackFixedLoop acc len n (src:ZeroCopyBuffer) =
        if n < len
        then
            let byt = src.ReadByte()
            let acc = (uint64 byt <<< (8*n)) ||| acc
            unpackFixedLoop acc len (n+1) src
        else
            acc

    /// Unpack fixed-size uint32 (8-bytes)
    let unpackFixed32 src = uint32 (unpackFixedLoop 0UL 4 0 src)

    /// Unpack fized-size uint64 (8-bytes)
    let unpackFixed64 src = uint64 (unpackFixedLoop 0UL 8 0 src)

    /// Unpack fixed-size float (4-bytes)
    let unpackSingle src =
        let u = unpackFixed32 src
        // TODO: eliminate the Array allocation,
        //       perhaps using CIL (MSIL) to load float from a register
        let bytes = BitConverter.GetBytes(u)
        if not BitConverter.IsLittleEndian then Array.Reverse bytes
        BitConverter.ToSingle(bytes,0)

    /// Unpack fized-size double (8-bytes)
    let unpackDouble src =
        let u = unpackFixed64 src
        // TODO: eliminate the Array allocation,
        //       perhaps using CIL (MSIL) to load float from a register
        let bytes = BitConverter.GetBytes(u)
        if not BitConverter.IsLittleEndian then Array.Reverse bytes
        BitConverter.ToDouble(bytes,0)

    /// Unpack length delimited field [0,MAX_FIELD_LEN) bytes.
    ///
    /// NOTE: Returns an ArraySegement which references a section of the
    /// underlying source Array, rather than a copy.  Changes to that source
    /// Array will therefore impact this function's returned value.
    let unpackLengthDelimited src =
        let len = unpackVarint src
        if len < uint64 MAX_FIELD_LEN then
            src.ReadByteSegment(uint32 len)
        else
            raise <| ProtobufWireFormatException( sprintf "Unpack: Maximum field length of %d bytes exceeded" MAX_FIELD_LEN)

    /// Pack uint64 as a varint (1-9 bytes)
    let packVarint (u:uint64) (dest:ZeroCopyBuffer) =

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

    /// Pack uint32 as fixed32 (4 bytes)
    let packFixed32 (u:uint32) (dest:ZeroCopyBuffer) =
        dest.WriteByte (byte u)
        dest.WriteByte (byte (u >>> 8))
        dest.WriteByte (byte (u >>> 16))
        dest.WriteByte (byte (u >>> 24))
        dest

    /// Pack uint64 as fixed64 (8 bytes)
    let packFixed64 (u:uint64) dest =
        dest
        |> packFixed32 (uint32 u)
        |> packFixed32 (uint32 (u >>> 32))
   
    /// Pack float as fixed32 (4 bytes)
    let packSingle (f:single) =
        let bytes = BitConverter.GetBytes(f)
        if not BitConverter.IsLittleEndian then Array.Reverse bytes
        let u = BitConverter.ToUInt32(bytes,0)
        packFixed32 u

    /// Pack double as fixed64 (8-bytes)
    let packDouble (d:double) =
        let bytes = BitConverter.GetBytes(d)
        if not BitConverter.IsLittleEndian then Array.Reverse bytes
        let UL = BitConverter.ToUInt64(bytes,0)
        packFixed64 UL

    /// Pack [0,MAX_FIELD_LEN) bytes as length delimited field
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
    let packLengthDelimited (len:uint32) (emplace:ArraySegment<byte>->unit) (dest:ZeroCopyBuffer) =
        dest |> packVarint (uint64 len) |> ignore
        dest.WriteByteSegment len emplace
        dest

    /// Unpack the field number and wire-type from a tag.
    /// Note: a tag is a field number with a wire type packed in the lower 3 bits.
    let unpackTag src =
        let u = unpackVarint src
        let fieldNum = u >>> 3
        // theoretic range is [0, UInt64.MaxValue>>>3],
        // but descriptor.proto defines it as an int32
        // in the range [1,2^28)
        if fieldNum > 0UL && fieldNum <= uint64 RawField.MaxFieldNum then
            let fieldNum = int32 fieldNum
            let wireType = enum<WireType> (int32 u &&& 0x07)
            (fieldNum, wireType)
        else
            raise <| ProtobufWireFormatException(sprintf "Unpack: field number must be in range [1, 2^28); found %d" fieldNum)

    /// Unpack a field into a RawField Discriminated Union.
    let unpackField src =
        let fieldNum, wireType = unpackTag src
        match wireType with
        | WireType.Varint ->
            Varint (fieldNum, unpackVarint src)
        | WireType.Fixed64 ->
            Fixed64 (fieldNum, unpackFixed64 src)
        | WireType.LengthDelimited ->
            LengthDelimited (fieldNum, unpackLengthDelimited src)
        | WireType.Fixed32 ->
            Fixed32 (fieldNum, unpackFixed32 src)

        | WireType.StartGroup
        | WireType.EndGroup
        | _ -> raise <| ProtobufWireFormatException(sprintf "Unpack: unsupported wiretype on field #%d: %A" fieldNum wireType)

    /// Pack a Tag (consisting of a field number and wire-type) as a single varint
    let packTag (fieldNum:int32) (wireType:WireType) =
        if fieldNum > 0 && fieldNum <= RawField.MaxFieldNum then
            let tag = (fieldNum <<< 3) ||| (int32 wireType)
            packVarint (uint64 tag)
        else
            raise <| ProtobufWireFormatException(sprintf "Pack: field numbeer must be in range [1, 2^28); given %d" fieldNum)

    /// Pack a varint-based field
    let packFieldVarint (fieldNum:int32) (u:uint64) =
        packTag fieldNum WireType.Varint
        >> packVarint u

    /// Pack a fixed32-based field
    let packFieldFixed32 (fieldNum:int32) (u:uint32) =
        packTag fieldNum WireType.Fixed32
        >> packFixed32 u

    /// Pack a fixed64-based field
    let packFieldFixed64 (fieldNum:int32) (u:uint64) =
        packTag fieldNum WireType.Fixed64
        >> packFixed64 u

    /// Pack a floating point single field
    let packFieldSingle (fieldNum:int32) (f:single) =
        packTag fieldNum WireType.Fixed32
        >> packSingle f

    /// Pack a floating point double field
    let packFieldDouble (fieldNum:int32) (d:double) =
        packTag fieldNum WireType.Fixed64
        >> packDouble d

    /// Pack a length delimited field
    let packFieldLengthDelimited (fieldNum:int32) len (emplace:ArraySegment<byte>->unit) =
        packTag fieldNum WireType.LengthDelimited
        >> packLengthDelimited len emplace

    /// Pack a bytes field
    let packFieldBytes (fieldNum:int32) (source:ArraySegment<byte>) =
        packFieldLengthDelimited
            (int32 fieldNum)
            (uint32 source.Count)
            (fun dest -> Array.Copy( source.Array, source.Offset, dest.Array, dest.Offset, source.Count))

    /// Pack a string field
    let packFieldString fieldNum (s:string) =
        let utf8 = System.Text.Encoding.UTF8
        let len = utf8.GetByteCount(s) |> uint32
        packFieldLengthDelimited fieldNum
            len
            (fun dest -> utf8.GetBytes( s, 0, s.Length, dest.Array, dest.Offset) |> ignore)

    /// Pack a raw field
    let packFieldRaw = function
        | Varint (n,v) -> packFieldVarint n v
        | Fixed32 (n,v) -> packFieldFixed32 n v
        | Fixed64 (n,v) -> packFieldFixed64 n v
        | LengthDelimited (n,v) -> packFieldBytes n v
