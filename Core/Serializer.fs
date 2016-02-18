namespace Froto.Core.Encoding

open System
open Froto.Core
open Froto.Core.WireFormat

module Utility =

    let zigZag32 (n:int32) = (n <<< 1) ^^^ (n >>> 31)
    let zigZag64 (n:int64) = (n <<< 1) ^^^ (n >>> 63)
    let zagZig32 (n:int32) = int32(uint32 n >>> 1) ^^^ (if n&&&1  = 0  then 0  else -1 )
    let zagZig64 (n:int64) = int64(uint64 n >>> 1) ^^^ (if n&&&1L = 0L then 0L else -1L)

    let varIntLen (v:uint64) =
        let rec loop acc len =
            let bMore = acc > 0x7FUL
            if bMore
            then loop (acc >>> 7) (len+1)
            else len
        loop v 1

    let tagLen (t:int32) =
        varIntLen ((uint64 t) <<< 3)

module Serializer =

    let inline internal flip f a b = f b a
    let inline internal toBool (u:uint64) = not (u=0UL)
    let inline internal fromBool b = if b then 1UL else 0UL

    let internal raiseMismatch expected actual =
        let extractNumAndType = function
            | RawField.Varint (n,_)  -> n, "VarInt"
            | RawField.Fixed32 (n,_) -> n, "Fixed32"
            | RawField.Fixed64 (n,_) -> n, "Fixed64"
            | RawField.LengthDelimited (n,_) -> n, "LengthDelimited"
        let (n, found) = actual |> extractNumAndType
        let s = sprintf "Deserialize failure: wiretype mismatch for field %d: expected %s, found %s" n expected found
        raise <| ProtobufWireFormatException(s)

    (* Deserialize from VarInt *)
    let helper_vi f fld = function
        | RawField.Varint (n, v) ->
            fld := f v
        | raw ->
            raiseMismatch "VarInt" raw

    let hydrateInt32  = helper_vi int32
    let hydrateInt64  = helper_vi int64
    let hydrateUInt32 = helper_vi uint32
    let hydrateUInt64 = helper_vi uint64
    let hydrateSInt32 = helper_vi (int32 >> Utility.zagZig32)
    let hydrateSInt64 = helper_vi (int64 >> Utility.zagZig64)
    let hydrateBool   = helper_vi toBool
    let inline hydrateEnum x  = helper_vi (int32 >> enum) x

    (* hydrate from Fixed32 *)
    let internal helper_fx32 f fld = function
        | RawField.Fixed32 (_, v) ->
            fld := f v
        | raw ->
            raiseMismatch "Fixed32" raw

    let hydrateFixed32  = helper_fx32 int32
    let hydrateSFixed32 = helper_fx32 (int32 >> Utility.zagZig32)
    let hydrateSingle  =
        let aux (u:uint32) =
            // TODO: eliminate the Array allocation,
            //       perhaps using CIL (MSIL) to load float from a register
            let bytes = BitConverter.GetBytes(u)
            if not BitConverter.IsLittleEndian then Array.Reverse bytes
            BitConverter.ToSingle(bytes,0)
        helper_fx32 aux


    (* hydrate from Fixed64 *)
    let internal helper_fx64 f fld = function
        | RawField.Fixed64 (_, v) ->
            fld := f v
        | raw ->
            raiseMismatch "Fixed64" raw

    let hydrateFixed64  = helper_fx64 int64
    let hydrateSFixed64 = helper_fx64 (int64 >> Utility.zagZig64)
    let hydrateDouble  =
        let aux (u:uint64) =
            // TODO: eliminate the Array allocation,
            //       perhaps using CIL (MSIL) to load float from a register
            let bytes = BitConverter.GetBytes(u)
            if not BitConverter.IsLittleEndian then Array.Reverse bytes
            BitConverter.ToDouble(bytes,0)
        helper_fx64 aux


    (* hydrate from LengthDelimited *)
    let internal helper_bytes f fld = function
        | RawField.LengthDelimited (_, v) ->
            fld := f v
        | raw ->
            raiseMismatch "LengthDelimited" raw

    let internal toString (a:System.ArraySegment<byte>) =
        let utf8 = System.Text.Encoding.UTF8
        utf8.GetString(a.Array, a.Offset, a.Count)

    let internal toByteArray (a:System.ArraySegment<byte>) =
        a.Array.[ a.Offset .. a.Offset + a.Count]

    let hydrateString = helper_bytes toString
    let hydrateBytes  = helper_bytes toByteArray
    // TODO: Can the following be made type safe?
    let hydrateMessage messageCtor = helper_bytes messageCtor

    (* hydrate Packed Repeated from LengthDelimited *)
    let helper_packed f fld = function
        | RawField.LengthDelimited (_,v) ->
            fld := 
                [
                    let s = ZeroCopyReadBuffer(v)
                    while not s.IsEof do
                        yield (f s)
                ]
        | raw ->
            raiseMismatch "LengthDelimited" raw

    let hydratePackedInt32    = helper_packed (decodeVarint >> int32)
    let hydratePackedInt64    = helper_packed (decodeVarint >> int64)
    let hydratePackedUInt32   = helper_packed (decodeVarint >> uint32)
    let hydratePackedUInt64   = helper_packed (decodeVarint >> uint64)
    let hydratePackedSInt32   = helper_packed (decodeVarint >> int32 >> Utility.zagZig32)
    let hydratePackedSInt64   = helper_packed (decodeVarint >> int64 >> Utility.zagZig64)
    let hydratePackedBool     = helper_packed (decodeVarint >> toBool)
    let inline hydratePackedEnum x    = helper_packed (decodeVarint >> int32 >> enum) x
    let hydratePackedFixed32  = helper_packed decodeFixed32
    let hydratePackedFixed64  = helper_packed decodeFixed64
    let hydratePackedSFixed32 = helper_packed (decodeFixed32 >> int32 >> Utility.zagZig32)
    let hydratePackedSFixed64 = helper_packed (decodeFixed64 >> int64 >> Utility.zagZig64)
    let hydratePackedSingle   = helper_packed decodeSingle
    let hydratePackedDouble   = helper_packed decodeDouble

    /// Generic Dehydrate for all varint types, excepted for signed & bool:
    ///   int32, int64, uint32, uint64, enum
    let inline dehydrateVarint fldNum i = WireFormat.encodeFieldVarint fldNum (uint64 i)

    let dehydrateSInt32 fldNum i = WireFormat.encodeFieldVarint fldNum (Utility.zigZag32 i |> uint64)
    let dehydrateSInt64 fldNum i = WireFormat.encodeFieldVarint fldNum (Utility.zigZag64 i |> uint64)

    let dehydrateBool fldNum b = dehydrateVarint fldNum (fromBool b)

    let inline dehydrateFixed32  fldNum i = WireFormat.encodeFieldFixed32 fldNum (uint32 i)
    let inline dehydrateFixed64  fldNum i = WireFormat.encodeFieldFixed64 fldNum (uint64 i)
    let inline dehydrateSFixed32 fldNum i = WireFormat.encodeFieldFixed32 fldNum (Utility.zigZag32 i |> uint32)
    let inline dehydrateSFixed64 fldNum i = WireFormat.encodeFieldFixed64 fldNum (Utility.zigZag64 i |> uint64)

    let dehydrateSingle fldNum i = WireFormat.encodeFieldSingle fldNum i
    let dehydrateDouble fldNum i = WireFormat.encodeFieldDouble fldNum i

    let dehydrateString fldNum s = WireFormat.encodeFieldString fldNum s
    let dehydrateBytes  fldNum buf = WireFormat.encodeFieldBytes fldNum buf

    (* Dehydrate Repeated Packed Numeric Values *)

    let dehydratePackedHelper lenFn encFn fieldNum xs =
        let xslen = xs
                    |> lenFn
                    |> uint64
        WireFormat.encodeTag fieldNum WireType.LengthDelimited
        >> WireFormat.encodeVarint xslen
        >> flip (List.fold (fun buf x -> buf |> encFn x )) xs

    let inline varIntListPackedLen (xs:'a list) =
        List.sumBy (uint64 >> Utility.varIntLen) xs

    /// Generic Dehydrate for all packed varint types, excepted for bool & signed:
    ///   int32, int64, uint32, uint64, enum
    let inline dehydratePackedVarint fieldNum xs =
        dehydratePackedHelper
            varIntListPackedLen
            (uint64 >> WireFormat.encodeVarint)
            fieldNum xs

    let dehydratePackedBool fieldNum xs =
        let packedLen = List.length
        dehydratePackedHelper
            List.length (* encodes to 1 byte per bool *)
            (fromBool >> WireFormat.encodeVarint)
            fieldNum xs

    let dehydratePackedSInt32 fieldNum xs =
        dehydratePackedHelper
            varIntListPackedLen
            (Utility.zigZag32 >> uint64 >> WireFormat.encodeVarint)
            fieldNum xs

    let dehydratePackedSInt64 fieldNum xs =
        dehydratePackedHelper
            varIntListPackedLen
            (Utility.zigZag64 >> uint64 >> WireFormat.encodeVarint)
            fieldNum xs

    let inline fixedListPackedLen size = (List.length >> ((*) size))
    let inline fixed32ListPackedLen xs = fixedListPackedLen 4 xs
    let inline fixed64ListPackedLen xs = fixedListPackedLen 8 xs

    let inline dehydratePackedFixed32 fieldNum xs =
        dehydratePackedHelper
            fixed32ListPackedLen
            (uint32 >> WireFormat.encodeFixed32)
            fieldNum xs

    let inline dehydratePackedFixed64 fieldNum xs =
        dehydratePackedHelper
            fixed64ListPackedLen
            (uint64 >> WireFormat.encodeFixed64)
            fieldNum xs

    let dehydratePackedSingle fieldNum xs =
        dehydratePackedHelper
            fixed32ListPackedLen
            WireFormat.encodeSingle
            fieldNum xs

    let dehydratePackedDouble fieldNum xs =
        dehydratePackedHelper
            fixed64ListPackedLen
            WireFormat.encodeDouble
            fieldNum xs

    let dehydratePackedSFixed32 fieldNum xs =
        dehydratePackedHelper
            fixed32ListPackedLen
            (Utility.zigZag32 >> uint32 >> WireFormat.encodeFixed32)
            fieldNum xs

    let dehydratePackedSFixed64 fieldNum xs =
        dehydratePackedHelper
            fixed64ListPackedLen
            (Utility.zigZag64 >> uint64 >> WireFormat.encodeFixed64)
            fieldNum xs

    (* Dehydrate Message *)
    let inline dehydrateMessage fieldNum (o:^msg when ^msg : (member SerializeLengthDelimited : ZeroCopyWriteBuffer -> ZeroCopyWriteBuffer)) =
        let serializeMsg zcb = (^msg : (member SerializeLengthDelimited : ZeroCopyWriteBuffer -> ZeroCopyWriteBuffer) (o,zcb))
        WireFormat.encodeTag fieldNum WireType.LengthDelimited
        >> serializeMsg

    (* Repeated Field Helpers *)
    let hydrateRepeated<'a> (hydrater:'a ref -> RawField -> unit) propRef rawField =
        let element = Unchecked.defaultof<'a>
        hydrater (ref element) rawField
        propRef := element :: !propRef

    let dehydrateRepeated<'a> (dehydrater:FieldNum -> 'a -> ZeroCopyWriteBuffer -> ZeroCopyWriteBuffer) (fldNum:int32) (vs:'a list) : (ZeroCopyWriteBuffer -> ZeroCopyWriteBuffer) =
        let dh = flip (dehydrater fldNum)
        let wrapperFn (zcb:ZeroCopyWriteBuffer) =
            vs
            |> List.iter (dh zcb >> ignore)
            zcb
        wrapperFn


[<AbstractClass>]
type MessageBase () =

    let mutable m_unknownFields = List.empty

    let asArraySegment (zcb:ZeroCopyWriteBuffer) =
        zcb.AsArraySegment

    abstract DecoderRing : Map<int,(RawField->unit)>
    abstract EncoderRing : (ZeroCopyWriteBuffer -> ZeroCopyWriteBuffer) list

    member x.Deserialize (buf:System.ArraySegment<byte>) =
        let zcb = ZeroCopyReadBuffer(buf)
        seq {
            while not zcb.IsEof do
                yield WireFormat.decodeField zcb
            }
        |> Seq.iter x.DeserializeField
        zcb.Remainder

    member x.DeserializeLengthDelimited (buf:System.ArraySegment<byte>) =
        let zcb = ZeroCopyReadBuffer(buf)
        let len = zcb |> WireFormat.decodeVarint |> uint32
        x.Deserialize( zcb.Remainder )

    member x.Serialize (zcb:ZeroCopyWriteBuffer) =
        for fn in x.EncoderRing do
            fn zcb |> ignore
        x.SerializeUnknownFields zcb
        zcb

    member x.Serialize (buf:System.ArraySegment<byte>) =
        ZeroCopyWriteBuffer(buf)
        |> x.Serialize
        |> asArraySegment

    member x.SerializedLength =
        let ncb = NullWriteBuffer()
        ncb |> x.Serialize |> ignore
        ncb.Length

    member x.Serialize() =
        Array.zeroCreate (int32 x.SerializedLength)
        |> ArraySegment
        |> x.Serialize

    member x.SerializeLengthDelimited (zcb:ZeroCopyWriteBuffer) =
        zcb
        |> WireFormat.encodeVarint (uint64 x.SerializedLength)
        |> x.Serialize

    member x.SerializeLengthDelimited (buf:System.ArraySegment<byte>) =
        ZeroCopyWriteBuffer(buf)
        |> x.SerializeLengthDelimited
        |> asArraySegment

    member x.SerializedLengthDelimitedLength =
        let len = x.SerializedLength
        let lenlen = Utility.varIntLen (uint64 len)
        (uint32 lenlen) + len

    member x.SerializeLengthDelimited() =
        Array.zeroCreate (int32 x.SerializedLengthDelimitedLength)
        |> ArraySegment
        |> x.SerializeLengthDelimited

    member x.UnknownFields = m_unknownFields

    member private x.DeserializeField (field:Encoding.RawField) =
        let n = field.FieldNum
        match x.DecoderRing |> Map.tryFind n with
        | Some(deserializeFn)   -> deserializeFn field
        | None                  -> m_unknownFields <- field :: m_unknownFields

    member private x.SerializeUnknownFields (zcb:ZeroCopyWriteBuffer) =

        let inline emplace (src:ArraySegment<byte>) (dst:ArraySegment<byte>) =
            Array.Copy(src.Array, src.Offset, dst.Array, dst.Offset, src.Count)
            
        for field in x.UnknownFields do
            match field with
                | RawField.Varint (n,v) ->
                    zcb |> WireFormat.encodeFieldVarint n v
                | RawField.LengthDelimited (n,v) ->
                    zcb |> WireFormat.encodeFieldLengthDelimited n (v.Count|>uint32) (emplace v)
                | RawField.Fixed32 (n,v) ->
                    zcb |> WireFormat.encodeFieldFixed32 n v
                | RawField.Fixed64 (n,v) ->
                    zcb |> WireFormat.encodeFieldFixed64 n v
            |> ignore
