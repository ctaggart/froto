namespace Froto.Core.Encoding

open System
open Froto.Core
open Froto.Core.WireFormat

///
/// Utility functions used by the serializer
///
module Utility =

    /// Encode SInt32
    let zigZag32 (n:int32) = (n <<< 1) ^^^ (n >>> 31)
    /// Encode SInt64
    let zigZag64 (n:int64) = (n <<< 1) ^^^ (n >>> 63)

    /// Decode SInt32
    let zagZig32 (n:int32) = int32(uint32 n >>> 1) ^^^ (if n&&&1  = 0  then 0  else -1 )
    /// Decode SInt64
    let zagZig64 (n:int64) = int64(uint64 n >>> 1) ^^^ (if n&&&1L = 0L then 0L else -1L)

    /// Calculate length when encoded as a Varint
    let varIntLen (v:uint64) =
        let rec loop acc len =
            let bMore = acc > 0x7FUL
            if bMore
            then loop (acc >>> 7) (len+1)
            else len
        loop v 1

    // Calculate field number length when encoded in a tag
    let tagLen (t:int32) =
        varIntLen ((uint64 t) <<< 3)

///
/// Serialization and Deserialization.
///
/// This module contains support for writing (or generating) code which
/// performs deserialization (hydration) and serialization (dehydration) of
/// individual properties.  These can be used directly to create methods
/// on classes, records, discriminated unions, etc.
///
/// In addition, an abstract base-class (MessageBase) is provided which
/// defines a simple DSL for constructing Classes which model Protobuf
/// Messages.
///
module Serializer =

    let inline internal flip f a b = f b a
    let inline internal toBool (u:uint64) = not (u=0UL)
    let inline internal fromBool b = if b then 1UL else 0UL

    let internal raiseMismatch expected actual =
        let extractNumAndType = function
            | RawField.Varint (n,_)  -> n, "Varint"
            | RawField.Fixed32 (n,_) -> n, "Fixed32"
            | RawField.Fixed64 (n,_) -> n, "Fixed64"
            | RawField.LengthDelimited (n,_) -> n, "LengthDelimited"
        let (n, found) = actual |> extractNumAndType
        let s = sprintf "Deserialize failure: wiretype mismatch for field %d: expected %s, found %s" n expected found
        raise <| ProtobufSerializerException(s)

//---- Deserialization

    /// Helper to deserialize from Varint.
    /// Since this is used by an inline function (hydrateEnum),
    /// it cannot be marked "internal".
    let helper_vi f fld = function
        | RawField.Varint (n, v) ->
            fld := f v
        | raw ->
            raiseMismatch "Varint" raw

    let hydrateInt32  = helper_vi int32
    let hydrateInt64  = helper_vi int64
    let hydrateUInt32 = helper_vi uint32
    let hydrateUInt64 = helper_vi uint64
    let hydrateSInt32 = helper_vi (int32 >> Utility.zagZig32)
    let hydrateSInt64 = helper_vi (int64 >> Utility.zagZig64)
    let hydrateBool   = helper_vi toBool
    let inline hydrateEnum x  = helper_vi (int32 >> enum) x

    /// Helper to deserialize from Fixed32
    let internal helper_fx32 f fld = function
        | RawField.Fixed32 (_, v) ->
            fld := f v
        | raw ->
            raiseMismatch "Fixed32" raw

    let hydrateFixed32  = helper_fx32 uint32
    let hydrateSFixed32 = helper_fx32 int32
    let hydrateSingle  =
        let aux (u:uint32) =
            // TODO: eliminate the Array allocation,
            //       perhaps using CIL (MSIL) to load float from a register
            let bytes = BitConverter.GetBytes(u)
            if not BitConverter.IsLittleEndian then Array.Reverse bytes
            BitConverter.ToSingle(bytes,0)
        helper_fx32 aux

    /// Helper to deserialize from Fixed64
    let internal helper_fx64 f fld = function
        | RawField.Fixed64 (_, v) ->
            fld := f v
        | raw ->
            raiseMismatch "Fixed64" raw

    let hydrateFixed64  = helper_fx64 uint64
    let hydrateSFixed64 = helper_fx64 int64
    let hydrateDouble  =
        let aux (u:uint64) =
            // TODO: eliminate the Array allocation,
            //       perhaps using CIL (MSIL) to load float from a register
            let bytes = BitConverter.GetBytes(u)
            if not BitConverter.IsLittleEndian then Array.Reverse bytes
            BitConverter.ToDouble(bytes,0)
        helper_fx64 aux


    /// Helper to deserialize from a LengthDelimited
    let internal helper_bytes f fld = function
        | RawField.LengthDelimited (_, v) ->
            fld := f v
        | raw ->
            raiseMismatch "LengthDelimited" raw

    let internal toString (a:System.ArraySegment<byte>) =
        let utf8 = System.Text.Encoding.UTF8
        utf8.GetString(a.Array, a.Offset, a.Count)

    let internal toByteArray (a:System.ArraySegment<byte>) =
        a.Array.[ a.Offset .. a.Offset + (a.Count-1)]

    let hydrateString = helper_bytes toString
    let hydrateBytes  = helper_bytes toByteArray
    // TODO: Can the following be made type safe?
    let hydrateMessage messageCtor = helper_bytes messageCtor

    /// Helper to deserialize Packed Repeated from LengthDelimited.
    /// Since this is used by an inline function (hydratePackedEnum),
    /// it cannot be marked "internal".
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
    let hydratePackedSFixed32 = helper_packed (decodeFixed32 >> int32)
    let hydratePackedSFixed64 = helper_packed (decodeFixed64 >> int64)
    let hydratePackedSingle   = helper_packed decodeSingle
    let hydratePackedDouble   = helper_packed decodeDouble

    /// Generic Dehydrate for all varint types, excepted for signed & bool:
    ///   int32, int64, uint32, uint64, enum
    let inline dehydrateVarint fldNum i = WireFormat.encodeFieldVarint fldNum (uint64 i)

//---- Serialization

    let dehydrateSInt32 fldNum i = WireFormat.encodeFieldVarint fldNum (Utility.zigZag32 i |> uint64)
    let dehydrateSInt64 fldNum i = WireFormat.encodeFieldVarint fldNum (Utility.zigZag64 i |> uint64)

    let dehydrateBool fldNum b = dehydrateVarint fldNum (fromBool b)

    let inline dehydrateFixed32  fldNum i = WireFormat.encodeFieldFixed32 fldNum (uint32 i)
    let inline dehydrateFixed64  fldNum i = WireFormat.encodeFieldFixed64 fldNum (uint64 i)

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

    let inline varIntListPackedLen encode (xs:'a list) =
        List.sumBy (encode >> Utility.varIntLen) xs

    /// Generic Dehydrate for all packed varint types, excepted for bool & signed:
    ///   int32, int64, uint32, uint64, enum
    let inline dehydratePackedVarint fieldNum xs =
        let encode = uint64
        dehydratePackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.encodeVarint)
            fieldNum xs

    let dehydratePackedBool fieldNum xs =
        let boolPackedLen = List.length
        dehydratePackedHelper
            boolPackedLen (* encodes to 1 byte per bool *)
            (fromBool >> WireFormat.encodeVarint)
            fieldNum xs

    let dehydratePackedSInt32 fieldNum xs =
        let encode = Utility.zigZag32 >> uint64
        dehydratePackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.encodeVarint)
            fieldNum xs

    let dehydratePackedSInt64 fieldNum xs =
        let encode = Utility.zigZag64 >> uint64
        dehydratePackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.encodeVarint)
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

///
/// Abstract base class for Protobuf-serializable Messages.
///
/// This class provides a simple DSL for deriving your own serializable
/// classes.  Such classes are best written in F#, due to the strong
/// dependance on function currying (partial function application),
/// especially in the DecoderRing and EncoderRing properties.
///
[<AbstractClass>]
type MessageBase () =

    let mutable m_unknownFields = List.empty

    let asArraySegment (zcb:ZeroCopyWriteBuffer) =
        zcb.AsArraySegment

    let remainder (zcb:ZeroCopyReadBuffer) =
        zcb.Remainder

    /// Derrived classes must provide a Clear function, which resets all
    /// members to their default values; generally, Zero(0) or Empty.
    abstract Clear : Unit -> Unit

    /// Derrived classes must provide a DecoderRing property, which is used
    /// to map from a field number to a deserialization (hydration) function.
    /// This is a map, because Protobuf fields can appear in any order.
    abstract DecoderRing : Map<int,(RawField->unit)>

    /// Derrived classes must provide an EncoderRing property, which is used
    /// to serialize the class.  Generally, this single function chains
    /// the serialization functions for all known members.
    abstract EncoderRing : ZeroCopyWriteBuffer -> ZeroCopyWriteBuffer

    /// List of fields provided in the protobuf, but which were not found on
    /// the DecoderRing.  All these fields will be serialized to the buffer
    /// after all fields on the EncoderRing.
    member x.UnknownFields
        with get() = m_unknownFields
        and  set(v) = m_unknownFields <- v

    /// Merge from a serialized buffer.
    ///
    /// Does not Clear() the object before merging, so fields which do
    /// not appear in the buffer will remain untouched, and repeated
    /// fields in the buffer will be added to any existing values.
    ///
    /// The entire buffer will be consumed, so must be of the right
    /// length to exactly contain the message.
    member x.Merge (zcb:ZeroCopyReadBuffer) : ZeroCopyReadBuffer =
        seq {
            while not zcb.IsEof do
                yield WireFormat.decodeField zcb
            }
        |> Seq.iter x.DeserializeField
        zcb

    /// Merge from a buffer whose first value is a varint
    /// specifying the length of the message.
    ///
    /// Does not Clear() the object before merging, so fields which do
    /// not appear in the buffer will remain untouched, and repeated
    /// fields in the buffer will be added to any existing values.
    ///
    /// Returns the remaining bytes in the buffer as an ArraySegment.
    member x.MergeLengthDelimited (zcb:ZeroCopyReadBuffer) =
        let len = zcb |> WireFormat.decodeVarint |> uint32
        let end_ = zcb.Position + len
        seq {
            while zcb.Position < end_ do
                yield WireFormat.decodeField zcb
            }
        |> Seq.iter x.DeserializeField
        if end_ <> zcb.Position then
            raise <| ProtobufSerializerException("Incorrect length for length-delimited field")
        zcb

    /// Merge from an ArraySegment.
    ///
    /// Does not Clear() the object before merging, so fields which do
    /// not appear in the buffer will remain untouched, and repeated
    /// fields in the buffer will be added to any existing values.
    ///
    /// The entire ArraySegment will be consumed, so must be of the right
    /// length to exactly contain the message.
    member x.Merge (buf:System.ArraySegment<byte>) =
        ZeroCopyReadBuffer(buf)
        |> x.Merge
        |> remainder

    /// Merge from an ArraySegment whose first value is a varint
    /// specifying the length of the message.
    ///
    /// Does not Clear() the object before merging, so fields which do
    /// not appear in the buffer will remain untouched, and repeated
    /// fields in the buffer will be added to any existing values.
    ///
    /// Returns the remaining bytes in the buffer as an ArraySegment.
    member x.MergeLengthDelimited (buf:System.ArraySegment<byte>) =
        ZeroCopyReadBuffer(buf)
        |> x.MergeLengthDelimited
        |> remainder

    /// Deserialize from a serialized buffer.
    ///
    /// Clear()'s the object before deserializing.
    ///
    /// The entire buffer will be consumed, so must be of the right
    /// length to exactly contain the message.
    member x.Deserialize (zcb:ZeroCopyReadBuffer) : ZeroCopyReadBuffer =
        x.Clear()
        x.Merge(zcb)

    /// Deserialize from a buffer whose first value is a varint
    /// specifying the length of the message.
    ///
    /// Clear()'s the object before deserializing.
    ///
    /// Returns the remaining bytes in the buffer as an ArraySegment.
    member x.DeserializeLengthDelimited (zcb:ZeroCopyReadBuffer) =
        x.Clear()
        x.MergeLengthDelimited(zcb)

    /// Deserialize from an ArraySegment.
    ///
    /// Clear()'s the object before deserializing.
    ///
    /// The entire ArraySegment will be consumed, so must be of the right
    /// length to exactly contain the message.
    member x.Deserialize (buf:System.ArraySegment<byte>) =
        x.Clear()
        x.Deserialize(buf)

    /// Deserialize from an ArraySegment whose first value is a varint
    /// specifying the length of the message.
    ///
    /// Clear()'s the object before deserializing.
    ///
    /// Returns the remaining bytes in the buffer as an ArraySegment.
    member x.DeserializeLengthDelimited (buf:System.ArraySegment<byte>) =
        x.Clear()
        x.DeserializeLengthDelimited(buf)

    /// Return number of bytes needed to serialize the object
    ///
    /// @see SerializedLengthDelimitedLength
    member x.SerializedLength =
        let ncb = NullWriteBuffer()
        ncb |> x.Serialize |> ignore
        ncb.Length

    /// Serialize the object by applying all functions on the EncoderRing.
    ///
    /// Will also serialize all fields stored on the UnknownFields list.
    member x.Serialize (zcb:ZeroCopyWriteBuffer) =
        zcb
        |> x.EncoderRing
        |> x.SerializeUnknownFields

    /// Serialize the object by applying all functions on the EncoderRing.
    ///
    /// Will also serialize all fields stored on the UnknownFields list.
    member x.Serialize (buf:System.ArraySegment<byte>) =
        ZeroCopyWriteBuffer(buf)
        |> x.Serialize
        |> asArraySegment

    /// Serialize to a new byte array, and return as an ArraySegment.
    member x.Serialize() =
        Array.zeroCreate (int32 x.SerializedLength)
        |> ArraySegment
        |> x.Serialize

    /// Return number of bytes needed to serialize both the object and the
    /// length of the object as a varint.
    member x.SerializedLengthDelimitedLength =
        let len = x.SerializedLength
        let lenlen = Utility.varIntLen (uint64 len)
        (uint32 lenlen) + len

    /// Serialize first the length as a varint, followed by the serialized
    /// object.
    member x.SerializeLengthDelimited (zcb:ZeroCopyWriteBuffer) =
        zcb
        |> WireFormat.encodeVarint (uint64 x.SerializedLength)
        |> x.Serialize

    /// Serialize first the length as a varint, followed by the serialized
    /// object.
    member x.SerializeLengthDelimited (buf:System.ArraySegment<byte>) =
        ZeroCopyWriteBuffer(buf)
        |> x.SerializeLengthDelimited
        |> asArraySegment

    /// Serialize, to a new byte array, the length as a varint followed by
    /// the serialized object, and return as an ArraySegment.
    member x.SerializeLengthDelimited() =
        Array.zeroCreate (int32 x.SerializedLengthDelimitedLength)
        |> ArraySegment
        |> x.SerializeLengthDelimited

    /// Deserialize a single field, using the DecoderRing.
    /// If the field number is not on the DecoderRing, then store the
    /// RawField on the UnknownFields list.
    member private x.DeserializeField (field:Encoding.RawField) =
        let n = field.FieldNum
        match x.DecoderRing |> Map.tryFind n with
        | Some(deserializeFn)   -> deserializeFn field
        | None                  -> m_unknownFields <- field :: m_unknownFields

    /// Serialize all unknown fields
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
        zcb
