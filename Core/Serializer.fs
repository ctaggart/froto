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

    /// Calculate length when encoded as a Varint; if value is default, then length is 0
    let varIntLenDefaulted d (v:uint64) =
        if v = d
        then 0
        else
            let rec loop acc len =
                let bMore = acc > 0x7FUL
                if bMore
                then loop (acc >>> 7) (len+1)
                else len
            loop v 1

    /// Calculate length when encoded as a Varint; if value is default, then length is 0
    let varIntLen (v:uint64) =
        varIntLenDefaulted 0UL v

    /// Calculate length when encoded as a Varint without a default; e.g., a packed field
    let varIntLenNoDefault (v:uint64) =
        let rec loop acc len =
            let bMore = acc > 0x7FUL
            if bMore
            then loop (acc >>> 7) (len+1)
            else len
        loop v 1


    /// Calculate field number length when encoded in a tag
    let tagLen (t:int32) =
        varIntLenNoDefault ((uint64 t) <<< 3)

    /// Apply a function to a Option value, if the value is Some.
    let inline IfSome f opt =
        match opt with
        | None -> id
        | Some(v) -> f v



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
    let helper_vi f (result:byref<'a>) = function
        | RawField.Varint (n, v) ->
            result <- f v
        | raw ->
            raiseMismatch "Varint" raw

    let hydrateInt32 (result:byref<int32>)    = helper_vi int32 &result
    let hydrateInt64 (result:byref<int64>)    = helper_vi int64 &result
    let hydrateUInt32 (result:byref<uint32>)  = helper_vi uint32 &result
    let hydrateUInt64 (result:byref<uint64>)  = helper_vi uint64 &result
    let hydrateSInt32 (result:byref<int32>)   = helper_vi (int32 >> Utility.zagZig32) &result
    let hydrateSInt64 (result:byref<int64>)   = helper_vi (int64 >> Utility.zagZig64) &result
    let hydrateBool (result:byref<bool>)      = helper_vi toBool &result
    let inline hydrateEnum (result:byref<'a>) = helper_vi (int32 >> enum) &result

    /// Helper to deserialize from Fixed32
    let internal helper_fx32 f (result:byref<'a>) = function
        | RawField.Fixed32 (_, v) ->
            result <- f v
        | raw ->
            raiseMismatch "Fixed32" raw

    let hydrateFixed32  (result:byref<uint32>) = helper_fx32 uint32 &result
    let hydrateSFixed32 (result:byref<int32>)  = helper_fx32 int32 &result
    let hydrateSingle   (result:byref<Single>) =
        let aux (u:uint32) =
            // TODO: eliminate the Array allocation,
            //       perhaps using CIL (MSIL) to load float from a register
            let bytes = BitConverter.GetBytes(u)
            if not BitConverter.IsLittleEndian then Array.Reverse bytes
            BitConverter.ToSingle(bytes,0)
        helper_fx32 aux &result

    /// Helper to deserialize from Fixed64
    let internal helper_fx64 f (fld:byref<'a>) = function
        | RawField.Fixed64 (_, v) ->
            fld <- f v
        | raw ->
            raiseMismatch "Fixed64" raw

    let hydrateFixed64  (result:byref<uint64>) = helper_fx64 uint64 &result
    let hydrateSFixed64 (result:byref<int64>) = helper_fx64 int64 &result
    let hydrateDouble   (result:byref<Double>) =
        let aux (u:uint64) =
            // TODO: eliminate the Array allocation,
            //       perhaps using CIL (MSIL) to load float from a register
            let bytes = BitConverter.GetBytes(u)
            if not BitConverter.IsLittleEndian then Array.Reverse bytes
            BitConverter.ToDouble(bytes,0)
        helper_fx64 aux &result


    /// Helper to deserialize from a LengthDelimited
    let internal helper_bytes f (fld:byref<'a>) = function
        | RawField.LengthDelimited (_, v) ->
            fld <- f v
        | raw ->
            raiseMismatch "LengthDelimited" raw

    let internal toString (a:System.ArraySegment<byte>) =
        let utf8 = System.Text.Encoding.UTF8
        utf8.GetString(a.Array, a.Offset, a.Count)

    let internal toByteArray (a:System.ArraySegment<byte>) =
        a.Array.[ a.Offset .. a.Offset + (a.Count-1)]

    let hydrateString (result:byref<string>) = helper_bytes toString &result
    let hydrateBytes  (result:byref<byte array>) = helper_bytes toByteArray &result
    let hydrateMessage messageCtor (result:byref<'a>) =
        helper_bytes messageCtor &result
    let hydrateOptionalMessage messageCtor (result:byref<'a option>) =
        hydrateMessage (messageCtor >> Some) &result

    /// Helper to deserialize Packed Repeated from LengthDelimited.
    /// Since this is used by an inline function (hydratePackedEnum),
    /// it cannot be marked "internal".
    let helper_packed f (result:byref<'a list>) = function
        | RawField.LengthDelimited (_,v) ->
            result <-
                [
                    let s = ZeroCopyBuffer(v)
                    while not s.IsEof do
                        yield (f s)
                ]
        | raw ->
            raiseMismatch "LengthDelimited" raw

    let hydratePackedInt32    (result:byref<int32 list>) = helper_packed (decodeVarint >> int32) &result
    let hydratePackedInt64    (result:byref<int64 list>) = helper_packed (decodeVarint >> int64) &result
    let hydratePackedUInt32   (result:byref<uint32 list>) = helper_packed (decodeVarint >> uint32) &result
    let hydratePackedUInt64   (result:byref<uint64 list>) = helper_packed (decodeVarint >> uint64) &result
    let hydratePackedSInt32   (result:byref<int32 list>) = helper_packed (decodeVarint >> int32 >> Utility.zagZig32) &result
    let hydratePackedSInt64   (result:byref<int64 list>) = helper_packed (decodeVarint >> int64 >> Utility.zagZig64) &result
    let hydratePackedBool     (result:byref<bool list>) = helper_packed (decodeVarint >> toBool) &result
    let inline hydratePackedEnum (result:byref<'a list>) = helper_packed (decodeVarint >> int32 >> enum) &result
    let hydratePackedFixed32  (result:byref<uint32 list>) = helper_packed decodeFixed32 &result
    let hydratePackedFixed64  (result:byref<uint64 list>) = helper_packed decodeFixed64 &result
    let hydratePackedSFixed32 (result:byref<int32 list>) = helper_packed (decodeFixed32 >> int32) &result
    let hydratePackedSFixed64 (result:byref<int64 list>) = helper_packed (decodeFixed64 >> int64) &result
    let hydratePackedSingle   (result:byref<Single list>) = helper_packed decodeSingle &result
    let hydratePackedDouble   (result:byref<Double list>) = helper_packed decodeDouble &result

//---- Serialization

    /// If value = default, then elide the field (don't serialize)
    let inline elided d v f =
        if v = d
        then id
        else f

    /// Generic Dehydrate for all varint types, excepted for signed & bool:
    ///   int32, int64, uint32, uint64, enum
    let inline dehydrateDefaultedVarint d fldNum v = elided d v <| WireFormat.encodeFieldVarint fldNum (uint64 v)
    let inline dehydrateNondefaultedVarint fldNum v = WireFormat.encodeFieldVarint fldNum (uint64 v)

    let dehydrateDefaultedSInt32 d fldNum v = elided d v <| WireFormat.encodeFieldVarint fldNum (Utility.zigZag32 v |> uint64)
    let dehydrateDefaultedSInt64 d fldNum v = elided d v <| WireFormat.encodeFieldVarint fldNum (Utility.zigZag64 v |> uint64)
    let dehydrateDefaultedBool   d fldNum v = elided d v <| dehydrateNondefaultedVarint fldNum (fromBool v)

    let inline dehydrateDefaultedFixed32  d fldNum v = elided d v <| WireFormat.encodeFieldFixed32 fldNum (uint32 v)
    let inline dehydrateDefaultedFixed64  d fldNum v = elided d v <| WireFormat.encodeFieldFixed64 fldNum (uint64 v)

    let dehydrateDefaultedSingle d fldNum v = elided d v <| WireFormat.encodeFieldSingle fldNum v
    let dehydrateDefaultedDouble d fldNum v = elided d v <| WireFormat.encodeFieldDouble fldNum v

    let dehydrateDefaultedString d fldNum v = elided d v <| WireFormat.encodeFieldString fldNum v
    let dehydrateDefaultedBytes  d fldNum v = elided d v <| WireFormat.encodeFieldBytes fldNum v

    let inline dehydrateVarint fldNum (v:'a) = dehydrateDefaultedVarint (Unchecked.defaultof<'a>) fldNum v

    let dehydrateSInt32 fldNum v = dehydrateDefaultedSInt32 0 fldNum v
    let dehydrateSInt64 fldNum v = dehydrateDefaultedSInt64 0L fldNum v
    let dehydrateBool   fldNum v = dehydrateDefaultedBool false fldNum v

    let inline dehydrateFixed32 fldNum v = dehydrateDefaultedFixed32 0 fldNum v
    let inline dehydrateFixed64 fldNum v = dehydrateDefaultedFixed64 0 fldNum v

    let dehydrateSingle fldNum v = dehydrateDefaultedSingle 0.0f fldNum v
    let dehydrateDouble fldNum v = dehydrateDefaultedDouble 0.0 fldNum v

    let dehydrateString fldNum v = dehydrateDefaultedString "" fldNum v
    let dehydrateBytes  fldNum v = dehydrateDefaultedBytes (ArraySegment ([||]:byte array)) fldNum v


    (* Dehydrate Repeated Packed Numeric Values *)

    let dehydratePackedHelper lenFn encFn fieldNum xs =
        let xslen = xs
                    |> lenFn
                    |> uint64
        WireFormat.encodeTag fieldNum WireType.LengthDelimited
        >> WireFormat.encodeVarint xslen
        >> flip (List.fold (fun buf x -> buf |> encFn x )) xs

    let inline varIntListPackedLen encode (xs:'a list) =
        List.sumBy (encode >> Utility.varIntLenNoDefault) xs

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
    let inline dehydrateMessage fieldNum (o:^msg when ^msg : (member SerializeLengthDelimited : ZeroCopyBuffer -> ZeroCopyBuffer)) =
        let serializeMsg zcb = (^msg : (member SerializeLengthDelimited : ZeroCopyBuffer -> ZeroCopyBuffer) (o,zcb))
        WireFormat.encodeTag fieldNum WireType.LengthDelimited
        >> serializeMsg

    let inline dehydrateOptionalMessage fieldNum (o:^msg option when ^msg : (member SerializeLengthDelimited : ZeroCopyBuffer -> ZeroCopyBuffer)) =
        o |> Utility.IfSome (fun o -> dehydrateMessage fieldNum o)

    (* Repeated Field Helpers *)
    type RepeatDelegate<'result,'value> = delegate of byref<'result> * 'value -> unit
    type RepeatMessageDelegate<'ctor,'result,'value> = delegate of 'ctor * byref<'result> * 'value -> unit

    let hydrateInt32Delegate  = RepeatDelegate( fun result field -> hydrateInt32 &result field )
    let hydrateInt64Delegate  = RepeatDelegate( fun result field -> hydrateInt64 &result field )
    let hydrateUInt32Delegate = RepeatDelegate( fun result field -> hydrateUInt32 &result field )
    let hydrateUInt64Delegate = RepeatDelegate( fun result field -> hydrateUInt64 &result field )
    let hydrateSInt32Delegate = RepeatDelegate( fun result field -> hydrateSInt32 &result field )
    let hydrateSInt64Delegate = RepeatDelegate( fun result field -> hydrateSInt64 &result field )
    let hydrateBoolDelegate   = RepeatDelegate( fun result field -> hydrateBool &result field )
// TODO Figure out how to fix this...
//    let hydrateEnumDelegate   = RepeatDelegate( fun result field -> hydrateEnum &result field )
    let hydrateFixed32Delegate  = RepeatDelegate( fun result field -> hydrateFixed32 &result field )
    let hydrateSFixed32Delegate = RepeatDelegate( fun result field -> hydrateSFixed32 &result field )
    let hydrateSingleDelegate   = RepeatDelegate( fun result field -> hydrateSingle &result field )
    let hydrateFixed64Delegate  = RepeatDelegate( fun result field -> hydrateFixed64 &result field )
    let hydrateSFixed64Delegate = RepeatDelegate( fun result field -> hydrateSFixed64 &result field )
    let hydrateDoubleDelegate   = RepeatDelegate( fun result field -> hydrateDouble &result field )
    let hydrateStringDelegate   = RepeatDelegate( fun result field -> hydrateString &result field )
    let hydrateBytesDelegate    = RepeatDelegate( fun result field -> hydrateBytes &result field )
    let hydrateMessageDelegate  = RepeatMessageDelegate( fun ctor result field -> hydrateMessage ctor &result field )
    let hydrateOptionalMessageDelegate  = RepeatMessageDelegate( fun ctor result field -> hydrateOptionalMessage ctor &result field )

    let hydrateRepeated (hydrater:RepeatDelegate<'a,RawField>) (propRef:byref<'a list>) rawField =
        let mutable element = Unchecked.defaultof<'a>
        hydrater.Invoke( &element, rawField )
        propRef <- element :: propRef

    let dehydrateRepeated<'a> (dehydrater:FieldNum -> 'a -> ZeroCopyBuffer -> ZeroCopyBuffer) (fldNum:int32) (vs:'a list) : (ZeroCopyBuffer -> ZeroCopyBuffer) =
        let dh = flip (dehydrater fldNum)
        let wrapperFn (zcb:ZeroCopyBuffer) =
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
/// especially in the DecoderRing and Encode properties.
///
[<AbstractClass>]
type MessageBase () =

    let mutable m_unknownFields = List.empty

    let asArraySegment (zcb:ZeroCopyBuffer) =
        zcb.AsArraySegment

    let remainder (zcb:ZeroCopyBuffer) =
        zcb.Remainder

    /// Derrived classes must provide a Clear function, which resets all
    /// members to their default values; generally, Zero(0) or Empty.
    abstract Clear : Unit -> Unit

    /// Derrived classes must provide a DecoderRing property, which is used
    /// to map from a field number to a deserialization (hydration) function.
    /// This is a map, because Protobuf fields can appear in any order.
    abstract DecoderRing : Map<int,(RawField->unit)>

    /// Derrived classes must provide an Encode method, which is used
    /// to serialize the class.  Generally, this single function chains
    /// the serialization functions for all known members.
    abstract Encode : ZeroCopyBuffer -> ZeroCopyBuffer

    /// Derrived classes MAY provide a list of required fields; when either
    /// merging or deserializing, all of these fields MUST be present in the
    /// serialized message, otherwise an exception will be thrown.
    abstract RequiredFields : Set<FieldNum>
    override x.RequiredFields = Set.empty

    /// List of fields provided in the protobuf, but which were not found on
    /// the DecoderRing.  All these fields will be serialized to the buffer
    /// after all fields on the Encode.
    member x.UnknownFields
        with get() = m_unknownFields
        and  set(v) = m_unknownFields <- v

    // Internal helper
    member private x.MergeWhile (predicate:ZeroCopyBuffer->bool) (zcb:ZeroCopyBuffer) =
        if Set.isEmpty x.RequiredFields then
            seq {
                while predicate zcb do
                    yield WireFormat.decodeField zcb
                }
            |> Seq.iter x.DeserializeField
            zcb
        else
            let mutable foundFields = Set.empty
            seq {
                while predicate zcb do
                    let rawField = WireFormat.decodeField zcb
                    let fieldId = rawField.FieldNum
                    foundFields <- foundFields |> Set.add fieldId
                    yield rawField
                }
            |> Seq.iter x.DeserializeField
            let missingFields = (x.RequiredFields - foundFields)
            if Set.isEmpty missingFields
            then zcb
            else raise <| ProtobufSerializerException(sprintf "Missing required fields %A" missingFields)


    /// Merge from a serialized buffer.
    ///
    /// Does not Clear() the object before merging, so fields which do
    /// not appear in the buffer will remain untouched, and repeated
    /// fields in the buffer will be added to any existing values.
    ///
    /// The entire buffer will be consumed, so must be of the right
    /// length to exactly contain the message.
    member x.Merge (zcb:ZeroCopyBuffer) : ZeroCopyBuffer =
        zcb
        |> x.MergeWhile (fun zcb -> not zcb.IsEof)

    /// Merge from a buffer whose first value is a varint
    /// specifying the length of the message.
    ///
    /// Does not Clear() the object before merging, so fields which do
    /// not appear in the buffer will remain untouched, and repeated
    /// fields in the buffer will be added to any existing values.
    ///
    /// Returns the remaining bytes in the buffer as an ArraySegment.
    member x.MergeLengthDelimited (zcb:ZeroCopyBuffer) =
        let len = zcb |> WireFormat.decodeVarint |> uint32
        let end_ = zcb.Position + len
        zcb
        |> x.MergeWhile (fun zcb -> zcb.Position < end_)

    /// Merge from an ArraySegment.
    ///
    /// Does not Clear() the object before merging, so fields which do
    /// not appear in the buffer will remain untouched, and repeated
    /// fields in the buffer will be added to any existing values.
    ///
    /// The entire ArraySegment will be consumed, so must be of the right
    /// length to exactly contain the message.
    member x.Merge (buf:System.ArraySegment<byte>) =
        ZeroCopyBuffer(buf)
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
        ZeroCopyBuffer(buf)
        |> x.MergeLengthDelimited
        |> remainder

    /// Deserialize from a serialized buffer.
    ///
    /// Clear()'s the object before deserializing.
    ///
    /// The entire buffer will be consumed, so must be of the right
    /// length to exactly contain the message.
    member x.Deserialize (zcb:ZeroCopyBuffer) : ZeroCopyBuffer =
        x.Clear()
        x.Merge(zcb)

    /// Deserialize from a buffer whose first value is a varint
    /// specifying the length of the message.
    ///
    /// Clear()'s the object before deserializing.
    ///
    /// Returns the remaining bytes in the buffer as an ArraySegment.
    member x.DeserializeLengthDelimited (zcb:ZeroCopyBuffer) =
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

    /// Serialize the object by applying all functions on the Encode.
    ///
    /// Will also serialize all fields stored on the UnknownFields list.
    member x.Serialize (zcb:ZeroCopyBuffer) =
        zcb
        |> x.Encode
        |> x.SerializeUnknownFields

    /// Serialize the object by applying all functions on the Encode.
    ///
    /// Will also serialize all fields stored on the UnknownFields list.
    member x.Serialize (buf:System.ArraySegment<byte>) =
        ZeroCopyBuffer(buf)
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
        let lenlen = Utility.varIntLenNoDefault (uint64 len)
        (uint32 lenlen) + len

    /// Serialize first the length as a varint, followed by the serialized
    /// object.
    member x.SerializeLengthDelimited (zcb:ZeroCopyBuffer) =
        zcb
        |> WireFormat.encodeVarint (uint64 x.SerializedLength)
        |> x.Serialize

    /// Serialize first the length as a varint, followed by the serialized
    /// object.
    member x.SerializeLengthDelimited (buf:System.ArraySegment<byte>) =
        ZeroCopyBuffer(buf)
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
    member private x.SerializeUnknownFields (zcb:ZeroCopyBuffer) =

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
