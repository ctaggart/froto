namespace Froto.Core.Encoding

open System
open Froto.Core
open Froto.Core.WireFormat


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
module RecordSerializer =

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
    let helper_vi f = function
        | RawField.Varint (n, v) ->
            f v
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
    let internal helper_fx32 f = function
        | RawField.Fixed32 (_, v) ->
            f v
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
    let internal helper_bytes f = function
        | RawField.LengthDelimited (_, v) ->
            f v
        | raw ->
            raiseMismatch "LengthDelimited" raw

    let internal toString (a:System.ArraySegment<byte>) =
        let utf8 = System.Text.Encoding.UTF8
        utf8.GetString(a.Array, a.Offset, a.Count)

    let internal toByteArray (a:System.ArraySegment<byte>) =
        a.Array.[ a.Offset .. a.Offset + (a.Count-1)]

    let hydrateString = helper_bytes toString
    let hydrateBytes  = helper_bytes toByteArray
    let hydrateMessage messageCtor = helper_bytes messageCtor
    let hydrateOptionalMessage messageCtor = hydrateMessage (messageCtor >> Some)

    /// Helper to deserialize Packed Repeated from LengthDelimited.
    /// Since this is used by an inline function (hydratePackedEnum),
    /// it cannot be marked "internal".
    let helper_packed f = function
        | RawField.LengthDelimited (_,v) ->
            [
                let s = ZeroCopyBuffer(v)
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
    let dehydrateRepeated<'a> (dehydrater:FieldNum -> 'a -> ZeroCopyBuffer -> ZeroCopyBuffer) (fldNum:int32) (vs:'a list) : (ZeroCopyBuffer -> ZeroCopyBuffer) =
        let dh = flip (dehydrater fldNum)
        let wrapperFn (zcb:ZeroCopyBuffer) =
            vs
            |> List.iter (dh zcb >> ignore)
            zcb
        wrapperFn

//
//---- Record Serialization Functions
//

(* Serialize *)
    let inline serialize (m:^msg when ^msg : (member Serializer : ZeroCopyBuffer -> ZeroCopyBuffer ) ) zcb =
        let serialize m zcb = (^msg : (member Serializer : ZeroCopyBuffer -> ZeroCopyBuffer) (m, zcb) )
        zcb
        |> serialize m

    let inline serializedLength (m:^msg when ^msg : (member Serializer : ZeroCopyBuffer -> ZeroCopyBuffer ) ) =
        let serialize m zcb = (^msg : (member Serializer : ZeroCopyBuffer -> ZeroCopyBuffer) (m, zcb) )
        let nwb = NullWriteBuffer()
        nwb |> serialize m |> ignore
        nwb.Length

    let inline serializedLengthDelimitedLength m =
        let len = serializedLength m
        let lenlen = Utility.varIntLenNoDefault (uint64 len)
        (uint32 lenlen) + len

    let inline serializeLengthDelimited m zcb =
        zcb
        |> WireFormat.encodeVarint (uint64 (serializedLength m))
        |> serialize m

(* Serializer with unknowns *)
    let inline emplace (src:ArraySegment<byte>) (dst:ArraySegment<byte>) =
        Array.Copy(src.Array, src.Offset, dst.Array, dst.Offset, src.Count)

    let internal serializeUnknown = function
        | RawField.Varint (n,v) ->
            WireFormat.encodeFieldVarint n v
        | RawField.LengthDelimited (n,v) ->
            WireFormat.encodeFieldLengthDelimited n (v.Count|>uint32) (emplace v)
        | RawField.Fixed32 (n,v) ->
            WireFormat.encodeFieldFixed32 n v
        | RawField.Fixed64 (n,v) ->
            WireFormat.encodeFieldFixed64 n v

    let rec serializeUnknowns zcb = function
        | [] -> zcb
        | h::t ->
            serializeUnknown h zcb
            |>  flip serializeUnknowns t
        
    let inline serializeWithUnknowns m unks zcb =
        zcb
        |> serialize m
        |> (flip serializeUnknowns unks)

    let inline serializedLengthWithUnknowns (m:^msg when ^msg : (member Serializer : ZeroCopyBuffer -> ZeroCopyBuffer ) ) unks =
        let serialize m zcb = (^msg : (member Serializer : ZeroCopyBuffer -> ZeroCopyBuffer) (m, zcb) )
        let nwb = NullWriteBuffer()
        nwb |> serializeWithUnknowns m unks |> ignore
        nwb.Length

    let inline serializedLengthDelimitedLengthWithUnknowns m unks =
        let len = serializedLengthWithUnknowns m unks
        let lenlen = Utility.varIntLenNoDefault (uint64 len)
        (uint32 lenlen) + len

    let inline serializeLengthDelimitedWithUnknowns m unks zcb =
        zcb
        |> WireFormat.encodeVarint (uint64 (serializedLengthWithUnknowns m unks))
        |> serializeWithUnknowns m unks

(* Deserialize *)
    let findDecoders decoderRing decodeUnknown (field:Encoding.RawField) =
        let n = field.FieldNum
        match decoderRing |> Map.tryFind n with
        | Some(decode) -> (decode, field)
        | None -> (decodeUnknown, field)

    let hydrate decoderRing decodeUnknown msgAcc fields =
        fields
        |> Seq.map (findDecoders decoderRing decodeUnknown)
        |> Seq.fold (fun acc (fn, fld) -> fn acc fld) msgAcc


    let inline deserializeFields (m:^msg when ^msg : (member DecoderRing : Map<int,^msg -> Encoding.RawField -> ^msg>)) fields =
        let decoderRing = (^msg : (member DecoderRing: Map<int,^msg -> Encoding.RawField -> ^msg>) (m) )
        let ignoreUnknown m fld = m
        let result =
            hydrate decoderRing ignoreUnknown m fields
        result

    let inline decodeFixup (m:^msg when ^msg : (static member DecodeFixup : ^msg -> ^msg)) =
        let decodeFixup m = (^msg : (static member DecodeFixup : ^msg -> ^msg) (m) )
        decodeFixup m

    let inline deserializeLengthDelimited m zcb =
        zcb
        |> Utility.decodeLengthDelimited
        |> deserializeFields m
        |> decodeFixup

    let inline deserialize m zcb =
        zcb
        |> Utility.decodeBuffer
        |> deserializeFields m
        |> decodeFixup


    let inline deserializeFieldsWithUnknowns (m:^msg when ^msg : (member DecoderRing : Map<int,^msg -> Encoding.RawField -> ^msg>)) fields =
        let decoderRing = (^msg : (member DecoderRing: Map<int,^msg -> Encoding.RawField -> ^msg>) (m) )
        let unks = ref List.empty
        let captureUnknown m fld =
            unks := fld :: !unks
            m
        let result =
            hydrate decoderRing captureUnknown m fields
        (result, List.rev !unks)

    let inline deserializeLengthDelimitedWithUnknowns m zcb =
        let (m,unks) =
            zcb
            |> Utility.decodeLengthDelimited
            |> deserializeFieldsWithUnknowns m
        (decodeFixup m, unks)

    let inline deserializeWithUnknowns m zcb =
        let (m,unks) =
            zcb
            |> Utility.decodeBuffer
            |> deserializeFieldsWithUnknowns m
        (decodeFixup m, unks)
