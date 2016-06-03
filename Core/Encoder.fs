namespace Froto.Serialization.Encoding

///
/// Hydration and Dehydration of .NET data types.
///
/// This module contains support for writing (or generating) code which
/// performs hydration (deserialization) and dehydration (serialization) of
/// individual properties.  These can be used directly to create methods
/// on classes, records, discriminated unions, etc.
///
/// In addition, a set of generic functions are provided which defines a
/// simple DSL for constructing Records which model Protobuf Messages.
///
module ProtobufEncoder =
    open System
    open Froto.Serialization
    open Froto.Serialization.Encoding.WireFormat
    open Froto.Serialization.Encoding.Utility

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
    /// Since this is used by an inline function (decodeEnum),
    /// it cannot be marked "internal".
    let helper_vi f = function
        | RawField.Varint (n, v) ->
            f v
        | raw ->
            raiseMismatch "Varint" raw

    let decodeInt32  = helper_vi int32
    let decodeInt64  = helper_vi int64
    let decodeUInt32 = helper_vi uint32
    let decodeUInt64 = helper_vi uint64
    let decodeSInt32 = helper_vi (int32 >> zagZig32)
    let decodeSInt64 = helper_vi (int64 >> zagZig64)
    let decodeBool   = helper_vi toBool
    let inline decodeEnum fld  = helper_vi (int32 >> enum) fld

    /// Helper to deserialize from Fixed32
    let internal helper_fx32 f = function
        | RawField.Fixed32 (_, v) ->
            f v
        | raw ->
            raiseMismatch "Fixed32" raw

    let decodeFixed32  = helper_fx32 uint32
    let decodeSFixed32 = helper_fx32 int32
    let decodeSingle  =
        let aux (u:uint32) =
            // TODO: eliminate the Array allocation,
            //       perhaps using CIL (MSIL) to load float from a register
            let bytes = BitConverter.GetBytes(u)
            if not BitConverter.IsLittleEndian then Array.Reverse bytes
            BitConverter.ToSingle(bytes,0)
        helper_fx32 aux

    /// Helper to deserialize from Fixed64
    let internal helper_fx64 f = function
        | RawField.Fixed64 (_, v) ->
            f v
        | raw ->
            raiseMismatch "Fixed64" raw

    let decodeFixed64  = helper_fx64 uint64
    let decodeSFixed64 = helper_fx64 int64
    let decodeDouble  =
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

    let decodeString = helper_bytes toString
    let decodeBytes  = helper_bytes toByteArray
    let decodeMessage messageCtor = helper_bytes (ZeroCopyBuffer >> messageCtor)
    let decodeOptionalMessage messageCtor = decodeMessage (messageCtor >> Some)

    /// Helper to deserialize Packed Repeated from LengthDelimited.
    /// Since this is used by an inline function (decodePackedEnum),
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

    let decodePackedInt32    = helper_packed (unpackVarint >> int32)
    let decodePackedInt64    = helper_packed (unpackVarint >> int64)
    let decodePackedUInt32   = helper_packed (unpackVarint >> uint32)
    let decodePackedUInt64   = helper_packed (unpackVarint >> uint64)
    let decodePackedSInt32   = helper_packed (unpackVarint >> int32 >> zagZig32)
    let decodePackedSInt64   = helper_packed (unpackVarint >> int64 >> zagZig64)
    let decodePackedBool     = helper_packed (unpackVarint >> toBool)
    let inline decodePackedEnum x    = helper_packed (unpackVarint >> int32 >> enum) x
    let decodePackedFixed32  = helper_packed unpackFixed32
    let decodePackedFixed64  = helper_packed unpackFixed64
    let decodePackedSFixed32 = helper_packed (unpackFixed32 >> int32)
    let decodePackedSFixed64 = helper_packed (unpackFixed64 >> int64)
    let decodePackedSingle   = helper_packed unpackSingle
    let decodePackedDouble   = helper_packed unpackDouble

//---- Serialization

    let encodeRawFields fieldList zcb =
        fieldList
        |> List.fold (fun zcb field -> WireFormat.packFieldRaw field zcb) zcb

    /// If value = default, then elide the field (don't serialize)
    let inline elided d v f =
        if v = d
        then id
        else f

    /// Generic Encode for all varint types, excepted for signed & bool:
    ///   int32, int64, uint32, uint64, enum
    let inline encodeDefaultedVarint d fldNum v = elided d v <| WireFormat.packFieldVarint fldNum (uint64 v)
    let inline encodeNondefaultedVarint fldNum v = WireFormat.packFieldVarint fldNum (uint64 v)

    let encodeDefaultedSInt32 d fldNum v = elided d v <| WireFormat.packFieldVarint fldNum (zigZag32 v |> uint64)
    let encodeDefaultedSInt64 d fldNum v = elided d v <| WireFormat.packFieldVarint fldNum (zigZag64 v |> uint64)
    let encodeDefaultedBool   d fldNum v = elided d v <| encodeNondefaultedVarint fldNum (fromBool v)

    let inline encodeDefaultedFixed32  d fldNum v = elided d v <| WireFormat.packFieldFixed32 fldNum (uint32 v)
    let inline encodeDefaultedFixed64  d fldNum v = elided d v <| WireFormat.packFieldFixed64 fldNum (uint64 v)

    let encodeDefaultedSingle d fldNum v = elided d v <| WireFormat.packFieldSingle fldNum v
    let encodeDefaultedDouble d fldNum v = elided d v <| WireFormat.packFieldDouble fldNum v

    let encodeDefaultedString d fldNum v = elided d v <| WireFormat.packFieldString fldNum v
    let encodeDefaultedBytes  d fldNum v = elided d v <| WireFormat.packFieldBytes fldNum v

    let inline encodeVarint fldNum (v:'a) = encodeDefaultedVarint (Unchecked.defaultof<'a>) fldNum v

    let encodeSInt32 fldNum v = encodeDefaultedSInt32 0 fldNum v
    let encodeSInt64 fldNum v = encodeDefaultedSInt64 0L fldNum v
    let encodeBool   fldNum v = encodeDefaultedBool false fldNum v

    let inline encodeFixed32 fldNum v = encodeDefaultedFixed32 0 fldNum v
    let inline encodeFixed64 fldNum v = encodeDefaultedFixed64 0 fldNum v

    let encodeSingle fldNum v = encodeDefaultedSingle 0.0f fldNum v
    let encodeDouble fldNum v = encodeDefaultedDouble 0.0 fldNum v

    let encodeString fldNum v = encodeDefaultedString "" fldNum v
    let encodeBytes  fldNum v = encodeDefaultedBytes (ArraySegment ([||]:byte array)) fldNum v


    (* Encode Repeated Packed Numeric Values *)

    let encodePackedHelper lenFn encFn fieldNum xs =
        let xslen = xs
                    |> lenFn
                    |> uint64
        WireFormat.packTag fieldNum WireType.LengthDelimited
        >> WireFormat.packVarint xslen
        >> flip (List.fold (fun buf x -> buf |> encFn x )) xs

    let inline varIntListPackedLen encode (xs:'a list) =
        List.sumBy (encode >> varIntLenNoDefault) xs

    /// Generic Encode for all packed varint types, excepted for bool & signed:
    ///   int32, int64, uint32, uint64, enum
    let inline encodePackedVarint fieldNum xs =
        let encode = uint64
        encodePackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.packVarint)
            fieldNum xs

    let encodePackedBool fieldNum xs =
        let boolPackedLen = List.length
        encodePackedHelper
            boolPackedLen (* encodes to 1 byte per bool *)
            (fromBool >> WireFormat.packVarint)
            fieldNum xs

    let encodePackedSInt32 fieldNum xs =
        let encode = zigZag32 >> uint64
        encodePackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.packVarint)
            fieldNum xs

    let encodePackedSInt64 fieldNum xs =
        let encode = zigZag64 >> uint64
        encodePackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.packVarint)
            fieldNum xs

    let inline fixedListPackedLen size = (List.length >> ((*) size))
    let inline fixed32ListPackedLen xs = fixedListPackedLen 4 xs
    let inline fixed64ListPackedLen xs = fixedListPackedLen 8 xs

    let inline encodePackedFixed32 fieldNum xs =
        encodePackedHelper
            fixed32ListPackedLen
            (uint32 >> WireFormat.packFixed32)
            fieldNum xs

    let inline encodePackedFixed64 fieldNum xs =
        encodePackedHelper
            fixed64ListPackedLen
            (uint64 >> WireFormat.packFixed64)
            fieldNum xs

    let encodePackedSingle fieldNum xs =
        encodePackedHelper
            fixed32ListPackedLen
            WireFormat.packSingle
            fieldNum xs

    let encodePackedDouble fieldNum xs =
        encodePackedHelper
            fixed64ListPackedLen
            WireFormat.packDouble
            fieldNum xs

    (* Encode Message *)

    (* Repeated Field Helpers *)
    let encodeRepeated<'a> (encoder:FieldNum -> 'a -> ZeroCopyBuffer -> ZeroCopyBuffer) (fldNum:int32) (vs:'a list) : (ZeroCopyBuffer -> ZeroCopyBuffer) =
        let dh = flip (encoder fldNum)
        let wrapperFn (zcb:ZeroCopyBuffer) =
            vs
            |> List.iter (dh zcb >> ignore)
            zcb
        wrapperFn

    let encodeMessage (fn:'m -> ZeroCopyBuffer -> ZeroCopyBuffer) fieldNum m =
        WireFormat.packTag fieldNum WireType.LengthDelimited
        >> fn m

    let encodeOptionalMessage (fn:'m -> ZeroCopyBuffer -> ZeroCopyBuffer) fieldNum (m:'m option) =
        m |> IfSome (fun o -> encodeMessage fn fieldNum o)

