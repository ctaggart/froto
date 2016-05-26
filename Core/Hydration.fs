namespace Froto.Core

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
module Hydration =
    open System
    open Froto.Core
    open Froto.Core.WireFormat
    open Froto.Core.Utility

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
    let hydrateSInt32 = helper_vi (int32 >> zagZig32)
    let hydrateSInt64 = helper_vi (int64 >> zagZig64)
    let hydrateBool   = helper_vi toBool
    let inline hydrateEnum fld  = helper_vi (int32 >> enum) fld

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
    let internal helper_fx64 f = function
        | RawField.Fixed64 (_, v) ->
            f v
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
    let hydrateMessage messageCtor = helper_bytes (ZeroCopyBuffer >> messageCtor)
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
    let hydratePackedSInt32   = helper_packed (decodeVarint >> int32 >> zagZig32)
    let hydratePackedSInt64   = helper_packed (decodeVarint >> int64 >> zagZig64)
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

    let dehydrateDefaultedSInt32 d fldNum v = elided d v <| WireFormat.encodeFieldVarint fldNum (zigZag32 v |> uint64)
    let dehydrateDefaultedSInt64 d fldNum v = elided d v <| WireFormat.encodeFieldVarint fldNum (zigZag64 v |> uint64)
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
        List.sumBy (encode >> varIntLenNoDefault) xs

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
        let encode = zigZag32 >> uint64
        dehydratePackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.encodeVarint)
            fieldNum xs

    let dehydratePackedSInt64 fieldNum xs =
        let encode = zigZag64 >> uint64
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

    (* Repeated Field Helpers *)
    let dehydrateRepeated<'a> (dehydrater:FieldNum -> 'a -> ZeroCopyBuffer -> ZeroCopyBuffer) (fldNum:int32) (vs:'a list) : (ZeroCopyBuffer -> ZeroCopyBuffer) =
        let dh = flip (dehydrater fldNum)
        let wrapperFn (zcb:ZeroCopyBuffer) =
            vs
            |> List.iter (dh zcb >> ignore)
            zcb
        wrapperFn

    let dehydrateMessage (fn:'m -> ZeroCopyBuffer -> ZeroCopyBuffer) fieldNum m =
        WireFormat.encodeTag fieldNum WireType.LengthDelimited
        >> fn m

    let dehydrateOptionalMessage (fn:'m -> ZeroCopyBuffer -> ZeroCopyBuffer) fieldNum (m:'m option) =
        m |> IfSome (fun o -> dehydrateMessage fn fieldNum o)

    let inline serializeClassLengthDelimited (o:^msg when ^msg : (member SerializeLengthDelimited : ZeroCopyBuffer -> ZeroCopyBuffer)) =
        let serializeMsg zcb = (^msg : (member SerializeLengthDelimited : ZeroCopyBuffer -> ZeroCopyBuffer) (o,zcb))
        serializeMsg
