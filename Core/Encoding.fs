namespace Froto.Serialization.Encoding

open System
open Froto.Serialization
open Froto.Serialization.Encoding.WireFormat
open Froto.Serialization.Encoding.Utility

///
/// Decoding of .NET data types from Protobuf wire format.
///
/// This module contains support for writing (or generating) code which
/// performs encoding ad decoding of individual properties.  These can be
/// used directly to create methods on classes, records, discriminated unions,
/// etc. for serialization and deserialization.
///
/// In addition, this module provides a set of generic functions which create
/// a simple DSL for constructing Records which model Protobuf Messages.
///
module Decode =

    /// Helper to deserialize from Varint.
    /// Since this is used by an inline function (toEnum),
    /// it cannot be marked "internal".
    let helper_vi f = function
        | RawField.Varint (n, v) ->
            f v
        | raw ->
            RawField.raiseMismatch "Varint" raw

    let toInt32  = helper_vi int32
    let toInt64  = helper_vi int64
    let toUInt32 = helper_vi uint32
    let toUInt64 = helper_vi uint64
    let toSInt32 = helper_vi (int32 >> zagZig32)
    let toSInt64 = helper_vi (int64 >> zagZig64)
    let toBool   = helper_vi int64ToBool
    let inline toEnum fld  = helper_vi (int32 >> enum) fld

    /// Helper to deserialize from Fixed32
    let internal helper_fx32 f = function
        | RawField.Fixed32 (_, v) ->
            f v
        | raw ->
            RawField.raiseMismatch "Fixed32" raw

    let toFixed32  = helper_fx32 uint32
    let toSFixed32 = helper_fx32 int32
    let toSingle  =
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
            RawField.raiseMismatch "Fixed64" raw

    let toFixed64  = helper_fx64 uint64
    let toSFixed64 = helper_fx64 int64
    let toDouble  =
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
            RawField.raiseMismatch "LengthDelimited" raw

    let internal arraySegtoString (a:System.ArraySegment<byte>) =
        let utf8 = System.Text.Encoding.UTF8
        utf8.GetString(a.Array, a.Offset, a.Count)

    let internal toByteArray (a:System.ArraySegment<byte>) =
        a.Array.[ a.Offset .. a.Offset + (a.Count-1)]

    let toString = helper_bytes arraySegtoString
    let toBytes  = helper_bytes toByteArray
    let toMessage messageCtor = helper_bytes (ZeroCopyBuffer >> messageCtor)
    let toOptionalMessage messageCtor = toMessage (messageCtor >> Some)

    /// Helper to deserialize Packed Repeated from LengthDelimited.
    /// Since this is used by an inline function (toPackedEnum),
    /// it cannot be marked "internal".
    let helper_packed f = function
        | RawField.LengthDelimited (_,v) ->
            [
                let s = ZeroCopyBuffer(v)
                while not s.IsEof do
                    yield (f s)
            ]
        | raw ->
            RawField.raiseMismatch "LengthDelimited" raw

    let toPackedInt32    = helper_packed (unpackVarint >> int32)
    let toPackedInt64    = helper_packed (unpackVarint >> int64)
    let toPackedUInt32   = helper_packed (unpackVarint >> uint32)
    let toPackedUInt64   = helper_packed (unpackVarint >> uint64)
    let toPackedSInt32   = helper_packed (unpackVarint >> int32 >> zagZig32)
    let toPackedSInt64   = helper_packed (unpackVarint >> int64 >> zagZig64)
    let toPackedBool     = helper_packed (unpackVarint >> int64ToBool)
    let inline toPackedEnum x    = helper_packed (unpackVarint >> int32 >> enum) x
    let toPackedFixed32  = helper_packed unpackFixed32
    let toPackedFixed64  = helper_packed unpackFixed64
    let toPackedSFixed32 = helper_packed (unpackFixed32 >> int32)
    let toPackedSFixed64 = helper_packed (unpackFixed64 >> int64)
    let toPackedSingle   = helper_packed unpackSingle
    let toPackedDouble   = helper_packed unpackDouble


///
/// Encoding of .NET data types to Protobuf wire format.
///
/// This module contains support for writing (or generating) code which
/// performs encoding ad decoding of individual properties.  These can be
/// used directly to create methods on classes, records, discriminated unions,
/// etc. for serialization and deserialization.
///
/// In addition, this module provides a set of generic functions which create
/// a simple DSL for constructing Records which model Protobuf Messages.
///
module Encode =

    /// Encode a list of RawFields into a ZeroCopyBuffer
    let fromRawFields fieldList zcb =
        fieldList
        |> List.fold (fun zcb field -> WireFormat.packFieldRaw field zcb) zcb

    /// If value = default, then elide the field (don't serialize)
    let inline elideDefault defV v f =
        if v = defV
        then id
        else f

    /// Generic Encode for all varint types, excepted for signed & bool:
    ///   int32, int64, uint32, uint64, enum
    let inline fromDefaultedVarint defV fldNum v = elideDefault defV v <| WireFormat.packFieldVarint fldNum (uint64 v)
    let inline fromNondefaultedVarint fldNum v = WireFormat.packFieldVarint fldNum (uint64 v)

    let fromDefaultedSInt32 defV fldNum v = elideDefault defV v <| WireFormat.packFieldVarint fldNum (zigZag32 v |> uint64)
    let fromDefaultedSInt64 defV fldNum v = elideDefault defV v <| WireFormat.packFieldVarint fldNum (zigZag64 v |> uint64)
    let fromDefaultedBool   defV fldNum v = elideDefault defV v <| fromNondefaultedVarint fldNum (boolToInt64 v)

    let inline fromDefaultedFixed32  defV fldNum v = elideDefault defV v <| WireFormat.packFieldFixed32 fldNum (uint32 v)
    let inline fromDefaultedFixed64  defV fldNum v = elideDefault defV v <| WireFormat.packFieldFixed64 fldNum (uint64 v)

    let fromDefaultedSingle defV fldNum v = elideDefault defV v <| WireFormat.packFieldSingle fldNum v
    let fromDefaultedDouble defV fldNum v = elideDefault defV v <| WireFormat.packFieldDouble fldNum v

    let fromDefaultedString defV fldNum v = elideDefault defV v <| WireFormat.packFieldString fldNum v
    let fromDefaultedBytes  defV fldNum v = elideDefault defV v <| WireFormat.packFieldBytes fldNum v

    let inline fromVarint fldNum (v:'a) = fromDefaultedVarint (Unchecked.defaultof<'a>) fldNum v

    let fromSInt32 fldNum v = fromDefaultedSInt32 0 fldNum v
    let fromSInt64 fldNum v = fromDefaultedSInt64 0L fldNum v
    let fromBool   fldNum v = fromDefaultedBool false fldNum v

    let inline fromFixed32 fldNum v = fromDefaultedFixed32 0 fldNum v
    let inline fromFixed64 fldNum v = fromDefaultedFixed64 0 fldNum v

    let fromSingle fldNum v = fromDefaultedSingle 0.0f fldNum v
    let fromDouble fldNum v = fromDefaultedDouble 0.0 fldNum v

    let fromString fldNum v = fromDefaultedString "" fldNum v
    let fromBytes  fldNum v = fromDefaultedBytes (ArraySegment ([||]:byte array)) fldNum v


    (* Encode Repeated Packed Numeric Values *)

    /// Helper for encoding packed types.
    /// Writes a length delimited field containing a list of values packed
    /// into the field body.
    ///
    /// NOTE: Protobuf does NOT provide a means to encode the field type, so
    /// encoder and decoder clients must agree on what is contained in the
    /// field body.
    let fromPackedHelper lenFn encFn fieldNum xs =
        let xslen = xs
                    |> lenFn
                    |> uint64
        WireFormat.packTag fieldNum WireType.LengthDelimited
        >> WireFormat.packVarint xslen
        >> flip (List.fold (fun buf x -> buf |> encFn x )) xs

    /// Calculate the total length of an encoded list of varints.
    let inline varIntListPackedLen encode (xs:'a list) =
        List.sumBy (encode >> varIntLenNoDefault) xs

    /// Generic Encode for all packed varint types, excepted for bool & signed:
    ///   int32, int64, uint32, uint64, enum
    let inline fromPackedVarint fieldNum xs =
        let encode = uint64
        fromPackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.packVarint)
            fieldNum xs

    let fromPackedBool fieldNum xs =
        let boolPackedLen = List.length
        fromPackedHelper
            boolPackedLen (* encodes to 1 byte per bool *)
            (boolToInt64 >> WireFormat.packVarint)
            fieldNum xs

    let fromPackedSInt32 fieldNum xs =
        let encode = zigZag32 >> uint64
        fromPackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.packVarint)
            fieldNum xs

    let fromPackedSInt64 fieldNum xs =
        let encode = zigZag64 >> uint64
        fromPackedHelper
            (varIntListPackedLen encode)
            (encode >> WireFormat.packVarint)
            fieldNum xs

    let inline fixedListPackedLen size = (List.length >> ((*) size))
    let inline fixed32ListPackedLen xs = fixedListPackedLen 4 xs
    let inline fixed64ListPackedLen xs = fixedListPackedLen 8 xs

    let inline fromPackedFixed32 fieldNum xs =
        fromPackedHelper
            fixed32ListPackedLen
            (uint32 >> WireFormat.packFixed32)
            fieldNum xs

    let inline fromPackedFixed64 fieldNum xs =
        fromPackedHelper
            fixed64ListPackedLen
            (uint64 >> WireFormat.packFixed64)
            fieldNum xs

    let fromPackedSingle fieldNum xs =
        fromPackedHelper
            fixed32ListPackedLen
            WireFormat.packSingle
            fieldNum xs

    let fromPackedDouble fieldNum xs =
        fromPackedHelper
            fixed64ListPackedLen
            WireFormat.packDouble
            fieldNum xs

    (* Encode Message *)

    /// Encode a list of same-type values, using the provided encoder and tag
    /// for each.  Unlike packed fields which share a single field tag,
    /// each value encoded will be complete with a field tag (type + FieldNum).
    ///
    let fromRepeated<'a> (encoder:FieldNum -> 'a -> ZeroCopyBuffer -> ZeroCopyBuffer) (fldNum:int32) (xs:'a list) : (ZeroCopyBuffer -> ZeroCopyBuffer) =
        let enc = flip (encoder fldNum)
        let wrapperFn (zcb:ZeroCopyBuffer) =
            xs
            |> List.iter (enc zcb >> ignore)
            zcb
        wrapperFn

    /// Encode a message with the supplied field number, using the supplied
    /// encode function.  This is a convienence function to simplify the call
    /// site and avoid exposing WireFormat to callers.
    let fromMessage (fn:'m -> ZeroCopyBuffer -> ZeroCopyBuffer) fieldNum m =
        WireFormat.packTag fieldNum WireType.LengthDelimited
        >> fn m

    /// Helper to encode an optional message, or nothing if None
    let fromOptionalMessage (fn:'m -> ZeroCopyBuffer -> ZeroCopyBuffer) fieldNum (m:'m option) =
        m |> Option.foldBack (fun o -> fromMessage fn fieldNum o)

