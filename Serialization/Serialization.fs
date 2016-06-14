﻿namespace Froto.Serialization

/// <summary>
/// Serialization support functions.
///
/// These functions can be used (for example) by a code generator which
/// creates types (F# Records, .NET Objects, etc.) that model a protobuf
/// message, and which can easily be serialized and deserialized from/to
/// either ArraySegments and, if more control over the underlying memory
/// is desired, from/TO ZeroCopyBuffers.
/// </summary>
///
/// See ExampleProtoRecord.fs and ExampleProtoClass.fs for example code.
///
/// Serializable types must provide, at minimum, a static `Serializer` method
/// which takes two parameters: an instance of the type and a ZeroCopyBuffer.
/// This method is expected to update the ZCB with the serialized data.
///
///    <c>Serializer : (m:^T, ZeroCopyBuffer) -> ZeroCopyBuffer</c>
///
/// Deserializable types must provide, at minimum, the following static
/// methods.
///
///     * DecoderRing   - maps FieldId to a deserialization function
///     * RememberFound - adds a FieldId to list of found fields
///     * DecodeFixup   - called after decoding the message
///     * RequiredFields- list of (proto2) required fields; can be empty
///     * FoundFields   - returns list of fields found
///     * UnknownFields - returns list of unknown fields
///
/// These can be associated with any valid F# type, including Records and
/// Classes, because the serialization functions use Static Type Constraints
/// to locate these static methods.
///
/// <c>DecoderRing : Map &lt; int, 'T -> RawField -> 'T &gt;</c>
///
/// Map of FieldId's to deserialization functions.  Each deserialization
/// function takes a type instance and a RawField, deserializes the RawField
/// into the type or creates a new type instance with the deserialized field,
/// and returns the new or update object.
///
/// NOTE: Since a FieldId of zero (0) is considered invalid by the Google
/// Protobuf Encoding Specification, we use a map from FieldId zero (0)
/// as a means to record unknown fields (any FieldId that is not in this
/// map).  If FieldId zero (0) is NOT supplied, and an unknown field is
/// encountered, Froto will throw a run-time error.  If preservation of
/// unknown fields is not needed, then the provided zero function may
/// do nothing (i.e., return the input type instance unmodified).
///
/// <c>RememberFound : ('T, FieldId) -> 'T</c>
///
/// This method is called to allow recording of found FieldIds.  The method
/// takes a type and FieldId tuple, and returns an updated or new type with
/// the FieldId recorded.  Generally used for Proto2 support, when checking
/// if all required fields were supplied in the message.  May simply return
/// 'T unaltered if such recording is not necessary.
///
/// <c>DecodeFixup : 'T -> 'T</c>
///
/// This method is called after deserializing the message.  It is generally
/// used to reverse any lists which were built up by adding to the head, so
/// that the list is in the correct order.  For example, the list of unknown
/// fields may need to be so reversed.  The DecodeFixup method will be
/// naturally called on inner messages as they are decoded, as long as
/// they are deserialized using <c>deserializeFrom</c> or a related function.
///
/// <c>RequiredFields : Set &lt; FieldId &gt;</c>
///
/// Set of FieldIds the must be present on a valid message.  May be Set.empty.
/// Deserialization will throw an exception if any required fields indicated
/// here are missing.
///
/// <c>FoundFields : 'T -> Set &lt; FieldId &gt;</c>
///
/// Extracts the set of found fields from 'T.  May return Set.empty.
///
/// <c>UnknownFields : 'T -> RawField list</c>
///
/// Extracts the list of unknown fields (fields not on the decoder ring)
/// from the supplied type instance.

module Serializer =

    open System
    open Froto.Serialization.Encoding
    open Froto.Serialization.Encoding.Utility

(* Serialize *)

    /// Serialize message into a ZeroCopyBuffer
    let inline serializeTo m zcb =
        (^msg : (static member Serializer : ^msg * ZeroCopyBuffer -> ZeroCopyBuffer) (m, zcb) )

    /// Get serialized length of a message
    let inline serializedLength m =
        let nwb = NullWriteBuffer()
        nwb |> serializeTo m |> ignore
        nwb.Length

    /// Get serialized length of a length-delimited message
    let inline serializedLengthLengthDelimited m =
        let len = serializedLength m
        let lenlen = Utility.varIntLenNoDefault (uint64 len)
        (uint32 lenlen) + len

    /// Get serialized length of a length-delimited message
    let inline serializedLengthLD m = serializedLengthLengthDelimited m

    /// Serialize a message, length delimited, into a ZeroCopyBuffer
    let inline serializeLengthDelimitedTo m zcb =
        zcb
        |> WireFormat.Pack.toVarint (uint64 (serializedLength m))
        |> serializeTo m

    /// Serialize a message, length delimited, into a ZeroCopyBuffer
    let inline serializeToLD m zcb = serializeLengthDelimitedTo m zcb


    /// Serialize a message into an ArraySegment backed by a new Array
    let inline serialize m =
        Array.zeroCreate (serializedLength m |> int32 )
        |> ZeroCopyBuffer
        |> serializeTo m
        |> ZeroCopyBuffer.asArraySegment

    /// Serialize a message, length-delimited, into an ArraySegment backed by a new Array
    let inline serializeLengthDelimited m =
        Array.zeroCreate (serializedLengthLD m |> int32 )
        |> ZeroCopyBuffer
        |> serializeToLD m
        |> ZeroCopyBuffer.asArraySegment

    /// Serialize a message, length-delimited, into an ArraySegment backed by a new Array
    let inline serializeLD m = serializeLengthDelimited m


(* Deserialize *)

    module Helpers =

        let inline decodeFixup (m:^msg) =
            (^msg : (static member DecodeFixup : ^msg -> ^msg) (m) )

        let inline deserializeFields m fields =

            let inline decoderRing (m:^msg) =
                (^msg : (static member DecoderRing: Map<int,^msg -> RawField -> ^msg>) () )
    
            let inline requiredFields (m:^msg) =
                (^msg : (static member RequiredFields: Set<FieldNum>) () )

            let inline foundFields (m:^msg) =
                (^msg : (static member FoundFields: ^msg -> Set<FieldNum>) (m) )

            let inline rememberFound (m:^msg) fieldNum =
                (^msg : (static member RememberFound : ^msg -> FieldNum -> ^msg) (m,fieldNum) )

            let inline fetchDecoder decoderRing (field:RawField) =
                let decode decoder m (rawField:RawField) =
                    let m = rememberFound m (rawField.FieldNum)
                    in decoder m rawField

                let n = field.FieldNum
                match decoderRing |> Map.tryFind n with
                | Some(decoder) -> (decode decoder, field)
                | None ->
                    match decoderRing |> Map.tryFind 0 with
                    | Some(decoder) -> (decode decoder, field)
                    | None ->
                        raise <| SerializerException(sprintf "Invalid decoder ring; encountered unknown field '%d' and ring must include an entry for field number 0 to handle unknown fields" n)

            let inline decode decoderRing state fields =
                fields
                |> Seq.map (fetchDecoder decoderRing)
                |> Seq.fold (fun acc (fn, fld) -> fn acc fld) state


            let m = decode (decoderRing m) m fields
            let missingFields = (requiredFields m) - (foundFields m)
            if Set.isEmpty missingFields
            then
                m
            else
                raise <| SerializerException(sprintf "Missing required fields %A" missingFields)

    /// Deserialize a message from a ZeroCopyBuffer, given a default message
    let inline deserializeFrom m zcb =
        zcb
        |> Utility.decodeBuffer
        |> Helpers.deserializeFields m
        |> Helpers.decodeFixup

    /// Deserialize a length-delimited message from a ZeroCopyBuffer, given a default message
    let inline deserializeFromLengthDelimited m zcb =
        zcb
        |> Utility.unpackLengthDelimited
        |> Helpers.deserializeFields m
        |> Helpers.decodeFixup

    /// Deserialize a length-delimited message from a ZeroCopyBuffer, given a default message
    let inline deserializeFromLD m zcb = deserializeFromLengthDelimited m zcb

    /// Deserialize a message from a length-delimited RawField, given a default message
    let inline deserializeFromRawField m (rawField:RawField) =
        let buf = 
            match rawField with
            | LengthDelimited (fieldId, buf) ->
                buf
            | _ ->
                raise <| SerializerException(sprintf "Expected LengthDelimited field, found %s" (rawField.GetType().Name) )
        buf
        |> ZeroCopyBuffer
        |> deserializeFrom m

    /// Deserialize a message from a length-delimited RawField, given a default message,
    /// and return Some(message).  Used to simplify the call-site when deserializing
    /// inner messages.
    let inline deserializeOptionalMessage m rawField =
        Some (deserializeFromRawField m rawField)

    /// Deserialize a message from an ArraySegment, given a default message
    let inline deserialize m (buf:ArraySegment<byte>) =
        buf
        |> ZeroCopyBuffer
        |> deserializeFrom m

    /// Deserialize a length-delimited message from an ArraySegment, given a default message
    let inline deserializeLengthDelimited m (buf:ArraySegment<byte>) =
        buf
        |> ZeroCopyBuffer
        |> deserializeFromLD m

    /// Deserialize a length-delimited message from an ArraySegment, given a default message
    let inline deserializeLD m buf = deserializeLengthDelimited m buf