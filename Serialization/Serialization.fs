namespace Froto.Serialization

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
/// For proto2 Deserializable types must provide, at minimum, the following static
/// methods.
///
///     * DecoderRing   - maps FieldId to a deserialization function
///     * RememberFound - adds a FieldId to list of found fields
///     * DecodeFixup   - called after decoding the message
///     * RequiredFields- list of required fields; can be empty
///     * FoundFields   - returns list of fields found
///
/// If you are using proto3 Deserializable types then you must provide, 
/// at minimum, the following static methods.
///
///     * DecoderRing   - maps FieldId to a deserialization function
///     * DecodeFixup   - called after decoding the message
///
/// The Proto3 module should be used to deserialize rather than functions inside
/// the Deserialize module e.g. 
///    <c>Deserialise.Proto3.fromArray message.Default bytes</c>
///
/// The static methods mentioned above can be associated with any valid F# type, 
/// including Records and Classes, because the serialization functions use
/// Static Type Constraints to locate these static methods.
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

open System
open Froto.Serialization.Encoding

module Serialize =

    module Helpers =
        let arraySegtoArray (seg:ArraySegment<'a>) =
            seg.Array.[ seg.Offset .. (seg.Count-1) ]

    /// Serialize message into a ZeroCopyBuffer.
    let inline toZeroCopyBuffer m zcb =
        (^msg : (static member Serializer : ^msg * ZeroCopyBuffer -> ZeroCopyBuffer) (m, zcb) )

    /// Serialize message into a ZeroCopyBuffer.
    let inline toZcb m zcb = toZeroCopyBuffer m zcb

    /// Get serialized length of a message.
    let inline serializedLength m =
        let nwb = NullWriteBuffer()
        nwb |> toZcb m |> ignore
        nwb.Length

    /// Get serialized length of a length-delimited message.
    let inline serializedLengthLengthDelimited m =
        let len = serializedLength m
        let lenlen = Utility.varIntLenNoDefault (uint64 len)
        (uint32 lenlen) + len

    /// Get serialized length of a length-delimited message.
    /// Shorthand for Serialize.serializedLengthLengthDelimited.
    let inline serializedLengthLD m = serializedLengthLengthDelimited m

    /// Serialize a message, length delimited, into a ZeroCopyBuffer.
    let inline toZeroCopyBufferLengthDelimited m zcb =
        zcb
        |> WireFormat.Pack.toVarint (uint64 (serializedLength m))
        |> toZeroCopyBuffer m

    /// Serialize a message, length delimited, into a ZeroCopyBuffer.
    /// Shorthand for Serialize.toZeroCopyBufferLengthDelimited.
    let inline toZcbLD m zcb = toZeroCopyBufferLengthDelimited m zcb


    /// Serialize a message into an existing ArraySegment.
    /// Note: exception thrown if ArraySegment is not large enough.
    let inline toArraySegment m (arraySeg:ArraySegment<byte>) =
        arraySeg
        |> ZeroCopyBuffer
        |> toZcb m
        |> ZeroCopyBuffer.asArraySegment

    /// Serialize a message, length-delimited, into an existing ArraySegment.
    /// Note: exception thrown if ArraySegment is not large enough.
    let inline toArraySegmentLengthDelimited m (arraySeg:ArraySegment<byte>) =
        arraySeg
        |> ZeroCopyBuffer
        |> toZcbLD m
        |> ZeroCopyBuffer.asArraySegment

    /// Serialize a message, length-delimited, into an existing ArraySegment.
    /// Shorthand for Serialize.toZeroCopyBufferLengthDelimited.
    /// Note: exception thrown if ArraySegment is not large enough.
    let inline toArraySegmentLD m arraySeg = toArraySegmentLengthDelimited m arraySeg

    /// Serialize a message into a new byte array
    let inline toArray m =
        Array.zeroCreate (serializedLength m |> int32 )
        |> ArraySegment
        |> toArraySegment m
        |> Helpers.arraySegtoArray

    /// Serialize a message, length-delimited, into a new byte array.
    let inline toArrayLengthDelimited m =
        Array.zeroCreate (serializedLengthLD m |> int32 )
        |> ArraySegment
        |> toArraySegmentLD m
        |> Helpers.arraySegtoArray

    /// Serialize a message, length-delimited, into a new byte array.
    /// Shorthand for Serialize.toArrayLengthDelimited
    let inline toArrayLD m = toArrayLengthDelimited m


module Deserialize =

    module Helpers =
        module Proto2 =    
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
    
                /// decode a sequence of fields, given a decoder ring and a default or mutable message.
                /// Note that a List would perform slightly better, but demand more memory during operation.
                let inline decode decoderRing m fields =
                    fields
                    |> Seq.map (fetchDecoder decoderRing)
                    |> Seq.fold (fun acc (fn, fld) -> fn acc fld) m
    
    
                let m = decode (decoderRing m) m fields
                let missingFields = (requiredFields m) - (foundFields m)
                if Set.isEmpty missingFields
                then
                    m
                else
                    raise <| SerializerException(sprintf "Missing required fields %A" missingFields)
                
        module Proto3 =
            let inline deserializeFields m fields =
    
                let inline decoderRing (m:^msg) =
                    (^msg : (static member DecoderRing: Map<int,^msg -> RawField -> ^msg>) () )
    
                let inline fetchDecoder decoderRing (field:RawField) =
                    let decode decoder m (rawField:RawField) =
                        decoder m rawField
    
                    let n = field.FieldNum
                    match decoderRing |> Map.tryFind n with
                    | Some(decoder) -> (decode decoder, field)
                    | None ->
                        match decoderRing |> Map.tryFind 0 with
                        | Some(decoder) -> (decode decoder, field)
                        //No action for unknown fields in proto3 if there is no supplied field mapping
                        | None -> (fun m _ -> m), field 
                    
                /// decode a sequence of fields, given a decoder ring and a default or mutable message.
                /// Note that a List would perform slightly better, but demand more memory during operation.
                let inline decode decoderRing m fields =
                    fields
                    |> Seq.map (fetchDecoder decoderRing)
                    |> Seq.fold (fun acc (fn, fld) -> fn acc fld) m
    
                let m = decode (decoderRing m) m fields
                m

    module Shared =
    
        let inline decodeFixup (m:^msg) =
            (^msg : (static member DecodeFixup : ^msg -> ^msg) (m) )
                    
        /// Deserialize a message from a ZeroCopyBuffer, given a default message.
        let inline fromZeroCopyBuffer deserializer m zcb =
            zcb
            |> Utility.decodeBuffer
            |> deserializer m
            |> decodeFixup
    
        /// Deserialize a message from a ZeroCopyBuffer, given a default message.
        /// Shorthand for Deserialize.fromZeroCopyBuffer.
        let inline fromZcb m zcb = fromZeroCopyBuffer m zcb
    
        /// Deserialize a length-delimited message from a ZeroCopyBuffer, given a default message.
        let inline fromZeroCopyBufferLengthDelimited deserializer m zcb =
            zcb
            |> Utility.unpackLengthDelimited
            |> deserializer m
            |> decodeFixup
    
        /// Deserialize a length-delimited message from a ZeroCopyBuffer, given a default message.
        /// Shorthand for Deserialize.fromZeroCopyBufferLengthDelimited.
        let inline fromZcbLD deserializer m zcb = fromZeroCopyBufferLengthDelimited deserializer m zcb
    
        /// Deserialize a message from a length-delimited RawField, given a default message.
        let inline fromRawField deserializer m (rawField:RawField) =
            let buf = 
                match rawField with
                | LengthDelimited (fieldId, buf) ->
                    buf
                | _ ->
                    raise <| SerializerException(sprintf "Expected LengthDelimited field, found %s" (rawField.GetType().Name) )
            buf
            |> ZeroCopyBuffer
            |> fromZeroCopyBuffer deserializer m
    
        /// Deserialize a message from a length-delimited RawField, given a default message,
        /// and return Some(message).  Used to simplify the call-site when deserializing
        /// inner messages.
        let inline optionalMessage deserializer m rawField =
            Some (fromRawField deserializer m rawField)
    
        /// Deserialize a message from an ArraySegment, given a default message.
        let inline fromArraySegment deserializer m (buf:ArraySegment<byte>) =
            buf
            |> ZeroCopyBuffer
            |> fromZcb deserializer m
    
        /// Deserialize a length-delimited message from an ArraySegment, given a default message.
        let inline fromArraySegmentLengthDelimited deserializer m (buf:ArraySegment<byte>) =
            buf
            |> ZeroCopyBuffer
            |> fromZcbLD deserializer m
    
        /// Deserialize a length-delimited message from an ArraySegment, given a default message.
        /// Shorthand for Deserialize.fromArraySegmentLengthDelimited.
        let inline fromArraySegmentLD deserializer m buf = fromArraySegmentLengthDelimited deserializer m buf
    
        /// Deserialize a message from a byte array, given a default message.
        let inline fromArray deserializer m buf =
            buf
            |> ArraySegment
            |> fromArraySegment deserializer m
    
        /// Deserialize a length-delimited message from a byte array, given a default message.
        let inline fromArrayLengthDelimited deserializer m buf =
            buf
            |> ArraySegment
            |> fromArraySegmentLD deserializer m
    
        /// Deserialize a length-delimited message from a byte array, given a default message.
        /// Shorthand for Deserialize.fromArrayLengthDelimited.
        let inline fromArrayLD deserializer m buf = fromArrayLengthDelimited deserializer m buf
        
    module Proto2 =
        open Helpers.Proto2
        /// Deserialize a message from a ZeroCopyBuffer, given a default message.
        let inline fromZeroCopyBuffer m zcb =
            Shared.fromZeroCopyBuffer deserializeFields m zcb
    
        /// Deserialize a message from a ZeroCopyBuffer, given a default message.
        /// Shorthand for Deserialize.fromZeroCopyBuffer.
        let inline fromZcb m zcb =
            Shared.fromZeroCopyBuffer deserializeFields m zcb
    
        /// Deserialize a length-delimited message from a ZeroCopyBuffer, given a default message.
        let inline fromZeroCopyBufferLengthDelimited m zcb =
            Shared.fromZeroCopyBufferLengthDelimited deserializeFields m zcb
           
        /// Deserialize a length-delimited message from a ZeroCopyBuffer, given a default message.
        /// Shorthand for Deserialize.fromZeroCopyBufferLengthDelimited.
        let inline fromZcbLD m zcb =
            Shared.fromZeroCopyBufferLengthDelimited deserializeFields m zcb
    
        /// Deserialize a message from a length-delimited RawField, given a default message.
        let inline fromRawField m (rawField:RawField) =
            Shared.fromRawField deserializeFields m rawField
    
        /// Deserialize a message from a length-delimited RawField, given a default message,
        /// and return Some(message).  Used to simplify the call-site when deserializing
        /// inner messages.
        let inline optionalMessage m rawField =
            Shared.optionalMessage deserializeFields m rawField
    
        /// Deserialize a message from an ArraySegment, given a default message.
        let inline fromArraySegment m (buf:ArraySegment<byte>) =
            Shared.fromArraySegment deserializeFields m buf
    
        /// Deserialize a length-delimited message from an ArraySegment, given a default message.
        let inline fromArraySegmentLengthDelimited m (buf:ArraySegment<byte>) =
            Shared.fromArraySegmentLengthDelimited deserializeFields m buf
    
        /// Deserialize a length-delimited message from an ArraySegment, given a default message.
        /// Shorthand for Deserialize.fromArraySegmentLengthDelimited.
        let inline fromArraySegmentLD m buf =
            Shared.fromArraySegmentLengthDelimited deserializeFields m buf
    
        /// Deserialize a message from a byte array, given a default message.
        let inline fromArray m buf =
            Shared.fromArray deserializeFields m buf
    
        /// Deserialize a length-delimited message from a byte array, given a default message.
        let inline fromArrayLengthDelimited m buf =
            Shared.fromArrayLengthDelimited deserializeFields m buf
    
        /// Deserialize a length-delimited message from a byte array, given a default message.
        /// Shorthand for Deserialize.fromArrayLengthDelimited.
        let inline fromArrayLD m buf =
            Shared.fromArrayLengthDelimited deserializeFields m buf
    
    module Proto3 =
         open Helpers.Proto3
         /// Deserialize a message from a ZeroCopyBuffer, given a default message.
         let inline fromZeroCopyBuffer m zcb =
             Shared.fromZeroCopyBuffer deserializeFields m zcb
     
         /// Deserialize a message from a ZeroCopyBuffer, given a default message.
         /// Shorthand for Deserialize.fromZeroCopyBuffer.
         let inline fromZcb m zcb =
             Shared.fromZeroCopyBuffer deserializeFields m zcb
     
         /// Deserialize a length-delimited message from a ZeroCopyBuffer, given a default message.
         let inline fromZeroCopyBufferLengthDelimited m zcb =
             Shared.fromZeroCopyBufferLengthDelimited deserializeFields m zcb
            
         /// Deserialize a length-delimited message from a ZeroCopyBuffer, given a default message.
         /// Shorthand for Deserialize.fromZeroCopyBufferLengthDelimited.
         let inline fromZcbLD m zcb =
             Shared.fromZeroCopyBufferLengthDelimited deserializeFields m zcb
     
         /// Deserialize a message from a length-delimited RawField, given a default message.
         let inline fromRawField m (rawField:RawField) =
             Shared.fromRawField deserializeFields m rawField
     
         /// Deserialize a message from a length-delimited RawField, given a default message,
         /// and return Some(message).  Used to simplify the call-site when deserializing
         /// inner messages.
         let inline optionalMessage m rawField =
             Shared.optionalMessage deserializeFields m rawField
     
         /// Deserialize a message from an ArraySegment, given a default message.
         let inline fromArraySegment m (buf:ArraySegment<byte>) =
             Shared.fromArraySegment deserializeFields m buf
     
         /// Deserialize a length-delimited message from an ArraySegment, given a default message.
         let inline fromArraySegmentLengthDelimited m (buf:ArraySegment<byte>) =
             Shared.fromArraySegmentLengthDelimited deserializeFields m buf
     
         /// Deserialize a length-delimited message from an ArraySegment, given a default message.
         /// Shorthand for Deserialize.fromArraySegmentLengthDelimited.
         let inline fromArraySegmentLD m buf =
             Shared.fromArraySegmentLengthDelimited deserializeFields m buf
     
         /// Deserialize a message from a byte array, given a default message.
         let inline fromArray m buf =
             Shared.fromArray deserializeFields m buf
     
         /// Deserialize a length-delimited message from a byte array, given a default message.
         let inline fromArrayLengthDelimited m buf =
             Shared.fromArrayLengthDelimited deserializeFields m buf
     
         /// Deserialize a length-delimited message from a byte array, given a default message.
         /// Shorthand for Deserialize.fromArrayLengthDelimited.
         let inline fromArrayLD m buf =
             Shared.fromArrayLengthDelimited deserializeFields m buf