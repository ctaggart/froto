namespace Froto.Core.ClassModel

open System
open Froto.Core

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
    member private x.DeserializeField (field:RawField) =
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
