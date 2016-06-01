namespace Froto.Core

/// Record Serialization support functions
///
module RecordModel =

    open System
    open Froto.Core
    open Froto.Core.Utility

(* Serialize *)
    let inline serialize m zcb =
        let serialize m zcb =
            (^msg : (static member Serializer : ^msg * ZeroCopyBuffer -> ZeroCopyBuffer) (m, zcb) )
        zcb
        |> serialize m

    let inline serializedLength m =
        let serialize m zcb =
            (^msg : (static member Serializer : ^msg * ZeroCopyBuffer -> ZeroCopyBuffer) (m, zcb) )
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
        let lenlen = varIntLenNoDefault (uint64 len)
        (uint32 lenlen) + len

    let inline serializeLengthDelimitedWithUnknowns m unks zcb =
        zcb
        |> WireFormat.encodeVarint (uint64 (serializedLengthWithUnknowns m unks))
        |> serializeWithUnknowns m unks

(* Deserialize *)

    let inline decoderRing (m:^msg) =
        (^msg : (static member DecoderRing: Map<int,^msg -> RawField -> ^msg>) () )
    
    let inline requiredFields (m:^msg) =
        (^msg : (static member RequiredFields: Set<FieldNum>) () )

    let inline foundFields (m:^msg) =
        (^msg : (static member FoundFields: ^msg -> Set<FieldNum>) (m) )

    let inline rememberFound (m:^msg) fieldNum =
        (^msg : (static member RememberFound : ^msg -> FieldNum -> ^msg) (m,fieldNum) )

    let inline decodeFixup (m:^msg) =
        (^msg : (static member DecodeFixup : ^msg -> ^msg) (m) )

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
                raise <| ProtobufSerializerException(sprintf "Invalid decoder ring; encountered unknown field '%d' and ring must include an entry for field number 0 to handle unknown fields" n)

    let inline hydrate decoderRing state fields =
        fields
        |> Seq.map (fetchDecoder decoderRing)
        |> Seq.fold (fun acc (fn, fld) -> fn acc fld) state

    let inline deserializeFields m fields =
        let m = hydrate (decoderRing m) m fields
        let missingFields = (requiredFields m) - (foundFields m)
        if Set.isEmpty missingFields
        then
            m
        else
            raise <| ProtobufSerializerException(sprintf "Missing required fields %A" missingFields)

    let inline deserializeLengthDelimited m zcb =
        zcb
        |> decodeLengthDelimited
        |> deserializeFields m
        |> decodeFixup

    let inline deserialize m zcb =
        zcb
        |> decodeBuffer
        |> deserializeFields m
        |> decodeFixup

    let inline deserializeFromRawField m (rawField:RawField) =
        let buf = 
            match rawField with
            | LengthDelimited (fieldId, buf) ->
                buf
            | _ ->
                raise <| ProtobufSerializerException(sprintf "Expected LengthDelimited field, found %s" (rawField.GetType().Name) )
        buf
        |> ZeroCopyBuffer
        |> deserialize m
