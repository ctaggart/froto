namespace Froto.Core

/// Record Serialization support functions
///
module RecordModel =

    open System
    open Froto.Core
    open Froto.Core.Utility

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
        let lenlen = varIntLenNoDefault (uint64 len)
        (uint32 lenlen) + len

    let inline serializeLengthDelimitedWithUnknowns m unks zcb =
        zcb
        |> WireFormat.encodeVarint (uint64 (serializedLengthWithUnknowns m unks))
        |> serializeWithUnknowns m unks

(* Deserialize *)
    let findDecoders decoderRing decodeUnknown (field:RawField) =
        let n = field.FieldNum
        match decoderRing |> Map.tryFind n with
        | Some(decode) -> (decode, field)
        | None -> (decodeUnknown, field)

    let hydrate decoderRing decodeUnknown msgAcc fields =
        fields
        |> Seq.map (findDecoders decoderRing decodeUnknown)
        |> Seq.fold (fun acc (fn, fld) -> fn acc fld) msgAcc


    let inline deserializeFields (m:^msg when ^msg : (member DecoderRing : Map<int,^msg -> RawField -> ^msg>)) fields =
        let decoderRing = (^msg : (member DecoderRing: Map<int,^msg -> RawField -> ^msg>) (m) )
        let ignoreUnknown m fld = m
        let result =
            hydrate decoderRing ignoreUnknown m fields
        result

    let inline decodeFixup (m:^msg when ^msg : (static member DecodeFixup : ^msg -> ^msg)) =
        let decodeFixup m = (^msg : (static member DecodeFixup : ^msg -> ^msg) (m) )
        decodeFixup m

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


    let inline deserializeFieldsWithUnknowns (m:^msg when ^msg : (member DecoderRing : Map<int,^msg -> RawField -> ^msg>)) fields =
        let decoderRing = (^msg : (member DecoderRing: Map<int,^msg -> RawField -> ^msg>) (m) )
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
            |> decodeLengthDelimited
            |> deserializeFieldsWithUnknowns m
        (decodeFixup m, unks)

    let inline deserializeWithUnknowns m zcb =
        let (m,unks) =
            zcb
            |> decodeBuffer
            |> deserializeFieldsWithUnknowns m
        (decodeFixup m, unks)