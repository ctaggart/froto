namespace Froto.Serialization

/// Serialization support functions
///
module Serializer =

    open Froto.Serialization.Encoding

(* Serialize *)

    let inline serialize m zcb =
        (^msg : (static member Serializer : ^msg * ZeroCopyBuffer -> ZeroCopyBuffer) (m, zcb) )

    let inline serializedLength m =
        let nwb = NullWriteBuffer()
        nwb |> serialize m |> ignore
        nwb.Length

    let inline serializedLengthDelimitedLength m =
        let len = serializedLength m
        let lenlen = Utility.varIntLenNoDefault (uint64 len)
        (uint32 lenlen) + len

    let inline serializeLengthDelimited m zcb =
        zcb
        |> WireFormat.packVarint (uint64 (serializedLength m))
        |> serialize m

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

    let inline deserialize m zcb =
        zcb
        |> Utility.decodeBuffer
        |> Helpers.deserializeFields m
        |> Helpers.decodeFixup

    let inline deserializeLengthDelimited m zcb =
        zcb
        |> Utility.unpackLengthDelimited
        |> Helpers.deserializeFields m
        |> Helpers.decodeFixup

    let inline deserializeFromRawField m (rawField:RawField) =
        let buf = 
            match rawField with
            | LengthDelimited (fieldId, buf) ->
                buf
            | _ ->
                raise <| SerializerException(sprintf "Expected LengthDelimited field, found %s" (rawField.GetType().Name) )
        buf
        |> ZeroCopyBuffer
        |> deserialize m
