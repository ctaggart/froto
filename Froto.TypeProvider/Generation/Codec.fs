namespace ProtoTypes.Generation

open System
open System.Collections.Generic

open Froto.Core
open Froto.Core.Encoding

open ProtoTypes.Core

/// Contains helper functions to read/write values to/from ZeroCopyBuffer
[<RequireQualifiedAccess>]
module Codec =

    let private write f (fieldNumber: FieldNum) (buffer: ZeroCopyBuffer) value =
        f fieldNumber value buffer |> ignore 

    let writeDouble: Writer<proto_double> = write Serializer.dehydrateDouble
    let writeFloat: Writer<proto_float> = write Serializer.dehydrateSingle
    let writeInt32: Writer<proto_int32> = write Serializer.dehydrateVarint
    let writeInt64: Writer<proto_int64> = write Serializer.dehydrateVarint
    let writeUInt32: Writer<proto_uint32> = write Serializer.dehydrateVarint
    let writeUInt64: Writer<proto_uint64> = write Serializer.dehydrateVarint
    let writeSInt32: Writer<proto_sint32> = write Serializer.dehydrateSInt32
    let writeSInt64: Writer<proto_sint64> = write Serializer.dehydrateSInt64
    // TODO Maybe should be fixed in Froto?
    let writeFixed32: Writer<proto_fixed32> = fun fieldNumber buffer value -> 
        Serializer.dehydrateDefaultedFixed32 0u fieldNumber value buffer |> ignore
        
    let writeFixed64: Writer<proto_fixed64> = fun fieldNumber buffer value -> 
        Serializer.dehydrateDefaultedFixed64 0UL fieldNumber value buffer |> ignore

    let writeSFixed32: Writer<proto_sfixed32> = write Serializer.dehydrateFixed32
    let writeSFixed64: Writer<proto_sfixed64> = fun fieldNumber buffer value ->
        Serializer.dehydrateDefaultedFixed64 0L fieldNumber value buffer |> ignore
        
    let writeBool: Writer<proto_bool> = write Serializer.dehydrateBool
    let writeString: Writer<proto_string> = write Serializer.dehydrateString
    let writeBytes: Writer<proto_bytes> = write Serializer.dehydrateBytes

    /// Serializes optional field using provided function to handle inner value if present
    let writeOptional (writeInner: Writer<'T>) position buffer value =
            match value with
            | Some(v) -> writeInner position buffer v
            | None -> ()
            
    let writeEmbedded position buffer (value: Message) =
        buffer
        |> WireFormat.encodeTag position WireType.LengthDelimited
        |> WireFormat.encodeVarint (uint64 value.SerializedLength)
        |> value.Serialize
        
    /// Value is expected to be of type option<'T>. It's not possible
    /// to use this type directly in the signature because of type providers limitations.
    /// All optional non-generated types (i.e. primitive types and enums) should be serialized using
    /// more strongly-typed writeOptional function
    let writeOptionalEmbedded<'T when 'T :> Message> : Writer<obj> =
        fun position buffer value ->
            if value <> null 
            then value :?> option<'T> |> Option.get |> writeEmbedded position buffer

    let writeRepeated (writeItem: Writer<'T>) position buffer value =
        value |> Seq.iter (writeItem position buffer)

    let writeRepeatedEmbedded<'T when 'T :> Message> : Writer<obj> =
        fun position buffer value ->
            value :?> list<'T> |> writeRepeated writeEmbedded position buffer
            
    let private writeMap writeKey writeValue convertValue : Writer<proto_map<_, _>> =
        fun position buffer value ->
            let item = new MapItem<_, _>(x, x, writeKey, writeValue)
            for pair in value do
                item.Key <- pair.Key
                item.Value <- convertValue pair.Value
                writeEmbedded position buffer item

    let writePrimitiveMap writeKey writeValue : Writer<proto_map<'Key, 'Value>> =
        fun position buffer value -> writeMap writeKey writeValue id position buffer value

    let writeMessageMap<'Key, 'Value when 'Value :> Message> writeKey : Writer<obj> =
        fun position buffer value -> 
            writeMap 
                writeKey
                writeEmbedded 
                (fun msg -> msg :> Message) 
                position
                buffer 
                (value :?> proto_map<'Key, 'Value>)
                
    let writeEnumMap<'Key> writeKey : Writer<proto_map<'Key, proto_int32>> =
        fun position buffer value ->
            writeMap writeKey writeInt32 id position buffer value

    let private readField<'T> f field = 
        let result = ref Unchecked.defaultof<'T>
        f result field
        !result

    let deserialize<'T when 'T :> Message and 'T : (new: unit -> 'T)> buffer =
        let x = new 'T()
        x.ReadFrom buffer
        x

    let readDouble: Reader<proto_double> = readField Serializer.hydrateDouble
    let readFloat: Reader<proto_float> = readField Serializer.hydrateSingle
    let readInt32: Reader<proto_int32> = readField Serializer.hydrateInt32
    let readInt64: Reader<proto_int64> = readField Serializer.hydrateInt64
    let readUInt32: Reader<proto_uint32> = readField Serializer.hydrateUInt32
    let readUInt64: Reader<proto_uint64> = readField Serializer.hydrateUInt64
    let readSInt32: Reader<proto_sint32> = readField Serializer.hydrateSInt32
    let readSInt64: Reader<proto_sint64> = readField Serializer.hydrateSInt64
    let readFixed32: Reader<proto_fixed32> = readField Serializer.hydrateFixed32
    let readFixed64: Reader<proto_fixed64> = readField Serializer.hydrateFixed64
    let readSFixed32: Reader<proto_sfixed32> = readField Serializer.hydrateSFixed32
    let readSFixed64: Reader<proto_sfixed64> = readField Serializer.hydrateSFixed64
    let readBool: Reader<proto_bool> = readField Serializer.hydrateBool
    let readString: Reader<proto_string> = readField Serializer.hydrateString
    let readBytes: Reader<proto_bytes> = readField Serializer.hydrateBytes >> proto_bytes
    
    let readEmbedded<'T when 'T :> Message and 'T : (new: unit -> 'T)> field = 
        match field with
        | LengthDelimited(_, segment) -> ZeroCopyBuffer segment |> deserialize<'T>
        | _ -> failwithf "Invalid format of the field: %O" field
        
    let readMapElement<'Key, 'Value> (map: proto_concrete_map<_, _>) keyReader (valueReader: Reader<'Value>) field =
        match field with
        | LengthDelimited(_, segment) ->
            let item = MapItem(keyReader, valueReader, x, x)
            item.ReadFrom <| ZeroCopyBuffer segment
            (map :?> proto_concrete_map<'Key, 'Value>).Add(item.Key, item.Value)
        | _ -> failwithf "Invalid format of the field: %O" field

    let readMessageMapElement<'Key, 'Value when 'Value :> Message and 'Value : (new: unit -> 'Value)> (map: obj) keyReader field =
        readMapElement (map :?> proto_concrete_map<'Key, 'Value>) keyReader readEmbedded<'Value> field