/// Contains helper functions to read/write values to/from ZeroCopyBuffer
[<RequireQualifiedAccess>]
module Froto.TypeProvider.Runtime.Codec

open Froto.Serialization
open Froto.Serialization.Encoding
open Froto.Serialization.Encoding.WireFormat
open Froto.TypeProvider.Runtime.Types

let private write f (fieldNumber: FieldNum) (buffer: ZeroCopyBuffer) value =
    f fieldNumber value buffer |> ignore

let writeDouble: Writer<proto_double> = write Encode.fromDouble
let writeFloat: Writer<proto_float> = write Encode.fromSingle
let writeInt32: Writer<proto_int32> = write Encode.fromVarint
let writeInt64: Writer<proto_int64> = write Encode.fromVarint
let writeUInt32: Writer<proto_uint32> = write Encode.fromVarint
let writeUInt64: Writer<proto_uint64> = write Encode.fromVarint
let writeSInt32: Writer<proto_sint32> = write Encode.fromSInt32
let writeSInt64: Writer<proto_sint64> = write Encode.fromSInt64
let writeFixed32: Writer<proto_fixed32> = write Encode.fromFixed32
let writeFixed64: Writer<proto_fixed64> = write Encode.fromFixed64
let writeSFixed32: Writer<proto_sfixed32> = write Encode.fromSFixed32
let writeSFixed64: Writer<proto_sfixed64> = write Encode.fromSFixed64
let writeBool: Writer<proto_bool> = write Encode.fromBool
let writeString: Writer<proto_string> = write Encode.fromString
let writeBytes: Writer<proto_bytes> = write Encode.fromBytes

/// Serializes optional field using provided function to handle inner value if present
let writeOptional (writeInner: Writer<'T>) position buffer value =
        match value with
        | Some(v) -> writeInner position buffer v
        | None -> ()

let writeEmbedded position buffer (value: Message) =
    if value |> box |> isNull |> not then
        buffer
        |> Pack.toTag position WireType.LengthDelimited
        |> Pack.toVarint (uint64 value.SerializedLength)
        |> value.Serialize

/// Value is expected to be of type option<'T>. It's not possible
/// to use this type directly in the signature because of type providers limitations.
/// All optional non-generated types (i.e. primitive types and enums) should be serialized using
/// more strongly-typed writeOptional function
let writeOptionalEmbedded<'T when 'T :> Message> : Writer<obj> =
    fun position buffer value ->
        if not <| isNull value
        then value :?> option<'T> |> Option.get |> writeEmbedded position buffer

let writeRepeated (writeItem: Writer<'T>) position buffer value =
    value |> Seq.iter (writeItem position buffer)

let writeRepeatedEmbedded<'T when 'T :> Message> : Writer<obj> =
    fun position buffer value ->
        value :?> proto_repeated<'T> |> writeRepeated writeEmbedded position buffer

let private writeMap writeKey writeValue convertValue : Writer<proto_map<_, _>> =
    fun position buffer value ->
        let item = new MapItem<_, _>(Unchecked.defaultof<_>, Unchecked.defaultof<_>, writeKey, writeValue)
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

let deserialize<'T when 'T :> Message and 'T : (new: unit -> 'T)> buffer =
    let x = new 'T()
    x.ReadFrom buffer
    x

let readDouble: Reader<proto_double> = Decode.toDouble
let readFloat: Reader<proto_float> = Decode.toSingle
let readInt32: Reader<proto_int32> = Decode.toInt32
let readInt64: Reader<proto_int64> = Decode.toInt64
let readUInt32: Reader<proto_uint32> = Decode.toUInt32
let readUInt64: Reader<proto_uint64> = Decode.toUInt64
let readSInt32: Reader<proto_sint32> = Decode.toSInt32
let readSInt64: Reader<proto_sint64> = Decode.toSInt64
let readFixed32: Reader<proto_fixed32> = Decode.toFixed32
let readFixed64: Reader<proto_fixed64> = Decode.toFixed64
let readSFixed32: Reader<proto_sfixed32> = Decode.toSFixed32
let readSFixed64: Reader<proto_sfixed64> = Decode.toSFixed64
let readBool: Reader<proto_bool> = Decode.toBool
let readString: Reader<proto_string> = Decode.toString
let readBytes: Reader<proto_bytes> = Decode.toBytes >> proto_bytes

let readEmbedded<'T when 'T :> Message and 'T : (new: unit -> 'T)> field =
    match field with
    | LengthDelimited(_, segment) -> ZeroCopyBuffer segment |> deserialize<'T>
    | _ -> failwithf "Invalid format of the field: %O" field

let readMapElement<'Key, 'Value> (map: proto_map<_, _>) keyReader (valueReader: Reader<'Value>) field =
    match field with
    | LengthDelimited(_, segment) ->
        let item = MapItem(keyReader, valueReader, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
        item.ReadFrom <| ZeroCopyBuffer segment
        (map :?> proto_map<'Key, 'Value>).Add(item.Key, item.Value)
    | _ -> failwithf "Invalid format of the field: %O" field

let readMessageMapElement<'Key, 'Value when 'Value :> Message and 'Value : (new: unit -> 'Value)> (map: obj) keyReader field =
    readMapElement (map :?> proto_map<'Key, 'Value>) keyReader readEmbedded<'Value> field
