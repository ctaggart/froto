/// Contains an implementation of deserialization methods for types generated from ProtoBuf messages
[<RequireQualifiedAccess>]
module internal Froto.TypeProvider.Generation.Deserialization

open FSharp.Quotations

open Froto.TypeProvider.Core
open Froto.TypeProvider.Runtime

open Froto.Parser.ClassModel
open Froto.Serialization.Encoding

let private primitiveReader = function
    | "double" -> <@@ Codec.readDouble @@>
    | "float" -> <@@ Codec.readFloat @@>
    | "int32" -> <@@ Codec.readInt32 @@>
    | "int64" -> <@@ Codec.readInt64 @@>
    | "uint32" -> <@@ Codec.readUInt32 @@>
    | "uint64" -> <@@ Codec.readUInt64 @@>
    | "sint32" -> <@@ Codec.readSInt32 @@>
    | "sint64" -> <@@ Codec.readSInt64 @@>
    | "fixed32" -> <@@ Codec.readFixed32 @@>
    | "fixed64" -> <@@ Codec.readFixed64 @@>
    | "sfixed32" -> <@@ Codec.readSFixed32 @@>
    | "sfixed64" -> <@@ Codec.readSFixed64 @@>
    | "bool" -> <@@ Codec.readBool @@>
    | "string" -> <@@ Codec.readString @@>
    | "bytes" -> <@@ Codec.readBytes @@>
    | x -> raise <| TypeNotSupportedException x

/// Creates quotation that converts RawField quotation to target property type
let private deserializeField (property: PropertyDescriptor) (rawField: Expr) =
    match property.Type.Kind with
    | Primitive -> Expr.Application(primitiveReader property.Type.ProtobufType, rawField)
    | Enum -> <@@ Codec.readInt32 %%rawField @@>
    | Class -> Expr.callStaticGeneric [property.Type.UnderlyingType] [rawField ] <@@ Codec.readEmbedded<Dummy> x @@> 

let private samePosition field idx = <@@ (%%field: RawField).FieldNum = idx @@>

/// Adds key-value pair to the property that corresponds the map 
let private handleMapElement this mapDescriptor field = 
    let map = Expr.PropertyGet(this, mapDescriptor.ProvidedProperty)
    let keyReader = primitiveReader mapDescriptor.KeyType.ProtobufType

    let readMethod, args =
        match mapDescriptor.ValueType.Kind with
        | Primitive -> 
            <@@ Codec.readMapElement x x x x @@>,
            [map; keyReader; primitiveReader mapDescriptor.ValueType.ProtobufType; field]
        | Enum -> 
            <@@ Codec.readMapElement x x x x @@>,
            [map; keyReader; <@@ Codec.readInt32 @@>; field]
        | Class -> 
                <@@ Codec.readMessageMapElement<_, Dummy> x x x @@>,
                [ Expr.box map; keyReader; field ]
                
    Expr.callStaticGeneric
        (map.Type.GenericTypeArguments |> List.ofArray)
        args
        readMethod

let private handleRequired this propertyDescriptor field =
    let value = deserializeField propertyDescriptor field
    Expr.PropertySet(this, propertyDescriptor.ProvidedProperty, value)

let private handleOptional this propertyDescriptor field =
    let value = deserializeField propertyDescriptor field
    let someValue = Expr.callStaticGeneric [value.Type] [value] <@@ Option.some x @@> 
    Expr.PropertySet(this, propertyDescriptor.ProvidedProperty, someValue)

let private handleRepeated this propertyDescriptor field =
    let value = deserializeField propertyDescriptor field
    let list = Expr.PropertyGet(this, propertyDescriptor.ProvidedProperty)
    match propertyDescriptor.Type.Kind with
    | Class ->
        Expr.callStaticGeneric 
            [propertyDescriptor.Type.UnderlyingType]
            [Expr.box list; value]
            <@@ ResizeArray.add x x @@>
    | _ ->
        let addMethod = list.Type.GetMethod("Add")
        Expr.Call(list, addMethod, [value])

let readFrom (typeInfo: TypeDescriptor) this allFields =

    try
        // handlers for all properties, depending on position
        let handlers = 
            typeInfo.AllProperties
            |> Seq.map (fun prop -> 
                match prop.Rule with
                | Required -> prop.Position, handleRequired this prop
                | Optional -> prop.Position, handleOptional this prop
                | Repeated -> prop.Position, handleRepeated this prop)
            |> Seq.append (typeInfo.Maps |> Seq.map(fun map -> map.Position, handleMapElement this map))
            |> List.ofSeq

        Expr.forLoop <@@ ZeroCopyBuffer.allFields %%allFields @@> (fun field ->
            handlers
            |> List.fold
                (fun acc (pos, handler) ->
                    Expr.IfThenElse(
                        samePosition field pos,
                        handler field,
                        acc))
                (Expr.Value(())))

    with
    | ex ->
        printfn "Failed to generate Deserialize method for type %s. Details: %O" typeInfo.Type.Name ex
        reraise()