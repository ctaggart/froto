namespace ProtoTypes.Generation

open System
open System.Collections.Generic
open FSharp.Quotations

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

/// Contains an implementation of deserialization methods for types generated from ProtoBuf messages
[<RequireQualifiedAccess>]
module Deserialization =

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
        | x -> notsupportedf "Primitive type '%s' is not supported" x

    /// Creates quotation that converts RawField quotation to target property type
    let private deserializeField (property: PropertyDescriptor) (rawField: Expr) =
        match property.TypeKind with
        | Primitive -> Expr.Application(primitiveReader property.ProtobufType, rawField)
        | Enum -> <@@ Codec.readInt32 %%rawField @@>
        | Class -> Expr.callStaticGeneric [property.UnderlyingType] [rawField ] <@@ Codec.readEmbedded<Dummy> x @@> 

    let readFrom (typeInfo: TypeDescriptor) this allFields =
    
        // 1. Declare local vars of type Dictionary<,> for all map fields
        // 2. Declare ResizeArray local variables for all repeated fields
        // 3. Read all fields from given buffer
        // 4. Iterate over fields read: if FieldNum matches field position - 
        //    set corresponding property or add value to the ResizeArray (for repeated fields)
        // 5. Convert ResizeArray to lists and set repeated fields
        // 6. Set properties corresponding to map fields using local Dictionary<,> variables
    
        try
        
        // for repeated rules - map from property to variable
        let resizeArrays =
            typeInfo.AllProperties
            |> Seq.filter (fun prop -> prop.Rule = Repeated)
            |> Seq.map (fun prop -> prop, Var(prop.ProvidedProperty.Name, Expr.makeGenericType [prop.UnderlyingType] typedefof<ResizeArray<_>>))
            |> dict
            
        let dictionaries = 
            typeInfo.Maps
            |> Seq.map (fun map -> 
                map, 
                Var(map.Property.Name, 
                    Expr.makeGenericType 
                        (map.Property.PropertyType.GenericTypeArguments |> List.ofArray) 
                        typedefof<proto_concrete_map<_, _>>))
            |> dict

        let samePosition field idx = <@@ (%%field: RawField).FieldNum = idx @@>

        /// For required and optional fields - set the property directly;
        /// for repeated - add to corresponding ResizeArray
        let handleField (property: PropertyDescriptor) (field: Expr) =
            let value = deserializeField property field
            
            match property.Rule with
            | Repeated -> 
                let list = Expr.Var(resizeArrays.[property])
                match property.TypeKind with
                | Class ->
                    Expr.callStaticGeneric 
                        [list.Type.GenericTypeArguments.[0]]
                        [Expr.box list; value]
                        <@@ ResizeArray.add x x @@>
                | _ ->
                    let addMethod = list.Type.GetMethod("Add")
                    Expr.Call(list, addMethod, [value])
            | Optional ->
                let someValue = Expr.callStaticGeneric [value.Type] [value] <@@ Option.some x @@> 
                Expr.PropertySet(this, property.ProvidedProperty, someValue)
            | Required ->
                Expr.PropertySet(this, property.ProvidedProperty, value)
                
        let handleMap map field = 
            let dict = Expr.Var(dictionaries.[map])
            let keyReader = primitiveReader map.KeyType

            let readMethod, args =
                match map.ValueTypeKind with
                | Primitive -> 
                    <@@ Codec.readMapElement x x x x @@>,
                    [dict; keyReader; primitiveReader map.ValueType; field]
                | Enum -> 
                    <@@ Codec.readMapElement x x x x @@>,
                    [dict; keyReader; <@@ Codec.readInt32 @@>; field]
                | Class -> 
                        <@@ Codec.readMessageMapElement<_, Dummy> x x x @@>,
                        [ Expr.box dict; keyReader; field ]
                        
            Expr.callStaticGeneric
                (dict.Type.GenericTypeArguments |> List.ofArray)
                args
                readMethod

        /// Converts ResizeArray to immutable list and sets corresponding repeated property
        let setRepeatedProperty property (resizeArrayVar: Var) =
            let itemTy = resizeArrayVar.Type.GenericTypeArguments.[0]
            let list = 
                match property.TypeKind with
                | Class ->
                    Expr.callStaticGeneric 
                        [itemTy] 
                        [Expr.box <| Expr.Var(resizeArrayVar)]
                        <@@ ResizeArray.toList x @@> 
                | _ -> 
                    Expr.callStaticGeneric [itemTy] [Expr.Var(resizeArrayVar)] <@@ List.ofSeq<_> x @@>

            Expr.PropertySet(this, property.ProvidedProperty, list)
            
        let setMapProperty map content = 
            Expr.PropertySet(this, map.Property, Expr.Var(content))

        let fieldLoop = Expr.forLoop <@@ ZeroCopyBuffer.allFields %%allFields @@> (fun field ->
            let maps = 
                typeInfo.Maps
                |> Seq.fold 
                    (fun acc prop ->
                        Expr.IfThenElse(
                            samePosition field prop.Position,
                            handleMap prop field,
                            acc
                        ))
                    (Expr.Value(()))

            typeInfo.AllProperties
            |> Seq.fold
                (fun acc prop ->
                    Expr.IfThenElse(
                        samePosition field prop.Position,
                        handleField prop field,
                        acc))
                maps)

        let setRepeatedFields =
            resizeArrays
            |> Seq.map (fun pair -> setRepeatedProperty pair.Key pair.Value)
            |> List.ofSeq
            
        let setMapFields = 
            dictionaries
            |> Seq.map (fun pair -> setMapProperty pair.Key pair.Value)
            |> List.ofSeq

        let create ty = Expr.callStaticGeneric [ty] [] <@@ create<_>() @@>
        
        let body = fieldLoop :: setRepeatedFields @ setMapFields |> Expr.sequence

        let body = 
            resizeArrays.Values
            |> Seq.fold (fun acc var -> Expr.Let(var, create var.Type, acc)) body
        
        dictionaries.Values
        |> Seq.fold (fun acc var -> Expr.Let(var, create var.Type, acc)) body

        with
        | ex ->
           printfn "Failed to generate Deserialize method for type %s. Details: %O" typeInfo.Type.Name ex
           reraise()