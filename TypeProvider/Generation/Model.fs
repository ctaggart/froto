namespace Froto.TypeProvider.Generation

open System
open ProviderImplementation.ProvidedTypes

open Froto.Parser.ClassModel
open Froto.Serialization.Encoding

type ProtobufType = string

type TypeKind = 
    | Primitive
    | Class
    | Enum

type TypeContext = 
    { Kind: TypeKind
      RuntimeType: Type
      ProtobufType: ProtobufType }

    member this.UnderlyingType =
        if this.RuntimeType.IsGenericType
        then this.RuntimeType.GenericTypeArguments.[0]
        else this.RuntimeType

type PropertyDescriptor = 
    { ProvidedProperty: ProvidedProperty
      ProvidedField: ProvidedField option
      Position: FieldNum
      Rule: ProtoFieldRule 
      Type: TypeContext }
        
type OneOfGroupDescriptor =
    { Properties: Map<int, PropertyDescriptor>;
      CaseField: ProvidedField }
      
type MapDescriptor = 
    { KeyType: TypeContext
      ValueType: TypeContext
      Position: FieldNum
      ProvidedProperty: ProvidedProperty
      ProvidedField: ProvidedField }
      
type TypeDescriptor = 
    { Type: ProvidedTypeDefinition;
      Properties: PropertyDescriptor list;
      OneOfGroups: OneOfGroupDescriptor list;
      Maps: MapDescriptor list }
      
    member this.AllProperties =
        this.OneOfGroups
        |> Seq.collect (fun group -> group.Properties |> Map.toSeq)
        |> Seq.map snd
        |> Seq.append this.Properties
        |> List.ofSeq