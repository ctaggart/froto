namespace ProtoTypes.Generation

open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core.Encoding

type TypeKind = 
    | Primitive
    | Class
    | Enum

type PropertyDescriptor = 
    { ProvidedProperty: ProvidedProperty;
      Position: FieldNum;
      ProtobufType: string;
      Rule: ProtoFieldRule; 
      TypeKind: TypeKind }
      
    member this.UnderlyingType =
        if this.ProvidedProperty.PropertyType.IsGenericType
        then this.ProvidedProperty.PropertyType.GenericTypeArguments.[0]
        else this.ProvidedProperty.PropertyType
        
type OneOfGroupDescriptor =
    { Properties: Map<int, PropertyDescriptor>;
      CaseField: ProvidedField }
      
type MapDescriptor = 
    { KeyType: string;
      ValueType: string; 
      ValueTypeKind: TypeKind;
      Position: FieldNum;
      Property: ProvidedProperty }
      
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