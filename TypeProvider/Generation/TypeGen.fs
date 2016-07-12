[<RequireQualifiedAccess>]
module internal Froto.TypeProvider.Generation.TypeGen

open System
open System.Reflection

open Microsoft.FSharp.Quotations

open Froto.TypeProvider.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.ClassModel
open Froto.Parser.Ast
open Froto.Serialization
    
let private applyRule rule (fieldType: Type) = 
    match rule with
    | Required -> fieldType
    | Optional -> Expr.makeGenericType [fieldType] typedefof<option<_>> 
    | Repeated -> Expr.makeGenericType [fieldType] typedefof<proto_repeated<_>>

let private createProperty scope (lookup: TypesLookup) (field: ProtoField) =

    let typeKind, propertyType = 
        match TypeResolver.resolve scope field.Type lookup with
        | Some(Enum, _) -> TypeKind.Enum, typeof<int>
        | Some(kind, t) -> kind, t
        | None -> invalidOp <| sprintf "Unable to resolve type '%s'" field.Type
    
    let propertyType = applyRule field.Rule propertyType
    let propertyName = Naming.snakeToPascal field.Name

    let property, backingField = 
        match field.Rule with
        | Repeated -> Provided.readOnlyProperty propertyType propertyName
        | _ -> Provided.readWriteProperty propertyType propertyName

    { ProvidedProperty = property
      ProvidedField = Some backingField
      Position = field.Position
      Rule = field.Rule
      Type = 
        { ProtobufType = field.Type
          Kind = typeKind
          RuntimeType = propertyType } }

let private createSerializeMethod typeInfo =
    let serialize =
        ProvidedMethod(
            "Serialize",
            [ ProvidedParameter("buffer", typeof<ZeroCopyBuffer>) ],
            typeof<Void>,
            InvokeCode = (fun args -> Serialization.serializeExpr typeInfo args.[1] args.[0]))

    serialize.SetMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.Public)

    serialize
    
let private createReadFromMethod typeInfo = 
    let readFrom = 
        ProvidedMethod(
            "LoadFrom",
            [ProvidedParameter("buffer", typeof<ZeroCopyBuffer>)],
            typeof<Void>,
            InvokeCode = (fun args -> Deserialization.readFrom typeInfo args.[0] args.[1]))

    readFrom.SetMethodAttrs(MethodAttributes.Virtual)

    readFrom
    
let private createDeserializeMethod targetType =
    let deserializeMethod = 
        ProvidedMethod(
            "Deserialize", 
            [ProvidedParameter("buffer", typeof<ZeroCopyBuffer>)], 
            targetType,
            InvokeCode = 
                (fun args -> Expr.callStaticGeneric [targetType] [args.[0]] <@@ Codec.deserialize<Dummy> x @@>))
            
    deserializeMethod.SetMethodAttrs(MethodAttributes.Static ||| MethodAttributes.Public)
    
    deserializeMethod

let private createEnum scope lookup (enum: ProtoEnum) =
    let _, providedEnum = 
        TypeResolver.resolveNonScalar scope enum.Name lookup
        |> Option.require (sprintf "Enum '%s' is not defined" enum.Name)
    
    enum.Items
    |> Seq.map (fun item -> Naming.upperSnakeToPascal item.Name, item.Value)
    |> Provided.addEnumValues providedEnum
    
    providedEnum

let private createMap scope typesLookup (name: string) (keyTy: PKeyType) (valueTy: PType) position =
    let keyTypeName = 
        match keyTy with 
        | TKInt32 -> TInt32 | TKInt64 -> TInt64 
        | TKUInt32 -> TUInt32 | TKUInt64 -> TUInt64 
        | TKSInt32 -> TSInt32 | TKSInt64 -> TSInt64
        | TKFixed32 -> TFixed32 | TKFixed64 -> TFixed64
        | TKSFixed32 -> TSFixed32 | TKSFixed64 -> TSFixed64
        | TKBool -> TBool
        | TKString -> TString
        |> TypeResolver.ptypeToString
        
    let valueTypeName = TypeResolver.ptypeToString valueTy
    let valueTypeKind, valueType = 
        TypeResolver.resolve scope valueTypeName typesLookup 
        |> Option.require (sprintf "Can't resolve type '%s'" valueTypeName)
        |> function
            | Enum, _ -> TypeKind.Enum, typeof<proto_int32>
            | kind, ty -> kind, ty
    
    let mapType = 
        Expr.makeGenericType 
            [ TypeResolver.resolveScalar keyTypeName |> Option.require (sprintf "Can't resolve scalar type '%s'" keyTypeName); 
                valueType]
            typedefof<proto_map<_, _>>
            
    let property, field = Provided.readOnlyProperty mapType <| Naming.snakeToPascal name
    
    let descriptor = 
        { KeyType = keyTypeName;
            ValueType = valueTypeName;
            ValueTypeKind = valueTypeKind;
            Position = position;
            Property = property }

    descriptor, field

let rec createType scope (lookup: TypesLookup) (message: ProtoMessage) = 
    try
        let _, providedType = 
            TypeResolver.resolveNonScalar scope message.Name lookup 
            |> Option.require (sprintf "Type '%s' is not defined" message.Name)
        
        let nestedScope = scope +.+ message.Name
        message.Enums |> Seq.map (createEnum nestedScope lookup) |> Seq.iter providedType.AddMember
        message.Messages |> Seq.map (createType nestedScope lookup) |> Seq.iter providedType.AddMember

        let properties = message.Fields |> List.map (createProperty nestedScope lookup)

        for prop in properties do
            providedType.AddMember prop.ProvidedProperty
            providedType.AddMember prop.ProvidedField.Value

        let oneOfGroups = 
            message.Parts 
            |> Seq.choose (fun x -> match x with | TOneOf(name, members) -> Some((name, members)) | _ -> None)
            |> Seq.map (fun (name, members) -> OneOf.generateOneOf nestedScope lookup name members)
            |> Seq.fold (fun all (info, members) -> providedType.AddMembers members; info::all) []
            
        let maps, mapFields = 
            message.Parts
            |> Seq.choose (fun x -> 
                match x with 
                | TMap(name, keyTy, valueTy, position) -> Some <| createMap nestedScope lookup name keyTy valueTy (int position) 
                | _ -> None)
            |> Seq.fold (fun (maps, fields) (map, field) -> 
                providedType.AddMember field
                providedType.AddMember map.Property
                map::maps, field::fields) 
                ([], [])

        let ctor = ProvidedConstructor([], InvokeCode = fun args ->
            properties
            |> Seq.filter (fun prop -> prop.Rule = Repeated)
            |> Seq.map (fun prop -> prop.ProvidedField.Value)
            |> Seq.append mapFields
            |> Seq.map (fun field -> 
                Expr.FieldSet(args.[0], field, Expr.callStaticGeneric [field.FieldType] [] <@@ create<_>() @@>))
            |> Expr.sequence)

        providedType.AddMember ctor

        let typeInfo = { Type = providedType; Properties = properties; OneOfGroups = oneOfGroups; Maps = maps }

        let serializeMethod = createSerializeMethod typeInfo
        providedType.AddMember serializeMethod
        providedType.DefineMethodOverride(serializeMethod, typeof<Message>.GetMethod("Serialize"))
        
        let readFromMethod = createReadFromMethod typeInfo
        providedType.AddMember readFromMethod
        providedType.DefineMethodOverride(readFromMethod, typeof<Message>.GetMethod("ReadFrom"))
        
        providedType.AddMember <| createDeserializeMethod providedType
        
        providedType
    with
    | ex ->
        printfn "An error occurred while generating type for message %s: %O" message.Name ex
        reraise()

/// For the given package e.g. "foo.bar.baz.abc" creates a hierarchy of nested types Foo.Bar.Baz.Abc 
/// and returns pair of the first and last types in the hirarchy, Foo and Abc in this case
let createNamespaceContainer (package: string) =

    let rec loop names (current: ProvidedTypeDefinition) =
        match names with
        | [] -> current
        | x::xs -> 
            let nested = ProvidedTypeDefinition(Naming.snakeToPascal x, Some typeof<obj>, IsErased = false)
            current.AddMember nested
            loop xs nested
            
    match package.Split('.') |> List.ofArray with
    | rootName::rest ->
        let root = ProvidedTypeDefinition(Naming.snakeToPascal rootName, Some typeof<obj>, IsErased = false)
        let deepest = loop rest root
        root, deepest
    | _ -> invalidArg "package" "Package name cannot be empty."