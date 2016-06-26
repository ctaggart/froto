namespace ProtoTypes.Generation

open System.Reflection
open FSharp.Quotations

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

[<RequireQualifiedAccess>]
module internal Provided =

    let message name = ProvidedTypeDefinition(name, Some typeof<Message>, IsErased = false)

    let enum name = ProvidedTypeDefinition(name, Some typeof<obj>, IsErased = false)
    
    let addEnumValues (enum: ProvidedTypeDefinition) =
        Seq.map(fun (name, value) ->  ProvidedLiteralField(name, typeof<int>, value))
        >> Seq.iter enum.AddMember
        
    let readOnlyProperty propertyType name =
        let field = ProvidedField(Naming.pascalToCamel name, propertyType)
        field.SetFieldAttributes(FieldAttributes.InitOnly ||| FieldAttributes.Private)
        let property = 
            ProvidedProperty(name, propertyType, GetterCode = (fun args -> Expr.FieldGet(args.[0], field)))

        property, field

    let readWriteProperty propertyType name = 
        let property, field = readOnlyProperty propertyType name
        property.SetterCode <- (fun args -> Expr.FieldSet(args.[0], field, args.[1]))

        property, field

    /// Creates an empty parameterless constructor
    let ctor () = ProvidedConstructor([], InvokeCode = (fun _ -> Expr.Value(())))