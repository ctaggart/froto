[<RequireQualifiedAccess>]
module internal Froto.TypeProvider.Generation.Provided

open System
open System.IO
open System.Reflection
open FSharp.Quotations

open Froto.TypeProvider.Core
open ProviderImplementation.ProvidedTypes

let message name = ProvidedTypeDefinition(name, Some typeof<Message>, IsErased = false)

let enum name = ProvidedTypeDefinition(name, Some typeof<obj>, IsErased = false)

let addEnumValues (enum: ProvidedTypeDefinition) =
    Seq.map(fun (name, value) ->  ProvidedLiteralField(name, typeof<int>, value))
    >> Seq.iter enum.AddMember
    
let readOnlyProperty propertyType name =
    let field = ProvidedField(Naming.pascalToCamel name, propertyType)
    field.SetFieldAttributes(FieldAttributes.InitOnly ||| FieldAttributes.Private)
    let property = 
        ProvidedProperty(
            name, 
            propertyType, 
            GetterCode = (fun args -> Expr.FieldGet(args.[0], field)))

    property, field

let readWriteProperty (ty: Type) propertyType name = 
    let property, field = readOnlyProperty propertyType name
    property.SetterCode <- (fun args ->
        let setter = Expr.FieldSet(args.[0], field, args.[1])
        if propertyType.IsValueType || 
            // None appears to be represented as null.
            (propertyType.IsGenericType && propertyType.GetGenericTypeDefinition() = typedefof<option<_>>) then
            setter
        else
            Expr.Sequential(
                <@@ Ensure.argNotNull x x@@>
                |> Expr.getMethodDef
                |> Expr.callStatic [Expr.Value property.Name; Expr.box args.[1]],
                setter))

    property, field

let assembly () =
    Path.ChangeExtension(Path.GetTempFileName(), ".dll") |> ProvidedAssembly