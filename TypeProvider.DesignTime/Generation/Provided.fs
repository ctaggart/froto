[<RequireQualifiedAccess>]
module internal Froto.TypeProvider.Generation.Provided

open System
open System.Reflection
open FSharp.Quotations

open Froto.TypeProvider.Core
open Froto.TypeProvider.Runtime
open Froto.TypeProvider.Runtime.Types
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations

let message name = ProvidedTypeDefinition(name, Some typeof<Message>, isErased = false)

let enum name = ProvidedTypeDefinition(name, Some typeof<obj>, isErased = false)

let addEnumValues (enum: ProvidedTypeDefinition) =
    Seq.map(fun (name, value) ->  ProvidedField.Literal(name, typeof<int>, value))
    >> Seq.iter enum.AddMember

let private backingField propertyType propertyName =
    let field = ProvidedField(Naming.pascalToCamel propertyName, propertyType)
    field.SetFieldAttributes(FieldAttributes.InitOnly ||| FieldAttributes.Private)
    field

let readOnlyProperty propertyType name =
    let field = backingField propertyType name
    let property =
        ProvidedProperty(
            name,
            propertyType,
            getterCode = (fun args -> Expr.FieldGet(args.[0], field)))

    property, field

let readWriteProperty (ty: Type) propertyType name =
    let field = backingField propertyType name
    let property =
        ProvidedProperty(
            name,
            propertyType,
            getterCode = (fun args -> Expr.FieldGet(args.[0], field)),
            setterCode = (fun args ->
                let setter = Expr.FieldSetUnchecked(args.[0], field, args.[1])
                if propertyType.IsValueType ||
                    // None appears to be represented as null.
                    (propertyType.IsGenericType && propertyType.GetGenericTypeDefinition() = typedefof<option<_>>) then
                    setter
                else
                    Expr.Sequential(
                        <@@ Ensure.argNotNull x x@@>
                        |> Expr.getMethodDef
                        |> Expr.callStatic [Expr.Value name; Expr.box args.[1]],
                        setter)))
    property, field

let assembly () = ProvidedAssembly()