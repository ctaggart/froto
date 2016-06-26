namespace ProtoTypes.Generation

open System
open System.Reflection
open FSharp.Quotations

open Froto.Parser.Model
open Froto.Parser.Ast

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

[<RequireQualifiedAccess>]
module OneOf = 
    
    /// Generates members that represent "oneof" group. Similar to whan current version of C# code generator is doing
    /// https://developers.google.com/protocol-buffers/docs/reference/csharp-generated#oneof
    let generateOneOf scope (typesLookup: TypesLookup) (name: string) (members: POneOfStatement list) = 
    
        let oneofCaseEnum = Provided.enum <| Naming.snakeToPascal name + "OneofCase"

        let numberedMembers = members |> List.mapi (fun i m -> i + 1, m)
        
        ("None", 0) :: (numberedMembers |> List.map (fun (i, TOneOfField(name, _, _, _)) -> Naming.snakeToPascal name, i))
        |> Provided.addEnumValues oneofCaseEnum
        
        let caseField = ProvidedField(Naming.snakeToCamel name + "Case", typeof<int>)
        let caseProperty = ProvidedProperty(Naming.camelToPascal caseField.Name, typeof<int>)
        caseProperty.GetterCode <- fun args -> Expr.FieldGet(args.[0], caseField)

        let valueField = ProvidedField(Naming.snakeToCamel name, typeof<obj>)
        let properties = 
            numberedMembers
            |> List.map (fun (i, TOneOfField(name, ptype, position, _)) -> 
                let kind, propertyType =
                    TypeResolver.resolvePType scope ptype typesLookup
                    |> Option.map (fun (kind, ty) -> kind, Expr.makeGenericType [ty] typedefof<option<_>>)
                    |> Option.require (sprintf "Unable to find type %A" ptype) 
                    
                let property = ProvidedProperty(Naming.snakeToPascal name, propertyType)
                
                property.GetterCode <- fun args -> 
                    Expr.IfThenElse(
                        Expr.equal (Expr.FieldGet(args.[0], caseField)) (Expr.Value(i)),
                        Expr.Coerce(Expr.FieldGet(args.[0], valueField), propertyType),
                        Expr.defaultOf propertyType)

                property.SetterCode <- fun args -> 
                    let value = args.[1]
                    let case = 
                        Expr.IfThenElse(
                            Expr.equal (Expr.box value) (Expr.Value(null)),
                            Expr.Value(0),
                            Expr.Value(i))
                    Expr.Sequential(
                        Expr.FieldSet(args.[0], valueField, Expr.box value),
                        Expr.FieldSet(args.[0], caseField, case))
                        
                let propertyInfo =
                    { ProvidedProperty = property;
                      Position = int position;
                      Rule = Optional;
                      ProtobufType = TypeResolver.ptypeToString ptype;
                      TypeKind = kind }

                i, propertyInfo)

        let clearMethod = ProvidedMethod("Clear" + Naming.snakeToPascal name, [], typeof<Void>)
        clearMethod.InvokeCode <- fun args -> 
            Expr.Sequential(
                Expr.FieldSet(args.[0], caseField, Expr.Value(0)),
                Expr.FieldSet(args.[0], valueField, Expr.Value(null))
            )
        
        let oneOfGroup = 
            { Properties = properties |> Map.ofSeq;
              CaseField = caseField }
        
        let allMembers = 
            [ valueField :> MemberInfo; 
              oneofCaseEnum :> MemberInfo; 
              caseField :> MemberInfo; 
              caseProperty :> MemberInfo;
              clearMethod :> MemberInfo ]
            |> List.append (properties |> List.map (fun (_, p) -> p.ProvidedProperty :> MemberInfo))
            
        oneOfGroup, allMembers