namespace ProtoTypes.Generation

open System

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Parser.Ast
    
type TypesLookup = Map<string, TypeKind * ProvidedTypeDefinition>

module internal TypeResolver = 
        
    let private getShortName (fullName: string) = fullName.Split('.') |> Seq.last
    
    let rec private allScopes (scope: string) = seq{
        yield scope
        let lowestScopePosition = scope.LastIndexOf(".")
        if lowestScopePosition > 0 
        then yield! scope.Substring(0, lowestScopePosition) |> allScopes
    } 

    let discoverTypes scope (messages: ProtoMessage seq): TypesLookup =
    
        let rec loop scope (message: ProtoMessage) = seq {
            let fullName = scope +.+ message.Name
            yield Class, fullName
            yield! message.Enums |> Seq.map (fun enum -> TypeKind.Enum, fullName +.+ enum.Name)
            yield! message.Messages |> Seq.collect (loop fullName)
        }
        
        messages
        |> Seq.collect (loop scope)
        |> Seq.map (fun (kind, fullName) -> 
            let name = getShortName fullName
            let ty = 
                match kind with
                | Class -> Provided.message name
                | Enum -> Provided.enum name
                | Primitive -> invalidOp <| sprintf "Primitive type '%s' does not require custom Type" fullName
            fullName, (kind, ty))
        |> Map.ofSeq
        
    let resolveScalar = function
        | "double" -> Some typeof<proto_double>
        | "float" -> Some typeof<proto_float>
        | "int32" -> Some typeof<proto_int32>
        | "int64" -> Some typeof<proto_int64>
        | "uint32" -> Some typeof<proto_uint32>
        | "uint64" -> Some typeof<proto_uint64>
        | "sint32" -> Some typeof<proto_sint32>
        | "sint64" -> Some typeof<proto_sint64>
        | "fixed32" -> Some typeof<proto_fixed32>
        | "fixed64" -> Some typeof<proto_fixed64>
        | "sfixed32" -> Some typeof<proto_sfixed32>
        | "sfixed64" -> Some typeof<proto_sfixed64>
        | "bool" -> Some typeof<proto_bool>
        | "string" -> Some typeof<proto_string>
        | "bytes" -> Some typeof<proto_bytes>
        | x -> None
        
    let ptypeToString = function
        | TDouble  -> "double" | TFloat -> "float"
        | TInt32   -> "int32"  | TInt64 -> "int64" | TUInt32 -> "uint32" | TUInt64 -> "uint64" | TSInt32 -> "sint32" | TSInt64 -> "sint64"
        | TFixed32 -> "fixed32"| TFixed64 -> "fixed64" | TSFixed32 -> "sfixed32" | TSFixed64 -> "sfixed64"
        | TBool    -> "bool"
        | TString  -> "string" | TBytes -> "bytes"
        | TIdent typeIdent -> typeIdent 
        
    let resolveNonScalar scope targetType (lookup: TypesLookup) =
        allScopes scope
        |> Seq.map (fun s -> s +.+ targetType)
        |> Seq.map (fun tp -> lookup |> Map.tryFind tp)
        |> Seq.tryFind Option.isSome
        |> Option.unwrap

    let resolve scope targetType (lookup: TypesLookup) = 
        let findInLookup () = 
            resolveNonScalar scope targetType lookup
            |> Option.map (fun (kind, ty) -> kind, ty :> Type)

        resolveScalar targetType 
        |> Option.map (fun t -> Primitive, t)
        |> Option.otherwise findInLookup
        
    let resolvePType scope targetType (lookup: TypesLookup) = 
        resolve scope (ptypeToString targetType) lookup

