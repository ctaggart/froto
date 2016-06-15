namespace Froto.Parser

/// Model for parsing Protobufs from C# and other .NET languages
///
/// NOTE: This object model is currently very incomplete, but approximates the
/// original Froto AST model.
///
/// TODO: Expand to expose complete Proto2 and Proto3 language.
module ClassModel =

    open System
    open Froto.Parser
    open Froto.Parser.Ast

    type ProtoFieldRule =
        | Required
        | Optional
        | Repeated

    type ProtoOption (prefix:string option, name:string option, value:string) =
        member val Name = name with get
        member val Value = value with get

    type ProtoField (rule:ProtoFieldRule, tp:string, name:string, position:int32, options:ProtoOption list option) =
        member val Rule = rule with get
        member val Type = tp with get
        member val Name = name with get
        member val Position = position with get
        member val Options = options with get

    type ProtoEnumItem (name:string, value:int32) =
        member val Name = name with get
        member val Value = value with get

    type ProtoEnum (name:string, items:ProtoEnumItem list) =
        member val Name = name with get
        member val Items = items with get

    let internal cvtOpt (ident,v) =
        ProtoOption(None, Some(ident), v.ToString())

    let internal cvtOpts (os:POption list) =
        if not (List.isEmpty os)
        then
            os
            |> List.map cvtOpt
            |> Some
        else None

    let internal cvtType = function
        | TDouble  -> "double" | TFloat -> "float"
        | TInt32   -> "int32"  | TInt64 -> "int64" | TUInt32 -> "uint32" | TUInt64 -> "uint64" | TSInt32 -> "sint32" | TSInt64 -> "sint64"
        | TFixed32 -> "fixed32"| TFixed64 -> "fixed64" | TSFixed32 -> "sfixed32" | TSFixed64 -> "sfixed64"
        | TBool    -> "bool"
        | TString  -> "string" | TBytes -> "bytes"
        | TIdent typeIdent -> typeIdent 

    let internal cvtFields (parts:PMessageStatement list) =
        parts
        |> List.choose (
            function
            | TField (ident, label, ftype, num, opts) ->
                let rule = match label with
                            | TRequired -> Required
                            | TOptional -> Optional
                            | TRepeated -> Repeated
                ProtoField(rule, cvtType ftype, ident, int32 num, cvtOpts opts) 
                |> Some
            | _ -> None )

    let internal cvtEnums (parts:PMessageStatement list) =
        parts
        |> List.choose (
            function
            | TMessageEnum (ident, xs) ->
                let items =
                    xs
                    |> List.choose (
                        function
                        | TEnumOption _ -> None
                        | TEnumField (ident, v, _) -> Some(ProtoEnumItem(ident,v))
                        )
                ProtoEnum(ident, items)
                |> Some
            | _ -> None
            )

    let cvtExtendStmts =
        List.choose (
            function
            | TExtendField(i,l,t,n,os) -> TField(i,l,t,n,os) |> Some
            | _ -> None
            )

    type ProtoMessage (name:string, parts:PMessageStatement list, isExtend:bool) =
        member val Name = name with get
        member val Parts = parts with get
        member val IsExtend = isExtend with get
        member x.Fields     = cvtFields x.Parts
        member x.Enums      = cvtEnums x.Parts
        member x.Messages =
            x.Parts
            |> List.choose (
                function
                | TMessageMessage(ident, stmts) ->
                    ProtoMessage(ident, stmts, false)
                    |> Some
                | _ -> None
                )
        member x.Extends =
            x.Parts
            |> List.choose (
                function
                | TMessageExtend(ident, stmts) ->
                    ProtoMessage(ident, cvtExtendStmts stmts, true)
                    |> Some
                | _ -> None
                )
        member x.Options =
            x.Parts
            |> List.choose (
                function
                | TMessageOption o -> Some(o)
                | _ -> None)
            |> List.map cvtOpt

    type ProtoRpc (name, requestType, responseType) =
        member val Name = name with get
        member val RequestType = requestType with get
        member val ResponseType = responseType with get

    let internal cvtRpc =
        List.choose (
            function
            | TRpc (ident, tReq, _, tResp, _, _)->
                ProtoRpc( ident, tReq, tResp )
                |> Some
            | _ -> None
            )

    type ProtoService (name:string, parts:PServiceStatement list) =
        member val Name = name with get
        member val Parts = parts with get
        member x.Rpcs = cvtRpc x.Parts

    let internal cvtImports =
        List.choose (
            function
            | TImport (name, _) -> Some name
            | _ -> None
            )

    let internal cvtPackages =
        List.choose (
            function
            | TPackage name -> Some name
            | _ -> None
            )

    let internal cvtMessages =
        List.choose (
            function
            | TMessage (ident, stmts) ->
                ProtoMessage(ident, stmts, false)
                |> Some
            | _ -> None
            )

    let internal cvtExtends =
        List.choose (
            function
            | TExtend (ident, stmts) ->
                ProtoMessage(ident, cvtExtendStmts stmts, true)
                |> Some
            | _ -> None
            )

    let internal cvtServices =
        List.choose (
            function
            | TService (ident, stmts) ->
                ProtoService(ident, stmts)
                |> Some
            | _ -> None
        )

    let internal cvtRootEnums =
        List.choose (
            function
            | TEnum (ident, stmts) ->
                let items =
                    stmts
                    |> List.choose (
                        function
                        | TEnumOption _ -> None
                        | TEnumField (ident, v, _) -> Some(ProtoEnumItem(ident,v))
                        )
                ProtoEnum(ident, items)
                |> Some
            | _ -> None
            )

    type ProtoFile (sections:PStatement list) =
        member val Sections = sections with get
        member x.Imports    = cvtImports x.Sections
        member x.Packages   = cvtPackages x.Sections
        member x.Messages   = cvtMessages x.Sections
        member x.Extends    = cvtExtends x.Sections
        member x.Options =
            x.Sections
            |> List.choose (
                function
                | TOption o -> Some(o)
                | _ -> None)
            |> List.map cvtOpt
        member x.Services   = cvtServices x.Sections
        member x.Enums   = cvtRootEnums x.Sections

        static member fromString( s ) =
            let stmts = Parse.fromString s
            ProtoFile(stmts)

        static member fromStream( streamName, stream ) =
            let stmts = Parse.fromStream streamName stream
            ProtoFile(stmts)

        static member fromFile( fileName ) =
            let stmts = Parse.fromFile fileName
            ProtoFile(stmts)
