
// https://developers.google.com/protocol-buffers/docs/proto

module Froto.ProtoAst

open System

type ProtoFieldRule =
    | Required
    | Optional
    | Repeated

type ProtoFieldOption (name:string, value:string) =
    member val Name = name with get
    member val Value = value with get

type ProtoField (rule:ProtoFieldRule, tp:string, name:string, position:int32, options:ProtoFieldOption list option) =
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

type ProtoMessagePart =
    | Field
    | Enum
    | Message

type ProtoMessage (name:string, parts:(ProtoMessagePart * Object) list) =
    member val Name = name with get
    member val Parts = parts with get
    member x.Fields =
        x.Parts
        |> List.filter (fun (p,_) -> match p with | Field -> true | _ -> false)
        |> List.map (fun (_,o) -> o :?> ProtoField)
    member x.Enums =
        x.Parts
        |> List.filter (fun (p,_) -> match p with | Enum -> true | _ -> false)
        |> List.map (fun (_,o) -> o :?> ProtoEnum)
    member x.Messages =
        x.Parts
        |> List.filter (fun (p,_) -> match p with | Message -> true | _ -> false)
        |> List.map (fun (_,o) -> o :?> ProtoMessage)

type ProtoSection =
//    | Import
    | Package
    | Message

type ProtoFile (sections:(ProtoSection * Object) list) =
    member val Sections = sections with get
//    member x.Imports =
//        x.Sections
//        |> List.filter (fun (s,_) -> match s with | Import -> true | _ -> false)
//        |> List.map (fun (_,o) -> o :?> string)
    member x.Packages =
        x.Sections
        |> List.filter (fun (s,_) -> match s with | Package -> true | _ -> false)
        |> List.map (fun (_,o) -> o :?> string)
    member x.Messages =
        x.Sections
        |> List.filter (fun (s,_) -> match s with | Message -> true | _ -> false)
        |> List.map (fun (_,o) -> o :?> ProtoMessage)