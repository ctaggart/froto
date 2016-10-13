module Froto.Parser.Ast

// Note: in the following,
//  A Production is generally prefixed with "P"
//  A Terminal is generally prefixed with "T"

type PProto = PStatement list

and TIdent = string
and TTypeIdent = string

and PStatement =
    | TSyntax of PSyntax
    | TImport of string * PVisibility
    | TPackage of TIdent
    | TOption of POption
    | TMessage of TIdent * PMessageStatement list
    | TEnum of TIdent * PEnumStatement list
    | TExtend of TIdent * PExtendStatement list // proto2
    | TService of TIdent * PServiceStatement list
    with
        override x.ToString() =
            match x with
            | TSyntax s -> sprintf "TSyntax %A" s
            | TImport (s,v) -> sprintf "TImport (\"%s\",%A)" s v
            | TPackage id -> sprintf "TPackage \"%s\"" id
            | TOption (n,v) -> sprintf "TOption (\"%s\",%A)" n v
            | TMessage (id, xs) ->
                sprintf "TMessage (\"%s\",[%s]" id
                    ( xs |> List.map (sprintf "%A")
                         |> List.reduce (sprintf "%s;%s") )
            | TEnum (id, xs) -> sprintf "TEnum (\"%s\",%A)" id xs
            | TExtend (id,xs) -> sprintf "TEnum (\"%s\",%A)" id xs
            | TService (id,xs) -> sprintf "TService (\"%s\",%A)" id xs

// TSyntax
and PSyntax =
    | TProto2
    | TProto3
    with
        override x.ToString() =
            match x with TProto2 -> "Proto2" | TProto3 -> "Proto3"
// TImport
and PVisibility =
    | TNormal
    | TPublic
    | TWeak

// TOption
and POption = TIdent * PConstant

// 
and PConstant =
    | TIntLit of int32
    | TFloatLit of float
    | TBoolLit of bool
    | TStrLit of string
    | TEnumLit of TIdent
    | TAggregateOptionsLit of POption list
    with
        override x.ToString() =
            match x with
            | TIntLit n -> sprintf "%i" n
            | TFloatLit n -> sprintf "%f" n
            | TBoolLit b -> sprintf "%s" (if b then "true" else "false")
            | TStrLit s -> sprintf "\"%s\"" s
            | TEnumLit s -> sprintf "%s" s
            | TAggregateOptionsLit s -> sprintf "%A" s


// TMessage
and PMessageStatement =
    | TField of TIdent * PLabel * PType * FieldNum * POption list
    | TMap of TIdent * PKeyType * PType * FieldNum * POption list
    | TGroup of TIdent * PLabel * FieldNum * PMessageStatement list // proto2
    | TExtensions of TRange list // proto2
    | TReservedRanges of TRange list
    | TReservedNames of TIdent list
    | TOneOf of TIdent * POneOfStatement list

    | TMessageOption of POption
    | TMessageEnum of TIdent * PEnumStatement list
    | TMessageMessage of TIdent * PMessageStatement list
    | TMessageExtend of TIdent * PExtendStatement list // proto2
    with
        override x.ToString() =
            match x with
            | TField (id, lbl, vt, num, opts)   -> sprintf "TField (%s,%A,%A,%u,[%A])" id lbl vt num opts
            | TMap (id, kt, vt, num, opts)      -> sprintf "TMap (%s,%A,%A,%u,[%A])" id kt vt num opts
            | TGroup (id, lbl, num, xs)         -> sprintf "TGroup (%s,%A,%u,%A)" id lbl num xs
            | TExtensions (es)                  -> sprintf "TExtensions %A" es
            | TReservedRanges (rs)              -> sprintf "TReservedRanges %A" rs
            | TReservedNames (rs)               -> sprintf "TReservedNames %A" rs
            | TOneOf (id, xs)                   -> sprintf "TOneOf (%s,%A)" id xs
            | TMessageOption (op)               -> sprintf "TMessageOptions %A" op
            | TMessageEnum (id,xs)              -> sprintf "TMessageEnum (%s,%A)" id xs
            | TMessageMessage (id, xs)          -> sprintf "TMessageMessage (%s,%A)" id xs
            | TMessageExtend (id, xs)           -> sprintf "TMessageExtend (%s,%A)" id xs


and PLabel =
    | TRequired // proto2
    | TOptional
    | TRepeated
    with
        override x.ToString() =
            match x with
            | TRequired -> "TRequired"
            | TOptional -> "TOptional"
            | TRepeated -> "TRepeated"

and PType =
    | TDouble  | TFloat
    | TInt32   | TInt64   | TUInt32   | TUInt64 | TSInt32 | TSInt64
    | TFixed32 | TFixed64 | TSFixed32 | TSFixed64
    | TBool
    | TString | TBytes
    | TIdent of TTypeIdent
    with
        override x.ToString() =
            match x with
            | TDouble -> "TDouble" | TFloat -> "TFloat"
            | TInt32 -> "TInt32" | TInt64 -> "TInt64" | TUInt32 -> "TUnt32" | TUInt64 -> "TInt64" | TSInt32 -> "TSInt32" | TSInt64 -> "TSInt64"
            | TFixed32 -> "TFixed32" | TFixed64 -> "TFixed64" | TSFixed32 -> "TSFixed32" | TSFixed64 -> "TSFixed64"
            | TBool -> "TBool"
            | TString -> "TString" | TBytes -> "TBytes"
            | TIdent s -> sprintf "TIdent \"%s\"" s

and PKeyType =
    | TKInt32   | TKInt64   | TKUInt32   | TKUInt64 | TKSInt32 | TKSInt64
    | TKFixed32 | TKFixed64 | TKSFixed32 | TKSFixed64
    | TKBool
    | TKString
    with
        override x.ToString() =
            match x with
            | TKInt32 -> "TKInt32" | TKInt64 -> "TKInt64" | TKUInt32 -> "TKUInt32" | TKUInt64 -> "TKUInt64" | TKSInt32 -> "TKSInt32" | TKSInt64 -> "TKSInt64"
            | TKFixed32 -> "TKFixed32" | TKFixed64 -> "TKFixed64" | TKSFixed32 -> "TKSFixed32" | TKSFixed64 -> "TKSFixed64"
            | TKBool -> "TKBool"
            | TKString -> "TKString"

and FieldNum = uint32

and PEnumStatement =
    | TEnumOption of POption
    | TEnumField of TIdent * int32 * POption list
    with
        override x.ToString() =
            match x with
            | TEnumOption o -> sprintf "TEnumOption %A" o
            | TEnumField (id, num, opts) -> sprintf "TEnumField (%s,%u,%A)" id num opts

// Extend
and PExtendStatement = // proto2
    | TExtendField of TIdent * PLabel * PType * FieldNum * POption list
    | TExtendGroup of TIdent * PLabel * FieldNum * PMessageStatement list
    with
        override x.ToString() =
            match x with
            | TExtendField(id,lbl,t,num,opts)   -> sprintf "TExtendField (%s,%A,%A,%u,%A)" id lbl t num opts
            | TExtendGroup(id,lbl,num,stmts)    -> sprintf "TExtendGroup (%s,%A,%u,%A)" id lbl num stmts

and TRange = FieldNum * FieldNum option

and POneOfStatement =
    | TOneOfField of TIdent * PType * FieldNum * POption list
    with
        override x.ToString() =
            match x with
            | TOneOfField(id,t,num,opts)        -> sprintf "TOneOfField (%s,%A,%u,%A)" id t num opts

// Service
and PServiceStatement =
    | TServiceOption of POption
    | TRpc of TIdent * TTypeIdent * bool * TTypeIdent * bool * POption list
    with
        override x.ToString() =
            match x with
            | TServiceOption (o)                -> sprintf "TServiceOption %A" o
            | TRpc (id,tReq,bReqStream,tResp,bRespStream,opts)
                                                -> sprintf "TRpc (%s,%s,%b,%s,%b,%A)" id tReq bReqStream tResp bRespStream opts
