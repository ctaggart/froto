
// https://developers.google.com/protocol-buffers/docs/proto

module Froto.ProtoAst

type ProtoFieldRule =
    | Required
    | Optional
    | Repeated

type ProtoField = {
    Rule : ProtoFieldRule;
    Type : string;
    Name : string;
    Position : int32;
    }

type ProtoEnumItem = {
    Name : string
    Value : int32
    }

type ProtoEnum = {
    Name : string;
    Items : ProtoEnumItem list;
    }

type ProtoMessagePart =
    | Field of ProtoField
    | Enum of ProtoEnum

type ProtoMessage = {
    Name : string;
    Parts : ProtoMessagePart list;
    //Nested : ProtoMessage list;
    }

type ProtoSection =
    | Import of string
    | Package of string
    | Message of ProtoMessage

type ProtoFile = 
    ProtoSection list
