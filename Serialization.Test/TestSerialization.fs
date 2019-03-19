namespace TestSerialization

open Xunit
open FsUnit.Xunit

open System

open Froto.Serialization
open Froto.Serialization.Encoding

[<Xunit.Trait("Kind", "Unit")>]
module RecordSerialization =

    type InnerMessage  = {
        id : int32
        name : string
        _foundFieldNums : Set<FieldNum>
        _unknownFields : RawField list
        }
        with
            static member Default = {
                id = 0
                name = ""
                _foundFieldNums = Set.empty
                _unknownFields = List.empty
                }

            static member Serializer (m, zcb) =
                (m.id            |> Encode.fromVarint 1) >>
                (m.name          |> Encode.fromString 2)
                <| zcb

            static member DecoderRing =
                [
                    0, fun m rawField -> { m with _unknownFields = rawField :: m._unknownFields } : InnerMessage
                    1, fun m rawField -> { m with id = rawField |> Decode.toInt32 } : InnerMessage
                    2, fun m rawField -> { m with name = rawField |> Decode.toString } : InnerMessage
                ]
                |> Map.ofList

            static member RememberFound (m,found) =
                { m with _foundFieldNums = m._foundFieldNums |> Set.add found }

            static member DecodeFixup m =
                { m with _unknownFields = List.rev m._unknownFields }

            static member RequiredFields =
                [ 1; 2 ] |> Set.ofList

            static member FoundFields m =
                m._foundFieldNums

            static member UnknownFields m =
                m._unknownFields

    [<Fact>]
    let ``Deserialize simple message`` () =
        let buf =
            [|
                0x01uy<<<3 ||| 0uy; // tag: id=1; varint
                99uy;               // value 99
                0x02uy<<<3 ||| 2uy  // tag: id=2; length delim
                12uy;               // length = 12
                0x54uy; 0x65uy; 0x73uy; 0x74uy; 0x20uy; 0x6duy; 0x65uy; 0x73uy; 0x73uy; 0x61uy; 0x67uy; 0x65uy
                                    // value "Test message"
            |]
        let msg = buf |> Deserialize.Proto2.fromArray InnerMessage.Default
        msg.id |> should equal 99
        msg.name |> should equal "Test message"

    [<Fact>]
    let ``Missing required field throws exception`` () =
        let buf =
            [|
//                0x01uy<<<3 ||| 0uy; // tag: id=1; varint
//                99uy;               // value 99
                0x02uy<<<3 ||| 2uy  // tag: id=2; length delim
                12uy;               // length = 12
                0x54uy; 0x65uy; 0x73uy; 0x74uy; 0x20uy; 0x6duy; 0x65uy; 0x73uy; 0x73uy; 0x61uy; 0x67uy; 0x65uy
                                    // value "Test message"
            |]
        fun () -> buf |> Deserialize.Proto2.fromArray InnerMessage.Default |> ignore
        |> should throw typeof<Froto.Serialization.SerializerException>

    [<Fact>]
    let ``Serialize simple message`` () =
        let msg = { InnerMessage.Default with
                        id = 98
                        name = "ABC0" }
        msg
        |> Serialize.toArray
        |> should equal
            [|
                0x01uy<<<3 ||| 0uy; // tag: id=1; varint
                98uy;               // value 98
                0x02uy<<<3 ||| 2uy  // tag: id=2; length delim
                4uy;                // length = 4
                0x41uy; 0x42uy; 0x43uy; 0x30uy
                                    // value "ABC0"
            |]

    type OuterMessage = {
        id : int32
        inner : InnerMessage option
        hasMore : bool
        }
        with
            static member Default = {
                id = 0
                inner = None
                hasMore = false
                }

            static member Serializer (m, zcb) =
                (m.id         |> Encode.fromVarint 1) >>
                (m.inner      |> Encode.fromOptionalMessage Serialize.toZcbLD 42) >>
                (m.hasMore    |> Encode.fromBool 43)
                <| zcb

            static member DecoderRing =
                [ 1 , fun m rawField -> { m with id      = Decode.toInt32 rawField } : OuterMessage
                  42, fun m rawField -> { m with inner   = Deserialize.Proto2.optionalMessage InnerMessage.Default rawField} : OuterMessage
                  43, fun m rawField -> { m with hasMore = Decode.toBool rawField } : OuterMessage
                ]
                |> Map.ofList

            static member RememberFound (m,_) = m
            static member DecodeFixup m = m
            static member RequiredFields = Set.empty
            static member FoundFields _ = Set.empty

    [<Fact>]
    let ``Deserialize compound message`` () =
        let buf =
            [|
                0x01uy<<<3 ||| 0uy;     // tag: fldnum=1, varint
                21uy;                   // value 21
                0xD0uy ||| 2uy; 0x02uy; // tag: fldnum=42, length delim
                16uy;                   // length 16
                0x01uy<<<3 ||| 0uy;     // tag: fldnum=1; varint
                99uy;                   // value 99
                0x02uy<<<3 ||| 2uy;     // tag: fldnum=2; length delim
                12uy;                   // length = 12
                0x54uy; 0x65uy; 0x73uy; 0x74uy; 0x20uy; 0x6duy; 0x65uy; 0x73uy; 0x73uy; 0x61uy; 0x67uy; 0x65uy
                                        // value "Test message"
                0xD8uy ||| 0uy; 0x02uy; // tag: fldnum=43, varint
                0x01uy;                 // value true
            |]
        let msg = buf |> Deserialize.Proto2.fromArray OuterMessage.Default
        msg.id |> should equal 21
        msg.inner.IsSome |> should equal true
        msg.inner.Value.id |> should equal 99
        msg.inner.Value.name |> should equal "Test message"
        
    [<Fact>]
    let ``Serialize compound message`` () =
        let msg = { OuterMessage.Default with
                        id = 5
                        inner = Some { InnerMessage.Default with id=6; name = "ABC0" }
                        hasMore = true }
        msg
        |> Serialize.toArray
        |> should equal
            [|
                0x01uy<<<3 ||| 0uy;     // tag: id=1; varint
                5uy;                    // value 5
                0xD0uy ||| 2uy; 0x02uy; // tag: fldnum=42, length delim
                8uy;                    // length = 8
                0x01uy<<<3 ||| 0uy;     // tag: id=1; varint
                6uy;                    // value 6
                0x02uy<<<3 ||| 2uy;     // tag: id=2; length delim
                4uy;                    // length = 4
                0x41uy; 0x42uy; 0x43uy; 0x30uy;
                                        // value "ABC0"
                0xD8uy ||| 0uy; 0x02uy; // tag: fldnum=43, varint
                0x01uy;                 // value true
            |]
     
    ///If you require the collection of unknown fields in your record type definitions then you can 
    ///use the following definition as a guide.  
    ///Note the _unknownFields definition as part of the record definition, zero entry in the DecoderRing, 
    ///the UnknownFields method and DecodeFixup has to reverse the order of the _unknownFields list.        
    type Proto3Message = {
        id : int32
        name : string
        _unknownFields : RawField list
        }
        with
            static member Default = {
                id = 0
                name = ""
                _unknownFields = List.empty
                }

            static member Serializer (m, zcb) =
                (m.id            |> Encode.fromVarint 1) >>
                (m.name          |> Encode.fromString 2) >>
                (m._unknownFields|> Encode.fromRawFields)
                <| zcb

            static member DecoderRing =
                [
                    0, fun m rawField -> { m with _unknownFields = rawField :: m._unknownFields } : Proto3Message
                    1, fun m rawField -> { m with id = rawField |> Decode.toInt32 } : _
                    2, fun m rawField -> { m with name = rawField |> Decode.toString } : _
                ]
                |> Map.ofList
    
            static member DecodeFixup m: Proto3Message =
                { m with _unknownFields = List.rev m._unknownFields }

            static member UnknownFields m =
                m._unknownFields
                
    [<Fact>]
    let ``Serialize proto3 message`` () =
        let msg = { Proto3Message.Default with
                        id = 5
                        name = "TEST" }
        let serialised = msg |> Serialize.toArray
        printfn "%A" serialised
        serialised
        |> should equal
            [|  
                1uy <<< 3 ||| 0uy; 5uy // field 1, type 0; value 5
                2uy <<< 3 ||| 2uy;     // field 2, type 2
                4uy;                   // length
                84uy; 69uy; 83uy; 84uy // TEST
            |]
            
    [<Fact>]
    let ``Deserialize proto3 message`` () =
        let buf =
            [|
                1uy <<< 3 ||| 0uy; 5uy // field 1, type 0; value 5
                2uy <<< 3 ||| 2uy;     // field 2, type 2
                4uy;                   // length
                84uy; 69uy; 83uy; 84uy // TEST
            |]
        let msg = buf |> Deserialize.Proto3.fromArray Proto3Message.Default
        msg.id |> should equal 5
        msg.name |> should equal "TEST"
        
    [<Fact>]
    let ``Deserialize proto3 message with extra field`` () =
        let buf =
            [|
                1uy <<< 3 ||| 0uy; 5uy  // field 1, type 0; value 5
                2uy <<< 3 ||| 2uy;      // field 2, type 2
                4uy;                    // length
                84uy; 69uy; 83uy; 84uy  // TEST
                3uy <<< 3 ||| 0uy; 42uy //extra field number 3, type varint, value 42
            |]
        let msg = buf |> Deserialize.Proto3.fromArray Proto3Message.Default
        msg.id |> should equal 5
        msg.name |> should equal "TEST"
        let unknown =
            msg._unknownFields
            |> List.tryHead
            |> Option.toList
            |> List.map (fun v -> v.FieldNum, v.WireType)
        unknown |> should equal [3, WireType.Varint]
     
    ///If you dont require the optional collection of Unknown Fields then your record type definitions
    ///can be described like the following.  
    ///Note the absence of a zero entry in the DecoderRing, no UnknownFields and DecodeFixup has no work to do.
    type Proto3MessageNoUnknown = {
        id : int32
        name : string
        }
        with
            static member Default = {
                id = 0
                name = ""
                }

            static member Serializer (m, zcb) =
                (m.id            |> Encode.fromVarint 1) >>
                (m.name          |> Encode.fromString 2)
                <| zcb

            static member DecoderRing =
                [
                    1, fun m rawField -> { m with id = rawField |> Decode.toInt32 } : Proto3MessageNoUnknown
                    2, fun m rawField -> { m with name = rawField |> Decode.toString } : _
                ]
                |> Map.ofList
    
            static member DecodeFixup m = m

    [<Fact>]
    let ``Deserialize proto3 message discarding extra field`` () =
        let buf =
            [|
                1uy <<< 3 ||| 0uy; 5uy  // field 1, type 0; value 5
                2uy <<< 3 ||| 2uy;      // field 2, type 2
                4uy;                    // length
                84uy; 69uy; 83uy; 84uy  // TEST
                3uy <<< 3 ||| 0uy; 42uy //extra field number 3, type varint, value 42
            |]
        let msg = buf |> Deserialize.Proto3.fromArray Proto3MessageNoUnknown.Default
        msg.id |> should equal 5
        msg.name |> should equal "TEST"
