namespace TestSerializer

open Xunit
open FsUnit.Xunit

open System
open Froto.Encoding
open Froto.Encoding.Encoders

[<Xunit.Trait("Kind", "Unit")>]
module ClassSerialization =

    open Froto.Serialization

    let toArray (seg:ArraySegment<'a>) =
        seg.Array.[ seg.Offset .. (seg.Count-1) ]

    type InnerMessage () =
        member val Id = 0 with get,set
        member val Name = "" with get,set
        member val _FoundFields = Set.empty with get,set

        member x.Clear() =
            x.Id <- 0
            x.Name <- ""

        static member Serializer (m:InnerMessage, zcb) =
            (m.Id             |> encodeVarint 1) >>
            (m.Name           |> encodeString 2)
            <| zcb

        static member DecoderRing = 
            [
                1, fun (m:InnerMessage) rawField -> m.Id          <- decodeInt32 rawField ; m
                2, fun (m:InnerMessage) rawField -> m.Name        <- decodeString rawField ; m
            ]
            |> Map.ofList

        static member RememberFound (m:InnerMessage, found) =
            m._FoundFields <- m._FoundFields.Add( found )
            m

        static member DecodeFixup (m:InnerMessage) =
            m

        static member RequiredFields =
            [ 1; 2 ] |> Set.ofList

        static member FoundFields (m:InnerMessage) =
            m._FoundFields

        static member UnknownFields (m:InnerMessage) =
            List.empty

        member m.Serialize () =
            Array.zeroCreate (serializedLength m |> int32)
            |> ZeroCopyBuffer
            |> serialize m
            |> ZeroCopyBuffer.asArraySegment

        member m.SerializeLengthDelimited () =
            Array.zeroCreate (serializedLengthDelimitedLength m |> int32)
            |> ZeroCopyBuffer
            |> serializeLengthDelimited m

        static member Deserialize (buf:ArraySegment<byte>) =
            buf
            |> ZeroCopyBuffer
            |> deserialize (InnerMessage())

        static member Deserialize (rawField:RawField) =
            rawField
            |> deserializeFromRawField (InnerMessage())

        static member DeserializeLengthDelimited (buf:ArraySegment<byte>) =
            buf
            |> ZeroCopyBuffer
            |> deserializeLengthDelimited (InnerMessage())


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
            |] |> ArraySegment
        let msg = InnerMessage.Deserialize(buf)
        msg.Id |> should equal 99
        msg.Name |> should equal "Test message"

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
            |] |> ArraySegment
        fun () -> InnerMessage.Deserialize(buf) |> ignore
        |> should throw typeof<Froto.Encoding.ProtobufSerializerException>

    [<Fact>]
    let ``Serialize simple message`` () =
        let msg = InnerMessage()
        msg.Id <- 98
        msg.Name <- "ABC0"
        msg.Serialize()
        |> toArray
        |> should equal
            [|
                0x01uy<<<3 ||| 0uy; // tag: id=1; varint
                98uy;               // value 98
                0x02uy<<<3 ||| 2uy  // tag: id=2; length delim
                4uy;                // length = 4
                0x41uy; 0x42uy; 0x43uy; 0x30uy
                                    // value "ABC0"
            |]

    type OuterMessage () =
        member val Id        = 0 with get,set
        member val Inner     = None with get,set
        member val HasMore   = false with get,set
        member val _FoundFields = Set.empty with get,set

        member x.Clear() =
            x.Id <- 0
            x.Inner <- None
            x.HasMore <- false

        static member Serializer (m:OuterMessage, zcb) =
            (m.Id         |> encodeVarint 1) >>
            (m.Inner      |> encodeOptionalMessage serializeLengthDelimited 42) >>
            (m.HasMore    |> encodeBool 43)
            <| zcb

        static member DecoderRing =
            [ 1 , fun (m:OuterMessage) rawField -> m.Id      <- decodeInt32 rawField; m
              42, fun (m:OuterMessage) rawField ->
                    let o = InnerMessage.Deserialize(rawField) |> Some
                    m.Inner <- o
                    m
              43, fun (m:OuterMessage) rawField -> m.HasMore <- decodeBool rawField; m
            ]
            |> Map.ofList

        static member RememberFound (m:OuterMessage, found) =
            m._FoundFields <- m._FoundFields.Add( found )
            m

        static member DecodeFixup (m:OuterMessage) =
            m

        static member RequiredFields =
            Set.empty

        static member FoundFields (m:OuterMessage) =
            m._FoundFields

        static member UnknownFields (m:OuterMessage) =
            List.empty

        member m.Serialize () =
            Array.zeroCreate (serializedLength m |> int32)
            |> ZeroCopyBuffer
            |> serialize m
            |> ZeroCopyBuffer.asArraySegment

        member m.SerializeLengthDelimited () =
            Array.zeroCreate (serializedLengthDelimitedLength m |> int32)
            |> ZeroCopyBuffer
            |> serializeLengthDelimited m

        static member Deserialize (buf:ArraySegment<byte>) =
            buf
            |> ZeroCopyBuffer
            |> deserialize (OuterMessage())

        static member DeserializeLengthDelimited (buf:ArraySegment<byte>) =
            buf
            |> ZeroCopyBuffer
            |> deserializeLengthDelimited (InnerMessage())

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
            |] |> ArraySegment
        let msg = OuterMessage.Deserialize(buf)
        msg.Id |> should equal 21
        msg.Inner.IsSome |> should equal true
        msg.Inner.Value.Id |> should equal 99
        msg.Inner.Value.Name |> should equal "Test message"
        
    [<Fact>]
    let ``Serialize compound message`` () =
        let msg = OuterMessage()
        msg.Inner <- Some(InnerMessage())
        msg.Id <- 5
        msg.Inner.Value.Id <- 6
        msg.Inner.Value.Name <- "ABC0"
        msg.HasMore <- true
        msg.Serialize()
        |> toArray
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

[<Xunit.Trait("Kind", "Unit")>]
module RecordSerialization =

    open Froto.Serialization

    let toArray (seg:ArraySegment<'a>) =
        seg.Array.[ seg.Offset .. (seg.Count-1) ]

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

            member m.Serialize () =
                Array.zeroCreate (serializedLength m |> int32)
                |> ZeroCopyBuffer
                |> serialize m
                |> ZeroCopyBuffer.asArraySegment

            member m.SerializeLengthDelimited () =
                Array.zeroCreate (serializedLengthDelimitedLength m |> int32)
                |> ZeroCopyBuffer
                |> serializeLengthDelimited m

            static member Deserialize (buf:ArraySegment<byte>) =
                buf
                |> ZeroCopyBuffer
                |> deserialize InnerMessage.Default

            static member DeserializeLengthDelimited (buf:ArraySegment<byte>) =
                buf
                |> ZeroCopyBuffer
                |> deserializeLengthDelimited InnerMessage.Default
       
            static member Serializer (m, zcb) =
                (m.id            |> encodeVarint 1) >>
                (m.name          |> encodeString 2)
                <| zcb

            static member DecoderRing =
                [
                    0, fun m rawField -> { m with _unknownFields = rawField :: m._unknownFields } : InnerMessage
                    1, fun m rawField -> { m with id = rawField |> decodeInt32 } : InnerMessage
                    2, fun m rawField -> { m with name = rawField |> decodeString } : InnerMessage
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
            |] |> ArraySegment
        let msg = buf |> InnerMessage.Deserialize
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
            |] |> ArraySegment
        fun () -> InnerMessage.Deserialize(buf) |> ignore
        |> should throw typeof<Froto.Encoding.ProtobufSerializerException>

    [<Fact>]
    let ``Serialize simple message`` () =
        let msg = { InnerMessage.Default with
                        id = 98
                        name = "ABC0" }
        msg.Serialize()
        |> toArray
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

            member m.Serialize () =
                Array.zeroCreate (serializedLength m |> int32)
                |> ZeroCopyBuffer
                |> serialize m
                |> ZeroCopyBuffer.asArraySegment

            member m.SerializeLengthDelimited () =
                Array.zeroCreate (serializedLengthDelimitedLength m |> int32)
                |> ZeroCopyBuffer
                |> serializeLengthDelimited m

            static member Deserialize (buf:ArraySegment<byte>) =
                buf
                |> ZeroCopyBuffer
                |> deserialize OuterMessage.Default

            static member DeserializeLengthDelimited (buf:ArraySegment<byte>) =
                buf
                |> ZeroCopyBuffer
                |> deserializeLengthDelimited OuterMessage.Default

            static member Serializer (m, zcb) =
                (m.id         |> encodeVarint 1) >>
                (m.inner      |> encodeOptionalMessage serializeLengthDelimited 42) >>
                (m.hasMore    |> encodeBool 43)
                <| zcb

            static member DecoderRing =
                [ 1 , fun m rawField -> { m with id      = decodeInt32 rawField } : OuterMessage
                  42, fun m rawField -> { m with inner   = Some <| deserializeFromRawField InnerMessage.Default rawField } : OuterMessage
                  43, fun m rawField -> { m with hasMore = decodeBool rawField } : OuterMessage
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
            |] |> ArraySegment
        let msg = buf |> OuterMessage.Deserialize
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
        msg.Serialize()
        |> toArray
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

