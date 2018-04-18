namespace TestSerializer

open Xunit
open FsUnit.Xunit

open System

open Froto.Serialization
open Froto.Serialization.Encoding

[<Xunit.Trait("Kind", "Unit")>]
module ClassSerialization =

    type InnerMessage () =
        member val Id = 0 with get,set
        member val Name = "" with get,set
        member val _FoundFields = Set.empty with get,set

        member x.Clear() =
            x.Id <- 0
            x.Name <- ""

        static member Serializer (m:InnerMessage, zcb) =
            (m.Id             |> Encode.fromVarint 1) >>
            (m.Name           |> Encode.fromString 2)
            <| zcb

        static member DecoderRing = 
            [
                1, fun (m:InnerMessage) rawField -> m.Id          <- Decode.toInt32 rawField ; m
                2, fun (m:InnerMessage) rawField -> m.Name        <- Decode.toString rawField ; m
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

        member m.Serialize ()                   = Serialize.toArray m
        member m.Serialize buf                  = Serialize.toArraySegment m buf
        member m.Serialize zcb                  = Serialize.toZeroCopyBuffer m zcb
        member m.SerializeLengthDelimited ()    = Serialize.toArrayLD m

        static member Deserialize buf                   = buf |> Deserialize.Proto2.fromArray (InnerMessage())
        static member Deserialize buf                   = buf |> Deserialize.Proto2.fromArraySegment (InnerMessage())
        static member Deserialize zcb                   = zcb |> Deserialize.Proto2.fromZeroCopyBuffer (InnerMessage())
        static member Deserialize raw                   = raw |> Deserialize.Proto2.fromRawField (InnerMessage())
        static member DeserializeLengthDelimited buf    = buf |> Deserialize.Proto2.fromArrayLD (InnerMessage())
        static member DeserializeLengthDelimited buf    = buf |> Deserialize.Proto2.fromArraySegmentLD (InnerMessage())
        static member DeserializeLengthDelimited zcb    = zcb |> Deserialize.Proto2.fromZcbLD (InnerMessage())


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
            |]
        fun () -> InnerMessage.Deserialize(buf) |> ignore
        |> should throw typeof<Froto.Serialization.SerializerException>

    [<Fact>]
    let ``Serialize simple message`` () =
        let msg = InnerMessage()
        msg.Id <- 98
        msg.Name <- "ABC0"
        msg.Serialize()
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
            (m.Id         |> Encode.fromVarint 1) >>
            (m.Inner      |> Encode.fromOptionalMessage Serialize.toZcbLD 42) >>
            (m.HasMore    |> Encode.fromBool 43)
            <| zcb

        static member DecoderRing =
            [ 1 , fun (m:OuterMessage) rawField -> m.Id      <- Decode.toInt32 rawField; m
              42, fun (m:OuterMessage) rawField ->
                    let o = InnerMessage.Deserialize(rawField) |> Some
                    m.Inner <- o
                    m
              43, fun (m:OuterMessage) rawField -> m.HasMore <- Decode.toBool rawField; m
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

        member m.Serialize ()                   = Serialize.toArray m
        member m.Serialize buf                  = Serialize.toArraySegment m buf
        member m.Serialize zcb                  = Serialize.toZeroCopyBuffer m zcb
        member m.SerializeLengthDelimited ()    = Serialize.toArrayLD m

        static member Deserialize buf                   = buf |> Deserialize.Proto2.fromArray (OuterMessage())
        static member Deserialize buf                   = buf |> Deserialize.Proto2.fromArraySegment (OuterMessage())
        static member Deserialize zcb                   = zcb |> Deserialize.Proto2.fromZeroCopyBuffer (OuterMessage())
        static member Deserialize raw                   = raw |> Deserialize.Proto2.fromRawField (OuterMessage())
        static member DeserializeLengthDelimited buf    = buf |> Deserialize.Proto2.fromArrayLD (OuterMessage())
        static member DeserializeLengthDelimited buf    = buf |> Deserialize.Proto2.fromArraySegmentLD (OuterMessage())
        static member DeserializeLengthDelimited zcb    = zcb |> Deserialize.Proto2.fromZcbLD (OuterMessage())


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

