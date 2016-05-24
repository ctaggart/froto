namespace TestSerializer

open Xunit
open FsUnit.Xunit

open System
open Froto.Core
open Froto.Core.Encoding

[<Xunit.Trait("Kind", "Unit")>]
module MessageSerialization =

    let toArray (seg:ArraySegment<'a>) =
        seg.Array.[ seg.Offset .. (seg.Count-1) ]

    type InnerMessage () =
        inherit MessageBase()
        let m_id = ref 0
        let m_name = ref ""

        let m_requiredFields =
            [ 1; 2 ]
            |> Set.ofList

        let m_decoderRing =
            [ 1, m_id   |> ClassSerializer.hydrateInt32
              2, m_name |> ClassSerializer.hydrateString
            ]
            |> Map.ofList
        
        member x.ID     with get() = !m_id and set(v) = m_id := v
        member x.Name   with get() = !m_name and set(v) = m_name := v

        override x.Clear() =
            m_id := 0
            m_name := ""

        override x.RequiredFields = m_requiredFields
        override x.DecoderRing = m_decoderRing
        override x.Encode zcb =
            let encode =
                (!m_id     |> ClassSerializer.dehydrateVarint 1) >>
                (!m_name   |> ClassSerializer.dehydrateString 2)
            encode zcb

        static member FromArraySegment (buf:ArraySegment<byte>) =
            let self = InnerMessage()
            self.Merge(buf) |> ignore
            self

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
        let msg = InnerMessage.FromArraySegment(buf)
        msg.ID |> should equal 99
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
        fun () -> InnerMessage.FromArraySegment(buf) |> ignore
        |> should throw typeof<Froto.Core.ProtobufSerializerException>

    [<Fact>]
    let ``Serialize simple message`` () =
        let msg = InnerMessage()
        msg.ID <- 98
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
        inherit MessageBase()
        let m_id        = ref 0
        let m_inner     = ref None
        let m_hasMore   = ref false

        let m_decoderRing =
            [  1, m_id      |> ClassSerializer.hydrateInt32;
              42, m_inner   |> ClassSerializer.hydrateOptionalMessage (InnerMessage.FromArraySegment);
              43, m_hasMore |> ClassSerializer.hydrateBool;
            ]
            |> Map.ofList

        member x.ID         with get() = !m_id and set(v) = m_id := v
        member x.Inner      with get() = !m_inner and set(v) = m_inner := v
        member x.HasMore    with get() = !m_hasMore and set(v) = m_hasMore := v

        override x.Clear() =
            m_id := 0
            m_inner := None
            m_hasMore := false

        override x.DecoderRing = m_decoderRing
        override x.Encode zcb =
            let encode =
                (!m_id         |> ClassSerializer.dehydrateVarint 1) >>
                (!m_inner      |> ClassSerializer.dehydrateOptionalMessage 42) >>
                (!m_hasMore    |> ClassSerializer.dehydrateBool 43)
            encode zcb

        static member FromArraySegment (buf:ArraySegment<byte>) =
            let self = OuterMessage()
            self.Merge(buf) |> ignore
            self

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
        let msg = OuterMessage.FromArraySegment(buf)
        msg.ID |> should equal 21
        msg.Inner.IsSome |> should equal true
        msg.Inner.Value.ID |> should equal 99
        msg.Inner.Value.Name |> should equal "Test message"
        
    [<Fact>]
    let ``Serialize compound message`` () =
        let msg = OuterMessage()
        msg.Inner <- Some(InnerMessage())
        msg.ID <- 5
        msg.Inner.Value.ID <- 6
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

