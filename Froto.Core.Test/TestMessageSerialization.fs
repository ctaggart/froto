namespace TestSerializer

open Xunit
open FsUnit.Xunit

open System
open Froto.Core
open Froto.Core.ClassModel
open Froto.Core.Hydration

[<Xunit.Trait("Kind", "Unit")>]
module MessageSerialization =

    let toArray (seg:ArraySegment<'a>) =
        seg.Array.[ seg.Offset .. (seg.Count-1) ]

    type InnerMessage () as self =
        inherit MessageBase()

        member val Id = 0 with get,set
        member val Name = "" with get,set

        override x.Clear() =
            x.Id <- 0
            x.Name <- ""

        override x.RequiredFields =
            [ 1; 2 ]
            |> Set.ofList

        override x.DecoderRing =
            [ 1, fun rawField -> self.Id <- hydrateInt32 rawField
              2, fun rawField -> self.Name <- hydrateString rawField
            ]
            |> Map.ofList
        
        override x.Encode zcb =
            let encode =
                (x.Id     |> dehydrateVarint 1) >>
                (x.Name   |> dehydrateString 2)
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
        fun () -> InnerMessage.FromArraySegment(buf) |> ignore
        |> should throw typeof<Froto.Core.ProtobufSerializerException>

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

    type OuterMessage () as self =
        inherit MessageBase()

        member val Id        = 0 with get,set
        member val Inner     = None with get,set
        member val HasMore   = false with get,set

        override x.Clear() =
            x.Id <- 0
            x.Inner <- None
            x.HasMore <- false

        override x.DecoderRing =
            [ 1 , fun rawField -> self.Id      <- hydrateInt32 rawField
              42, fun rawField -> self.Inner   <- hydrateOptionalMessage (InnerMessage.FromArraySegment) rawField
              43, fun rawField -> self.HasMore <- hydrateBool rawField
            ]
            |> Map.ofList

        override x.Encode zcb =
            let encode =
                (x.Id         |> dehydrateVarint 1) >>
                (x.Inner      |> dehydrateOptionalMessage 42) >>
                (x.HasMore    |> dehydrateBool 43)
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

