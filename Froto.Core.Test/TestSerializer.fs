namespace TestSerializer

open Xunit
open FsUnit.Xunit

open System
open Froto.Core
open Froto.Core.Encoding

[<Xunit.Trait("Kind", "Unit")>]
module Utility =
    open Froto.Core.Encoding.Utility

    [<Fact>]
    let ``Zig to Zag`` () =
        zigZag32  0 |> should equal 0
        zigZag32 -1 |> should equal 1
        zigZag32  1 |> should equal 2
        zigZag32 -2 |> should equal 3
        zigZag32  2 |> should equal 4

        zigZag64  0L |> should equal 0L
        zigZag64 -1L |> should equal 1L
        zigZag64  1L |> should equal 2L
        zigZag64 -2L |> should equal 3L
        zigZag64  2L |> should equal 4L
    
    let ``Zag to Zig`` () =
        zagZig32 0 |> should equal  0
        zagZig32 1 |> should equal -1
        zagZig32 2 |> should equal  1
        zagZig32 3 |> should equal -2
        zagZig32 4 |> should equal  2

        zagZig64 0L |> should equal  0L
        zagZig64 1L |> should equal -1L
        zagZig64 2L |> should equal  1L
        zagZig64 3L |> should equal -2L
        zagZig64 4L |> should equal  2L

    [<Fact>]
    let ``Length when encoded to Varint`` () =

        varIntLen      0UL |> should equal 1
        varIntLen   0x7FUL |> should equal 1
        varIntLen   0x80UL |> should equal 2
        varIntLen 0x3FFFUL |> should equal 2
        varIntLen 0x4000UL |> should equal 3

    [<Fact>]
    let ``Length of field number when encoded in a tag`` () =
        tagLen    0 |> should equal 1
        tagLen 0x0F |> should equal 1
        tagLen 0x10 |> should equal 2

[<Xunit.Trait("Kind", "Unit")>]
module Deserialize =

    open Froto.Core.Encoding.Serializer

    type ETestEnum =
        | ZERO = 0
        | ONE  = 1
        | TWO  = 2

    [<Fact>]
    let ``hydrate numeric types from Varint`` () =
        let vi  = RawField.Varint (1,2UL)

        // Int32
        let x = ref 0
        vi |> hydrateInt32 x
        !x |> should equal 2

        // Int64
        let x = ref 0L
        vi |> hydrateInt64 x
        !x |> should equal 2L

        // UInt32
        let x = ref 0u
        vi |> hydrateUInt32 x
        !x |> should equal 2u

        // UInt64
        let x = ref 0UL
        vi |> hydrateUInt64 x
        !x |> should equal 2UL

        // SInt32
        let x = ref 0
        vi |> hydrateSInt32 x
        !x |> should equal 1

        // SInt64
        let x = ref 0L
        vi |> hydrateSInt64 x
        !x |> should equal 1L

        // Bool
        let x = ref false
        vi |> hydrateBool x
        !x |> should equal true

        // Enum
        let x = ref ETestEnum.ZERO
        vi |> hydrateEnum x
        !x |> should equal ETestEnum.TWO

    [<Fact>]
    let ``Int overflow is truncated`` () =
        let vi = RawField.Varint (1, 0x0000000100000001UL)
        let x = ref 0
        vi |> hydrateInt32 x
        !x |> should equal 1

    [<Fact>]
    let ``Unknown enum value is preserved`` () =
        let vi = RawField.Varint (1, 42UL)
        let x = ref ETestEnum.ZERO
        vi |> hydrateEnum x
        int(!x) |> should equal 42

    [<Fact>]
    let ``Unexpected wire type throws`` () =
        let f32 = RawField.Fixed32(1, 42u)
        let x = ref 0
        fun () ->
            f32 |> hydrateInt32 x
        |> should throw typeof<ProtobufSerializerException>

    [<Fact>]
    let ``Hydrate numeric types from fixed`` () =
        let vi = RawField.Fixed32 (1, 42u)
        let x = ref 0u
        vi |> hydrateFixed32 x
        !x |> should equal 42u

        let vi = RawField.Fixed64 (1, 42UL)
        let x = ref 0UL
        vi |> hydrateFixed64 x
        !x |> should equal 42UL

        let vi = RawField.Fixed32 (1, 42u)
        let x = ref 0
        vi |> hydrateSFixed32 x
        !x |> should equal 42

        let vi = RawField.Fixed64 (1, 42UL)
        let x = ref 0L
        vi |> hydrateSFixed64 x
        !x |> should equal 42L

        let vi = RawField.Fixed32 (1, 0b01000000u <<< (3*8))
        let x = ref 0.0f
        vi |> hydrateSingle x
        !x |> should equal 2.0f

        let vi = RawField.Fixed64 (1, 0b01000000UL <<< (7*8) )
        let x = ref 0.0
        vi |> hydrateDouble x
        !x |> should equal 2.0

    [<Fact>]
    let ``Hydrate bytes`` () =
        let buf = [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let x = ref Array.empty
        vi |> hydrateBytes x
        !x |> should equal buf

    [<Fact>]
    let ``Hydrate string`` () =
        let buf = [| 0x41uy; 0x42uy; 0x43uy; 0x34uy; 0x32uy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let x = ref ""
        vi |> hydrateString x
        !x |> should equal "ABC42"

    [<Fact>]
    let ``Hydrate packed numeric types`` () =

        // Packet Int32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedInt32 xs
        !xs |> should equal [ 0; 1; 2; 3; 4 ]

        // Packet UInt32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedUInt32 xs
        !xs |> should equal [ 0u; 1u; 2u; 3u; 4u ]

        // Packet SInt32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedSInt32 xs
        !xs |> should equal [ 0; -1; 1; -2; 2 ]

        // Packet Int64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedInt64 xs
        !xs |> should equal [ 0L; 1L; 2L; 3L; 4L ]

        // Packet UInt64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedUInt64 xs
        !xs |> should equal [ 0UL; 1UL; 2UL; 3UL; 4UL ]

        // Packet SInt64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedSInt64 xs
        !xs |> should equal [ 0L; -1L; 1L; -2L; 2L ]

        // Packed Bool
        let buf = [|0uy; 1uy; 2uy; 0uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedBool xs
        !xs |> should equal [ false; true; true; false; true ]

        // Packed Enum
        let buf = [|0uy; 1uy; 2uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs : ETestEnum list ref = ref List.empty
        vi |> hydratePackedEnum xs
        !xs |> should equal [ ETestEnum.ZERO; ETestEnum.ONE; ETestEnum.TWO ]

        // Packed Fixed32
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedFixed32 xs
        !xs |> should equal [ 0u; 0x00000201u; 0xFFFFFFFFu ]

        // Packed Fixed64
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0uy; 0uy; 0uy; 0xFFuy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedFixed64 xs
        !xs |> should equal [ 0x0000020100000000UL; 0xFF000000FFFFFFFFUL ]

        // Packed SFixed32
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedSFixed32 xs
        !xs |> should equal [ 0; 0x00000201; -1 ]

        // Packed SFixed64
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedSFixed64 xs
        !xs |> should equal [ 0x0000020100000000L; -1L ]

        // Packed Single
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x00uy; 0x00uy; 0x00uy; 0b01000000uy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedSingle xs
        !xs |> should equal [ 0.0f; 2.0f ]

        // Packed Double
        let buf = [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 
                     0x9Auy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0xB9uy; 0x3Fuy;
                     0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        let xs = ref List.empty
        vi |> hydratePackedDouble xs
        !xs |> should equal [ 0.0; 0.10; 0.0 ]


    type InnerMsg () =
        inherit MessageBase()

        let m_id = ref 0
        let m_name = ref ""

        override x.DecoderRing =
            [ 1, m_id             |> Serializer.hydrateInt32
              2, m_name           |> Serializer.hydrateString
            ]
            |> Map.ofList

        override x.EncoderRing =
            [ !m_id            |> Serializer.dehydrateVarint 1
              !m_name          |> Serializer.dehydrateString 2
            ]

        static member Deserialize buf =
            let self = InnerMsg()
            self.Deserialize(buf) |> ignore
            self

        static member DeserializeLengthDelimited buf =
            let self = InnerMsg()
            self.DeserializeLengthDelimited(buf) |> ignore
            self
        
        member x.ID = !m_id
        member x.Name = !m_name

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
        let msg = InnerMsg.Deserialize(buf)
        msg.ID |> should equal 99
        msg.Name |> should equal "Test message"


    type OuterMsg () =
        inherit MessageBase()

        let m_id    = ref 0
        let m_inner = ref <| InnerMsg()

        override x.DecoderRing =
            [  1, m_id    |> Serializer.hydrateInt32;
              42, m_inner |> Serializer.hydrateMessage (InnerMsg.Deserialize);
            ]
            |> Map.ofList

        override x.EncoderRing =
            [ !m_id |> Serializer.dehydrateVarint 1
              !m_inner |> Serializer.dehydrateMessage 42 ]

        static member Deserialize buf =
            let self = OuterMsg()
            self.Deserialize(buf) |> ignore
            self

        static member DeserializeLengthDelimited buf =
            let self = OuterMsg()
            self.DeserializeLengthDelimited(buf) |> ignore
            self

        member x.ID = !m_id
        member x.Inner = !m_inner

    [<Fact>]
    let ``Deserialize nested message`` () =
        let buf =
            [|  0x01uy<<<3 ||| 0uy;     // tag: fldnum=1, varint
                21uy;                   // value 42
                0xD0uy ||| 2uy; 0x02uy; // tag: fldnum=42, length delim
                16uy;                   // length 16
                0x01uy<<<3 ||| 0uy;     // tag: fldnum=1; varint
                99uy;                   // value 99
                0x02uy<<<3 ||| 2uy;     // tag: fldnum=2; length delim
                12uy;                   // length = 12
                0x54uy; 0x65uy; 0x73uy; 0x74uy; 0x20uy; 0x6duy; 0x65uy; 0x73uy; 0x73uy; 0x61uy; 0x67uy; 0x65uy
                                        // value "Test message"
            |] |> ArraySegment
        let msg = OuterMsg.Deserialize(buf)
        msg.ID |> should equal 21
        msg.Inner.ID |> should equal 99
        msg.Inner.Name |> should equal "Test message"
        

    (* TODO: Tests to write
        - Serialization Tests
        - Missing required field causes exception
        - Optional fields can be detected as ommitted
        - Enum properly has correct default (handle via codegen)
     *)