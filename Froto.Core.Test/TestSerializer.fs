﻿namespace TestSerializer

(* TODO:
    Write tests (and implementation) for:
    - Missing required field causes exception
    - Optional fields can be detected as ommitted?
    - Enum properly has correct default (handle via codegen)
*)

open Xunit
open FsUnit.Xunit

open System
open Froto.Encoding

[<Xunit.Trait("Kind", "Unit")>]
module Utility =
    open Froto.Encoding.Utility

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

        varIntLen      0UL |> should equal 0
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

    open Froto.Encoding.Encoders

    type ETestEnum =
        | ZERO = 0
        | ONE  = 1
        | TWO  = 2

    [<Fact>]
    let ``decode numeric types from Varint`` () =
        let vi = RawField.Varint (1,2UL)

        // Int32
        decodeInt32 vi
        |> should equal 2

        // Int64
        decodeInt64 vi
        |> should equal 2L

        // UInt32
        decodeUInt32 vi
        |> should equal 2u

        // UInt64
        decodeUInt64 vi
        |> should equal 2UL

        // SInt32
        decodeSInt32 vi
        |> should equal 1

        // SInt64
        decodeSInt64 vi
        |> should equal 1L

        // Bool
        decodeBool vi
        |> should equal true

        // Enum
        decodeInt32 vi
        |> enum<ETestEnum>
        |> should equal ETestEnum.TWO

        // Enum
        decodeEnum vi : ETestEnum
        |> should equal ETestEnum.TWO


    [<Fact>]
    let ``Int overflow is truncated`` () =
        let vi = RawField.Varint (1, 0x0000000100000001UL)
        decodeInt32 vi
        |> should equal 1

    [<Fact>]
    let ``Unknown enum value is preserved`` () =
        let vi = RawField.Varint (1, 42UL)
        (decodeEnum vi : ETestEnum)
        |> should equal (enum 42 : ETestEnum)

    [<Fact>]
    let ``Unexpected wire type throws`` () =
        let f32 = RawField.Fixed32(1, 42u)
        fun () ->
            decodeInt32 f32 |> ignore
        |> should throw typeof<ProtobufSerializerException>

    [<Fact>]
    let ``Hydrate numeric types from fixed`` () =
        let vi = RawField.Fixed32 (1, 42u)
        decodeFixed32 vi
        |> should equal 42u

        let vi = RawField.Fixed64 (1, 42UL)
        decodeFixed64 vi
        |> should equal 42UL

        let vi = RawField.Fixed32 (1, 42u)
        decodeSFixed32 vi
        |> should equal 42

        let vi = RawField.Fixed64 (1, 42UL)
        decodeSFixed64 vi
        |> should equal 42L

        let vi = RawField.Fixed32 (1, 0b01000000u <<< (3*8))
        decodeSingle vi
        |> should equal 2.0f

        let vi = RawField.Fixed64 (1, 0b01000000UL <<< (7*8) )
        decodeDouble vi
        |> should equal 2.0

    [<Fact>]
    let ``Hydrate bytes`` () =
        let buf = [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodeBytes vi
        |> should equal buf

    [<Fact>]
    let ``Hydrate string`` () =
        let buf = [| 0x41uy; 0x42uy; 0x43uy; 0x34uy; 0x32uy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodeString vi
        |> should equal "ABC42"

    [<Fact>]
    let ``Hydrate packed numeric types`` () =

        // Packet Int32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedInt32 vi
        |> should equal [ 0; 1; 2; 3; 4 ]

        // Packet UInt32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedUInt32 vi
        |> should equal [ 0u; 1u; 2u; 3u; 4u ]

        // Packet SInt32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedSInt32 vi
        |> should equal [ 0; -1; 1; -2; 2 ]

        // Packet Int64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedInt64 vi
        |> should equal [ 0L; 1L; 2L; 3L; 4L ]

        // Packet UInt64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedUInt64 vi
        |> should equal [ 0UL; 1UL; 2UL; 3UL; 4UL ]

        // Packet SInt64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedSInt64 vi
        |> should equal [ 0L; -1L; 1L; -2L; 2L ]

        // Packed Bool
        let buf = [|0uy; 1uy; 2uy; 0uy; 4uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedBool vi
        |> should equal [ false; true; true; false; true ]

        // Packed Enum
        let buf = [|0uy; 1uy; 2uy|]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedEnum vi : ETestEnum list
        |> should equal [ ETestEnum.ZERO; ETestEnum.ONE; ETestEnum.TWO ]

        // Packed Fixed32
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedFixed32 vi
        |> should equal [ 0u; 0x00000201u; 0xFFFFFFFFu ]

        // Packed Fixed64
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0uy; 0uy; 0uy; 0xFFuy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedFixed64 vi
        |> should equal [ 0x0000020100000000UL; 0xFF000000FFFFFFFFUL ]

        // Packed SFixed32
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedSFixed32 vi
        |> should equal [ 0; 0x00000201; -1 ]

        // Packed SFixed64
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedSFixed64 vi
        |> should equal [ 0x0000020100000000L; -1L ]

        // Packed Single
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x00uy; 0x00uy; 0x00uy; 0b01000000uy |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedSingle vi
        |> should equal [ 0.0f; 2.0f ]

        // Packed Double
        let buf = [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 
                     0x9Auy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0xB9uy; 0x3Fuy;
                     0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; |]
        let vi = RawField.LengthDelimited (1, ArraySegment(buf))
        decodePackedDouble vi
        |> should equal [ 0.0; 0.10; 0.0 ]


[<Xunit.Trait("Kind", "Unit")>]
module Serialize =
    open Froto.Encoding.Encoders

    type ZCB = ZeroCopyBuffer
    let toArray(zcb:ZCB) = zcb.ToArray()

    let fid = 1 // field ID

    [<Fact>]
    let ``Dedecode integer varint`` () =
        ZCB(2)
        |> encodeVarint fid 0UL
        |> toArray
        |> should equal Array.empty

        ZCB(2)
        |> encodeVarint fid 2UL
        |> toArray
        |> should equal [| 0x08uy; 2uy |]

    [<Fact>]
    let ``Dedecode SInt32 varint`` () =
        ZCB(2)
        |> encodeSInt32 fid 0
        |> toArray
        |> should equal Array.empty

        ZCB(2)
        |> encodeSInt32 fid -1
        |> toArray
        |> should equal [| 0x08uy; 1uy |]

        ZCB(2)
        |> encodeSInt32 fid 1
        |> toArray
        |> should equal [| 0x08uy; 2uy |]

    [<Fact>]
    let ``Dedecode SInt64 varint`` () =
        ZCB(2)
        |> encodeSInt64 fid 0L
        |> toArray
        |> should equal Array.empty

        ZCB(2)
        |> encodeSInt64 fid -1L
        |> toArray
        |> should equal [| 0x08uy; 1uy |]

        ZCB(2)
        |> encodeSInt64 fid 1L
        |> toArray
        |> should equal [| 0x08uy; 2uy |]

    [<Fact>]
    let ``Dedecode bool varint`` () =
        ZCB(2)
        |> encodeBool fid false
        |> toArray
        |> should equal Array.empty

        ZCB(2)
        |> encodeBool fid true
        |> toArray
        |> should equal [| 0x08uy; 1uy |]

    [<Fact>]
    let ``Dedecode Fixed32`` () =
        ZCB(5)
        |> encodeFixed32 fid 5
        |> toArray
        |> should equal [| 0x08uy ||| 5uy; 5uy;0uy;0uy;0uy |]

    [<Fact>]
    let ``Dedecode Fixed64`` () =
        ZCB(9)
        |> encodeFixed64 fid 5
        |> toArray
        |> should equal [| 0x08uy ||| 1uy; 5uy;0uy;0uy;0uy; 0uy;0uy;0uy;0uy |]

    [<Fact>]
    let ``Dedecode Single`` () =
        ZCB(5)
        |> encodeSingle fid 2.0f
        |> toArray
        |> should equal [| 0x08uy ||| 5uy; 0uy; 0uy; 0b00000000uy; 0b01000000uy |]

    [<Fact>]
    let ``Dedecode Double`` () =
        ZCB(9)
        |> encodeDouble fid 0.10
        |> toArray
        |> should equal [| 0x08uy ||| 1uy; 0x9Auy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0xB9uy; 0x3Fuy |]

    [<Fact>]
    let ``Dedecode String`` () =
        ZCB(6)
        |> encodeString fid "0ABC"
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 4uy; 0x30uy; 0x41uy; 0x42uy; 0x43uy |]

    [<Fact>]
    let ``Dedecode Bytes`` () =
        ZCB(6)
        |> encodeBytes fid (ArraySegment([| 3uy; 4uy; 5uy; 6uy; |]))
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 4uy; 3uy; 4uy; 5uy; 6uy |]


    let ``Dedecode with default value`` () =
        // Verify non-default value results in an actual value
        ZCB(2)
        |> encodeDefaultedVarint 0UL fid 2UL
        |> toArray
        |> should equal [| 0x08uy; 2uy |]

        // Now, check that default value result in an empty array (value is elided)
        let checkGetsElided f =
            ZCB(16)
            |> f
            |> toArray
            |> should equal Array.empty
        
        checkGetsElided <| encodeDefaultedVarint 1UL fid 1UL
        checkGetsElided <| encodeDefaultedSInt32 2 fid 2
        checkGetsElided <| encodeDefaultedSInt64 3L fid 3L
        checkGetsElided <| encodeDefaultedBool true fid true
        checkGetsElided <| encodeDefaultedFixed32 4 fid 4
        checkGetsElided <| encodeDefaultedFixed64 5L fid 5L
        checkGetsElided <| encodeDefaultedSingle 0.60f fid 0.60f
        checkGetsElided <| encodeDefaultedDouble 0.70 fid 0.70
        checkGetsElided <| encodeDefaultedString "Hello" fid "Hello"
        checkGetsElided <| encodeDefaultedBytes (ArraySegment([|8uy;9uy|])) fid (ArraySegment([|8uy;9uy|]))

    [<Fact>]
    let ``Dedecode Packed Varint`` () =
        ZCB(8)
        |> encodePackedVarint fid [ 0; 1; 128; 129 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 6uy; 0uy; 1uy; 0x80uy; 0x01uy; 0x81uy; 0x01uy |]
    
    [<Fact>]
    let ``Dedecode Packed Bool`` () =
        ZCB(5)
        |> encodePackedBool fid [ false; true; false ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 3uy; 0uy; 1uy; 0uy |]

    [<Fact>]
    let ``Dedecode Packed SInt32`` () =
        ZCB(5)
        |> encodePackedSInt32 fid [ 0; -1; 1 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 3uy; 0uy; 1uy; 2uy |]

    [<Fact>]
    let ``Dedecode Packed SInt64`` () =
        ZCB(5)
        |> encodePackedSInt64 fid [ 0L; -1L; 1L ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 3uy; 0uy; 1uy; 2uy |]

    [<Fact>]
    let ``Dedecode Packed Fixed32`` () =
        ZCB(10)
        |> encodePackedFixed32 fid [ 0; -1 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 8uy; 0x00uy;0x00uy;0x00uy;0x00uy; 0xFFuy;0xFFuy;0xFFuy;0xFFuy |]
        

    [<Fact>]
    let ``Dedecode Packed Fixed64`` () =
        ZCB(18)
        |> encodePackedFixed64 fid [ 0; -1 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 16uy; 0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; 0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy |]

    [<Fact>]
    let ``Dedecode Packed Single`` () =
        ZCB(10)
        |> encodePackedSingle fid [ 0.0f; 2.0f ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 8uy; 0x00uy;0x00uy;0x00uy;0x00uy; 0uy;0uy;0b00000000uy;0b01000000uy |]
        

    [<Fact>]
    let ``Dedecode Packed Double`` () =
        ZCB(18)
        |> encodePackedDouble fid [ 0.0; 0.10 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 16uy; 0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; 0x9Auy;0x99uy;0x99uy;0x99uy;0x99uy;0x99uy;0xB9uy;0x3Fuy |]

    [<Fact>]
    let ``Dedecode Repeated Field`` () =
        ZCB(12)
        |> encodeRepeated encodeString fid [ "0ABC"; "CBA0" ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 4uy; 0x30uy; 0x41uy; 0x42uy; 0x43uy;
                           0x08uy ||| 2uy; 4uy; 0x43uy; 0x42uy; 0x41uy; 0x30uy |]


