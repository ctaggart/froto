namespace TestSerializer

open Xunit
open FsUnit.Xunit

[<Xunit.Trait("Kind", "Unit")>]
module Decode =

    open System

    open Froto.Serialization
    open Froto.Serialization.Encoding


    type ETestEnum =
        | ZERO = 0
        | ONE  = 1
        | TWO  = 2

    [<Fact>]
    let ``decode numeric types from Varint`` () =
        let vi = RawField.Varint (1,2UL)

        // Int32
        vi
        |> Decode.toInt32
        |> should equal 2

        // Int64
        vi
        |> Decode.toInt64
        |> should equal 2L

        // UInt32
        vi
        |> Decode.toUInt32
        |> should equal 2u

        // UInt64
        vi
        |> Decode.toUInt64
        |> should equal 2UL

        // SInt32
        vi
        |> Decode.toSInt32
        |> should equal 1

        // SInt64
        vi
        |> Decode.toSInt64
        |> should equal 1L

        // Bool
        vi
        |> Decode.toBool
        |> should equal true

        // Enum
        vi
        |> Decode.toInt32
        |> enum<ETestEnum>
        |> should equal ETestEnum.TWO

        // Enum
        vi
        |> Decode.toEnum : ETestEnum
        |> should equal ETestEnum.TWO


    [<Fact>]
    let ``Int overflow is truncated`` () =
        let vi = RawField.Varint (1, 0x0000000100000001UL)
        vi
        |> Decode.toInt32
        |> should equal 1

    [<Fact>]
    let ``Unknown enum value is preserved`` () =
        let vi = RawField.Varint (1, 42UL)
        vi
        |> Decode.toEnum : ETestEnum
        |> should equal (enum 42 : ETestEnum)

    [<Fact>]
    let ``Unexpected wire type throws`` () =
        let f32 = RawField.Fixed32(1, 42u)
        fun () ->
            Decode.toInt32 f32 |> ignore
        |> should throw typeof<EncoderException>

    [<Fact>]
    let ``Decode numeric types to fixed`` () =
        RawField.Fixed32 (1, 42u)
        |> Decode.toFixed32
        |> should equal 42u

        RawField.Fixed64 (1, 42UL)
        |> Decode.toFixed64
        |> should equal 42UL

        RawField.Fixed32 (1, 42u)
        |> Decode.toSFixed32
        |> should equal 42

        RawField.Fixed64 (1, 42UL)
        |> Decode.toSFixed64
        |> should equal 42L

        RawField.Fixed32 (1, 0b01000000u <<< (3*8))
        |> Decode.toSingle
        |> should equal 2.0f

        RawField.Fixed64 (1, 0b01000000UL <<< (7*8) )
        |> Decode.toDouble
        |> should equal 2.0

    [<Fact>]
    let ``Decode bytes`` () =
        let buf = [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy |]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toBytes
        |> should equal buf

    [<Fact>]
    let ``Decode string`` () =
        let buf = [| 0x41uy; 0x42uy; 0x43uy; 0x34uy; 0x32uy |]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toString
        |> should equal "ABC42"

    [<Fact>]
    let ``Decode packed numeric types`` () =

        // Packet Int32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedInt32
        |> should equal [ 0; 1; 2; 3; 4 ]

        // Packet UInt32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedUInt32
        |> should equal [ 0u; 1u; 2u; 3u; 4u ]

        // Packet SInt32
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedSInt32
        |> should equal [ 0; -1; 1; -2; 2 ]

        // Packet Int64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedInt64
        |> should equal [ 0L; 1L; 2L; 3L; 4L ]

        // Packet UInt64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedUInt64
        |> should equal [ 0UL; 1UL; 2UL; 3UL; 4UL ]

        // Packet SInt64
        let buf = [|0uy; 1uy; 2uy; 3uy; 4uy|]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedSInt64
        |> should equal [ 0L; -1L; 1L; -2L; 2L ]

        // Packed Bool
        let buf = [|0uy; 1uy; 2uy; 0uy; 4uy|]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedBool
        |> should equal [ false; true; true; false; true ]

        // Packed Enum
        let buf = [|0uy; 1uy; 2uy|]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedEnum : ETestEnum list
        |> should equal [ ETestEnum.ZERO; ETestEnum.ONE; ETestEnum.TWO ]

        // Packed Fixed32
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedFixed32
        |> should equal [ 0u; 0x00000201u; 0xFFFFFFFFu ]

        // Packed Fixed64
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0uy; 0uy; 0uy; 0xFFuy |]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedFixed64
        |> should equal [ 0x0000020100000000UL; 0xFF000000FFFFFFFFUL ]

        // Packed SFixed32
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedSFixed32
        |> should equal [ 0; 0x00000201; -1 ]

        // Packed SFixed64
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x01uy; 0x02uy; 0x00uy; 0x00uy;  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedSFixed64
        |> should equal [ 0x0000020100000000L; -1L ]

        // Packed Single
        let buf = [|0uy; 0uy; 0uy; 0uy; 0x00uy; 0x00uy; 0x00uy; 0b01000000uy |]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedSingle
        |> should equal [ 0.0f; 2.0f ]

        // Packed Double
        let buf = [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 
                     0x9Auy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0xB9uy; 0x3Fuy;
                     0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; |]
        RawField.LengthDelimited (1, ArraySegment(buf))
        |> Decode.toPackedDouble
        |> should equal [ 0.0; 0.10; 0.0 ]


[<Xunit.Trait("Kind", "Unit")>]
module Encode =

    open System

    open Froto.Serialization
    open Froto.Serialization.Encoding

    type ZCB = ZeroCopyBuffer
    let toArray(zcb:ZCB) = zcb.ToArray()

    let fid = 1 // field ID

    [<Fact>]
    let ``Encode from raw field list`` () =
        let byteTag n t = (n <<<3) ||| (int32 t) |> byte
        let fields =
            [
                RawField.Varint(1, 2UL)
                RawField.Fixed32(2, 0x80000001u)
                RawField.Fixed64(3, 0x8000000000000001UL)
                RawField.LengthDelimited(4, ArraySegment([|0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy|]))
            ]
        ZCB(256)
        |> Encode.fromRawFields fields
        |> toArray
        |> should equal
            [| byteTag 1 WireType.Varint;  2uy;
               byteTag 2 WireType.Fixed32; 0x01uy; 0x00uy; 0x00uy; 0x80uy;
               byteTag 3 WireType.Fixed64; 1uy;0uy;0uy;0uy; 0uy;0uy;0uy;0x80uy;
               byteTag 4 WireType.LengthDelimited; 5uy; 0uy; 1uy; 2uy; 3uy; 4uy;
             |]



    [<Fact>]
    let ``Encode integer varint`` () =
        ZCB(2)
        |> Encode.fromVarint fid 0UL
        |> toArray
        |> should equal Array.empty

        ZCB(2)
        |> Encode.fromRequiredVarint fid 0UL
        |> toArray
        |> should equal [| 0x08uy; 0uy |]

        ZCB(2)
        |> Encode.fromVarint fid 2UL
        |> toArray
        |> should equal [| 0x08uy; 2uy |]

    [<Fact>]
    let ``Encode SInt32 varint`` () =
        ZCB(2)
        |> Encode.fromSInt32 fid 0
        |> toArray
        |> should equal Array.empty

        ZCB(2)
        |> Encode.fromRequiredSInt32 fid 0
        |> toArray
        |> should equal [| 0x08uy; 0uy|]

        ZCB(2)
        |> Encode.fromSInt32 fid -1
        |> toArray
        |> should equal [| 0x08uy; 1uy |]

        ZCB(2)
        |> Encode.fromSInt32 fid 1
        |> toArray
        |> should equal [| 0x08uy; 2uy |]

    [<Fact>]
    let ``Encode SInt64 varint`` () =
        ZCB(2)
        |> Encode.fromSInt64 fid 0L
        |> toArray
        |> should equal Array.empty

        ZCB(2)
        |> Encode.fromRequiredSInt64 fid 0L
        |> toArray
        |> should equal [| 0x08uy; 0uy |]

        ZCB(2)
        |> Encode.fromSInt64 fid -1L
        |> toArray
        |> should equal [| 0x08uy; 1uy |]

        ZCB(2)
        |> Encode.fromSInt64 fid 1L
        |> toArray
        |> should equal [| 0x08uy; 2uy |]

    [<Fact>]
    let ``Encode bool varint`` () =
        ZCB(2)
        |> Encode.fromBool fid false
        |> toArray
        |> should equal Array.empty

        ZCB(2)
        |> Encode.fromRequiredBool fid false
        |> toArray
        |> should equal [| 0x08uy; 0uy |]

        ZCB(2)
        |> Encode.fromBool fid true
        |> toArray
        |> should equal [| 0x08uy; 1uy |]

    [<Fact>]
    let ``Encode Fixed32`` () =
        ZCB(5)
        |> Encode.fromFixed32 fid 0u
        |> toArray
        |> should equal Array.empty

        ZCB(5)
        |> Encode.fromRequiredFixed32 fid 0u
        |> toArray
        |> should equal [| 0x08uy ||| 5uy; 0uy;0uy;0uy;0uy |]

        ZCB(5)
        |> Encode.fromFixed32 fid 5u
        |> toArray
        |> should equal [| 0x08uy ||| 5uy; 5uy;0uy;0uy;0uy |]


    [<Fact>]
    let ``Encode Fixed64`` () =
        ZCB(9)
        |> Encode.fromFixed64 fid 0UL
        |> toArray
        |> should equal Array.empty

        ZCB(9)
        |> Encode.fromRequiredFixed64 fid 0UL
        |> toArray
        |> should equal [| 0x08uy ||| 1uy; 0uy;0uy;0uy;0uy; 0uy;0uy;0uy;0uy |]

        ZCB(9)
        |> Encode.fromFixed64 fid 5UL
        |> toArray
        |> should equal [| 0x08uy ||| 1uy; 5uy;0uy;0uy;0uy; 0uy;0uy;0uy;0uy |]

    [<Fact>]
    let ``Encode SFixed32`` () =
        ZCB(5)
        |> Encode.fromSFixed32 fid 0
        |> toArray
        |> should equal Array.empty

        ZCB(5)
        |> Encode.fromRequiredSFixed32 fid 0
        |> toArray
        |> should equal [| 0x08uy ||| 5uy; 0uy;0uy;0uy;0uy |]

        ZCB(5)
        |> Encode.fromSFixed32 fid 5
        |> toArray
        |> should equal [| 0x08uy ||| 5uy; 5uy;0uy;0uy;0uy |]

    [<Fact>]
    let ``Encode SFixed64`` () =
        ZCB(9)
        |> Encode.fromSFixed64 fid 0L
        |> toArray
        |> should equal Array.empty

        ZCB(9)
        |> Encode.fromRequiredSFixed64 fid 0L
        |> toArray
        |> should equal [| 0x08uy ||| 1uy; 0uy;0uy;0uy;0uy; 0uy;0uy;0uy;0uy |]

        ZCB(9)
        |> Encode.fromSFixed64 fid 5L
        |> toArray
        |> should equal [| 0x08uy ||| 1uy; 5uy;0uy;0uy;0uy; 0uy;0uy;0uy;0uy |]

    [<Fact>]
    let ``Encode Single`` () =
        ZCB(5)
        |> Encode.fromSingle fid 0.0f
        |> toArray
        |> should equal Array.empty

        ZCB(5)
        |> Encode.fromRequiredSingle fid 0.0f
        |> toArray
        |> should equal [| 0x08uy ||| 5uy; 0uy; 0uy; 0b00000000uy; 0b00000000uy |]

        ZCB(5)
        |> Encode.fromSingle fid 2.0f
        |> toArray
        |> should equal [| 0x08uy ||| 5uy; 0uy; 0uy; 0b00000000uy; 0b01000000uy |]

    [<Fact>]
    let ``Encode Double`` () =
        ZCB(9)
        |> Encode.fromDouble fid 0.0
        |> toArray
        |> should equal Array.empty

        ZCB(9)
        |> Encode.fromRequiredDouble fid 0.0
        |> toArray
        |> should equal [| 0x08uy ||| 1uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

        ZCB(9)
        |> Encode.fromDouble fid 0.10
        |> toArray
        |> should equal [| 0x08uy ||| 1uy; 0x9Auy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0xB9uy; 0x3Fuy |]

    [<Fact>]
    let ``Encode String`` () =
        ZCB(6)
        |> Encode.fromString fid ""
        |> toArray
        |> should equal Array.empty

        ZCB(6)
        |> Encode.fromRequiredString fid ""
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 0uy |]

        ZCB(6)
        |> Encode.fromString fid "0ABC"
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 4uy; 0x30uy; 0x41uy; 0x42uy; 0x43uy |]

    [<Fact>]
    let ``Encode Bytes`` () =
        ZCB(6)
        |> Encode.fromBytes fid (ArraySegment(Array.empty))
        |> toArray
        |> should equal Array.empty

        ZCB(6)
        |> Encode.fromRequiredBytes fid (ArraySegment(Array.empty))
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 0uy |]

        ZCB(6)
        |> Encode.fromBytes fid (ArraySegment([| 3uy; 4uy; 5uy; 6uy; |]))
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 4uy; 3uy; 4uy; 5uy; 6uy |]


    [<Fact>]
    let ``Encode with default value`` () =
        // Verify non-default value results in an actual value
        ZCB(2)
        |> Encode.fromDefaultedVarint 0UL fid 2UL
        |> toArray
        |> should equal [| 0x08uy; 2uy |]

        ZCB(6)
        |> Encode.fromDefaultedBytes (ArraySegment[| 3uy; 4uy |]) fid (ArraySegment([| 3uy; 4uy; 5uy; 6uy; |]))
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 4uy; 3uy; 4uy; 5uy; 6uy |]

        ZCB(6)
        |> Encode.fromDefaultedBytes (ArraySegment[| 3uy; 4uy; 5uy; 7uy |]) fid (ArraySegment([| 3uy; 4uy; 5uy; 6uy; |]))
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 4uy; 3uy; 4uy; 5uy; 6uy |]

        // Now, check that default value result in an empty array (value is elided)
        let checkGetsElided f =
            ZCB(16)
            |> f
            |> toArray
            |> should equal Array.empty
        
        checkGetsElided <| Encode.fromDefaultedVarint 5UL fid 5UL
        checkGetsElided <| Encode.fromDefaultedSInt32 2 fid 2
        checkGetsElided <| Encode.fromDefaultedSInt64 3L fid 3L
        checkGetsElided <| Encode.fromDefaultedBool true fid true
        checkGetsElided <| Encode.fromDefaultedFixed32 4u fid 4u
        checkGetsElided <| Encode.fromDefaultedFixed64 5UL fid 5UL
        checkGetsElided <| Encode.fromDefaultedSFixed32 4 fid 4
        checkGetsElided <| Encode.fromDefaultedSFixed64 5L fid 5L
        checkGetsElided <| Encode.fromDefaultedSingle 0.60f fid 0.60f
        checkGetsElided <| Encode.fromDefaultedDouble 0.70 fid 0.70
        checkGetsElided <| Encode.fromDefaultedString "Hello" fid "Hello"
        checkGetsElided <| Encode.fromDefaultedBytes (ArraySegment([|8uy;9uy|])) fid (ArraySegment([|8uy;9uy|]))
        checkGetsElided <| Encode.fromDefaultedBytes (ArraySegment([||])) fid (ArraySegment([||]))

    [<Fact>]
    let ``Encode Packed Varint`` () =
        ZCB(8)
        |> Encode.fromPackedVarint fid [ 0; 1; 128; 129 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 6uy; 0uy; 1uy; 0x80uy; 0x01uy; 0x81uy; 0x01uy |]
    
    [<Fact>]
    let ``Encode Packed Bool`` () =
        ZCB(5)
        |> Encode.fromPackedBool fid [ false; true; false ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 3uy; 0uy; 1uy; 0uy |]

    [<Fact>]
    let ``Encode Packed SInt32`` () =
        ZCB(5)
        |> Encode.fromPackedSInt32 fid [ 0; -1; 1 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 3uy; 0uy; 1uy; 2uy |]

    [<Fact>]
    let ``Encode Packed SInt64`` () =
        ZCB(5)
        |> Encode.fromPackedSInt64 fid [ 0L; -1L; 1L ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 3uy; 0uy; 1uy; 2uy |]

    [<Fact>]
    let ``Encode Packed Fixed32`` () =
        ZCB(10)
        |> Encode.fromPackedFixed32 fid [ 0u; uint32 -1 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 8uy; 0x00uy;0x00uy;0x00uy;0x00uy; 0xFFuy;0xFFuy;0xFFuy;0xFFuy |]

    [<Fact>]
    let ``Encode Packed Fixed64`` () =
        ZCB(18)
        |> Encode.fromPackedFixed64 fid [ 0UL; uint64 -1 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 16uy; 0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; 0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy |]

    [<Fact>]
    let ``Encode Packed SFixed32`` () =
        ZCB(10)
        |> Encode.fromPackedSFixed32 fid [ 0; -1 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 8uy; 0x00uy;0x00uy;0x00uy;0x00uy; 0xFFuy;0xFFuy;0xFFuy;0xFFuy |]
        

    [<Fact>]
    let ``Encode Packed SFixed64`` () =
        ZCB(18)
        |> Encode.fromPackedSFixed64 fid [ 0L; -1L ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 16uy; 0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; 0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy;0xFFuy |]

    [<Fact>]
    let ``Encode Packed Single`` () =
        ZCB(10)
        |> Encode.fromPackedSingle fid [ 0.0f; 2.0f ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 8uy; 0x00uy;0x00uy;0x00uy;0x00uy; 0uy;0uy;0b00000000uy;0b01000000uy |]
        

    [<Fact>]
    let ``Encode Packed Double`` () =
        ZCB(18)
        |> Encode.fromPackedDouble fid [ 0.0; 0.10 ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 16uy; 0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; 0x9Auy;0x99uy;0x99uy;0x99uy;0x99uy;0x99uy;0xB9uy;0x3Fuy |]

    [<Fact>]
    let ``Encode Repeated Field`` () =
        ZCB(12)
        |> Encode.fromRepeated Encode.fromString fid [ "0ABC"; "CBA0" ]
        |> toArray
        |> should equal [| 0x08uy ||| 2uy; 4uy; 0x30uy; 0x41uy; 0x42uy; 0x43uy;
                           0x08uy ||| 2uy; 4uy; 0x43uy; 0x42uy; 0x41uy; 0x30uy |]


