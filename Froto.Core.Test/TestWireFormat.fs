﻿namespace TestWireFormat

open Xunit
open FsUnit.Xunit

open System

open Froto.Serialization
open Froto.Serialization.Encoding
open Froto.Serialization.Encoding.WireFormat

module Helpers =
    type System.ArraySegment<'a>
        with member x.ToArray() =
                        x.Array.[ x.Offset .. x.Offset + x.Count - 1]

[<Xunit.Trait("Kind", "Unit")>]
module UnpackValue =
    open Helpers

    type ZCR = ZeroCopyBuffer

    let toArray (a:ArraySegment<byte>) =
        a.ToArray()

    [<Fact>]
    let ``Can unpack a varint`` () =

        [| 0b00000001uy |]
        |> ZCR
        |> unpackVarint
        |> should equal 1UL

        [| 0b10101100uy; 0b00000010uy |]
        |> ZCR
        |> unpackVarint
        |> should equal 300UL

    [<Fact>]
    let ``Unpack Varint stops after 64 bits of bytes`` () =
        [| 0x81uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x00uy |]
        |> ZCR
        |> unpackVarint
        |> should equal 1UL

        fun () ->
            [| 0x81uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x00uy |]
            |> ZCR
            |> unpackVarint
            |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Can unpack max Varint`` () =
        [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x01uy |]
        |> ZCR
        |> unpackVarint
        |> should equal System.UInt64.MaxValue

    [<Fact>]
    let ``Unpack varint ignores overflow`` () =
        // TODO: Should this really throw an error?
        // That would add another IF statment to the inner unpack loop of every varint...
        [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy |]
        |> ZCR
        |> unpackVarint
        |> should equal System.UInt64.MaxValue


    [<Fact>]
    let ``Varint throws if buffer too small`` () =
        fun () ->
            [| 0x80uy; 0x80uy; 0x80uy |]
            |> ZCR
            |> unpackVarint
            |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Can decode fixed`` () =
        [| 0x01uy; 0x20uy; 0x03uy; 0x40uy |]
        |> ZCR
        |> unpackFixed32
        |> should equal 0x40032001u

        [| 0x01uy; 0x20uy; 0x03uy; 0x40uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy |]
        |> ZCR
        |> unpackFixed64
        |> should equal 0x8000000040032001UL

    [<Fact>]
    let ``Fixed throws if buffer too small`` () =
        fun () ->
            [| 0x01uy; 0x20uy; 0x03uy |]
            |> ZCR
            |> unpackFixed32
            |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Unpack Single`` () =
        [| 0uy; 0uy; 0b00000000uy; 0b01000000uy |]
        |> ZCR
        |> unpackSingle
        |> should equal 2.0f

    [<Fact>]
    let ``Unpack Double`` () =
        [| 0x9Auy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0xB9uy; 0x3Fuy |]
        |> ZCR
        |> unpackDouble
        |> should equal 0.10

    [<Fact>]
    let ``Unpack length delimited`` () =

        // len=3; should not return last byte
        [| 0x03uy; 0x00uy; 0x01uy; 0x02uy; 0x00uy |]
        |> ZCR
        |> unpackLengthDelimited
        |> toArray
        |> should equal [| 00uy; 01uy; 02uy |]

[<Xunit.Trait("Kind", "Unit")>]
module PackValue =
    open Helpers

    type ZCW = ZeroCopyBuffer

    let toArray (a:ZCW) =
        a.ToArray()

    [<Fact>]
    let ``Pack one-byte varint`` () =
        ZCW(2)
        |> packVarint 0x01UL
        |> toArray
        |> should equal [| 0x01uy |]

    [<Fact>]
    let ``Pack two-byte varint`` () =
        ZCW(2)
        |> packVarint 0x81UL
        |> toArray
        |> should equal [| 0x81uy; 0x01uy |]

    [<Fact>]
    let ``Pack max-byte varint`` () =
        ZCW(10)
        |> packVarint 0x8000000000000000UL
        |> toArray
        |> should equal [| 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x01uy |]

    [<Fact>]
    let ``Pack max-value varint`` () =
        ZCW(10)
        |> packVarint System.UInt64.MaxValue
        |> toArray
        |> should equal [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x01uy |]

    [<Fact>]
    let ``Pack Fixed32`` () =
        ZCW(4)
        |> packFixed32 0x80000001u
        |> toArray
        |> should equal [| 0x01uy; 0x00uy; 0x00uy; 0x80uy |]

    [<Fact>]
    let ``Pack Fixed64`` () =
        ZCW(8)
        |> packFixed64 0x8000000000000001UL
        |> toArray
        |> should equal [| 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy |]

    [<Fact>]
    let ``Pack Single and Double`` () =
        ZCW(4)
            |> packSingle 2.0f
            |> ZeroCopyBuffer
            |> unpackSingle
            |> should equal 2.0f

        ZCW(8)
            |> packDouble 0.10
            |> ZeroCopyBuffer
            |> unpackDouble
            |> should equal 0.10

    [<Fact>]
    let ``Pack length delimited`` () =

        let src = [| 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
        let len = src.Length

        ZCW(256)
        |> packLengthDelimited
            (uint32 len)
            (fun dest-> Array.Copy(src, 0L, dest.Array, int64 dest.Offset, int64 len) )
        |> toArray
        |> should equal [| 0x05uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]

[<Xunit.Trait("Kind", "Unit")>]
module UnpackField =
    open Helpers

    type ZCR = ZeroCopyBuffer

    [<Fact>]
    let ``Unpack tag`` () =
        
        [| 0x08uy |]
        |> ZCR
        |> unpackTag
        |> should equal (1, WireType.Varint)

        [| 0x11uy |]
        |> ZCR
        |> unpackTag
        |> should equal (2, WireType.Fixed64)

        [| 0xD2uy; 0x02uy |]
        |> ZCR
        |> unpackTag
        |> should equal (42, WireType.LengthDelimited )


    [<Fact>]
    let ``Tag validated to range [1,2^28)`` () =
        
        fun () ->
            [| 0x08uy |]
            |> ZCR
            |> unpackTag
            |> ignore
        |> should not' (throw typeof<WireFormatException>)

        fun () ->
            [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x08uy |]
            |> ZCR
            |> unpackTag
            |> ignore
        |> should not' (throw typeof<WireFormatException>)

        fun () ->
            [| 0x00uy |]
            |> ZCR
            |> unpackTag
            |> ignore
        |> should throw typeof<WireFormatException>

        fun () ->
            [| 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x10uy |]
            |> ZCR
            |> unpackTag
            |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Unpack varint field`` () =
        [| 0x08uy; 2uy |]
        |> ZCR
        |> unpackField
        |> should equal (Varint (1, 2UL))

    [<Fact>]
    let ``Unpack fixed64 field`` () =
        [| 0x09uy; 0x00uy;0x00uy;0x00uy;0x00uy; 0x00uy;0x01uy;0x02uy;0x03uy |]
        |> ZCR
        |> unpackField
        |> should equal (Fixed64 (1, 0x0302010000000000UL))

    [<Fact>]
    let ``Unpack length delimited field`` () =
        let field =
            [| 0x1Auy; 0x03uy; 0x00uy;0x00uy;0x01uy; 0x00uy;0x00uy;0x01uy;0x02uy;0x03uy |]
            |> ZCR
            |> unpackField

        match field with
        | LengthDelimited (num, seg) ->
            seg.ToArray()
            |> should equal [| 0uy; 0uy; 1uy |]
        | _ -> failwithf "Expected: LengthDelimited; Found: %A" field

    [<Fact>]
    let ``Unpack fixed32 field`` () =
        [| byte ((9<<<3) ||| 5); 0x00uy;0x01uy;0x02uy;0x03uy |]
        |> ZCR
        |> unpackField
        |> should equal (Fixed32 (9, 0x03020100u))

    [<Fact>]
    let ``Unpack StartGroup or EndGroup throws`` () =
        fun () ->
            [| byte ((1<<<3) ||| 3) |]
            |> ZCR
            |> unpackField
            |> ignore
        |> should throw typeof<WireFormatException>

        fun () ->
            [| byte ((1<<<3) ||| 4) |]
            |> ZCR
            |> unpackField
            |> ignore
        |> should throw typeof<WireFormatException>


[<Xunit.Trait("Kind", "Unit")>]
module PackField =
    open Helpers

    type ZCW = ZeroCopyBuffer

    let toArray (a:ZCW) = a.ToArray()

    [<Fact>]
    let ``Pack tag`` () =
        ZCW(256)
        |> packTag 2 WireType.Fixed64
        |> toArray
        |> should equal [| 0x11uy |]

    [<Fact>]
    let ``Field number validated to range [1, 2^28)`` () =
        let buf = ZCW(256)

        fun () -> buf |> packTag 1 WireType.Fixed64 |> ignore
        |> should not' (throw typeof<WireFormatException>)

        fun () -> buf |> packTag RawField.MaxFieldNum WireType.Fixed64 |> ignore
        |> should not' (throw typeof<WireFormatException>)

        fun () -> buf |> packTag 0 WireType.Fixed64 |> ignore
        |> should throw typeof<WireFormatException>

        fun () -> buf |> packTag (RawField.MaxFieldNum+1) WireType.Fixed64 |> ignore
        |> should throw typeof<WireFormatException>

        fun () -> buf |> packTag -1 WireType.Fixed64 |> ignore
        |> should throw typeof<WireFormatException>

        fun () -> buf |> packTag (Int32.MinValue) WireType.Fixed64 |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Pack varint field`` () =
        ZCW(256)
        |> packFieldVarint 2 42UL 
        |> toArray
        |> should equal [| 0x10uy; 42uy |]

    [<Fact>]
    let ``Pack fixed64 field`` () =
        ZCW(256)
        |> packFieldFixed64 2 42UL 
        |> toArray
        |> should equal [| 0x11uy; 42uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]

    [<Fact>]
    let ``Pack length delimited field`` () =
        let src = [| 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
        let len = src.Length

        ZCW(256)
        |> packFieldLengthDelimited 2
            (uint32 len)
            (fun dest-> Array.Copy(src, 0L, dest.Array, int64 dest.Offset, int64 len) )
        |> toArray
        |> should equal [| 0x12uy; 0x05uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]

    let ``Pack bytes field`` () =
        let src = [| for i in 1..64 -> byte i|]

        ZCW(256)
        |> packFieldBytes 2 (ArraySegment(src))
        |> toArray
        |> should equal (Array.append [| 0x12uy; 64uy |] src)

    let ``Pack string field`` () =
        let src = "123"

        ZCW(256)
        |> packFieldString 2 src
        |> toArray
        |> should equal [| 0x12uy; 3uy; 0x31uy; 0x32uy; 0x33uy |]

    [<Fact>]
    let ``Pack fixed32 field`` () =
        ZCW(256)
        |> packFieldFixed32 2 42u 
        |> toArray
        |> should equal [| 0x15uy; 42uy; 0uy; 0uy; 0uy|]

    [<Fact>]
    let ``Pack RawField`` () =

        // Varint
        ZCW(256)
        |> packFieldRaw (Varint(2, 42UL))
        |> toArray
        |> should equal [| 0x10uy; 42uy |]

        // Fixed32
        ZCW(256)
        |> packFieldRaw (Fixed32(2, 42u))
        |> toArray
        |> should equal [| 0x15uy; 42uy; 0uy; 0uy; 0uy |]

        // Fixed64
        ZCW(256)
        |> packFieldRaw (Fixed64(2, 42UL)) 
        |> toArray
        |> should equal [| 0x11uy; 42uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]

        // LengthDelimited
        ZCW(256)
        |> packFieldRaw (LengthDelimited(2, ArraySegment([| 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |])))
        |> toArray
        |> should equal [| 0x12uy; 0x05uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]

