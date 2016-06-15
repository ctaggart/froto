namespace TestWireFormat

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
        |> Unpack.fromVarint
        |> should equal 1UL

        [| 0b10101100uy; 0b00000010uy |]
        |> ZCR
        |> Unpack.fromVarint
        |> should equal 300UL

    [<Fact>]
    let ``Unpack Varint stops after 64 bits of bytes`` () =
        [| 0x81uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x00uy |]
        |> ZCR
        |> Unpack.fromVarint
        |> should equal 1UL

        fun () ->
            [| 0x81uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x00uy |]
            |> ZCR
            |> Unpack.fromVarint
            |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Can unpack max Varint`` () =
        [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x01uy |]
        |> ZCR
        |> Unpack.fromVarint
        |> should equal System.UInt64.MaxValue

    [<Fact>]
    let ``Unpack varint ignores overflow`` () =
        // TODO: Should this really throw an error?
        // That would add another IF statment to the inner Unpack.from loop of every varint...
        [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy |]
        |> ZCR
        |> Unpack.fromVarint
        |> should equal System.UInt64.MaxValue


    [<Fact>]
    let ``Varint throws if buffer too small`` () =
        fun () ->
            [| 0x80uy; 0x80uy; 0x80uy |]
            |> ZCR
            |> Unpack.fromVarint
            |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Can decode fixed`` () =
        [| 0x01uy; 0x20uy; 0x03uy; 0x40uy |]
        |> ZCR
        |> Unpack.fromFixed32
        |> should equal 0x40032001u

        [| 0x01uy; 0x20uy; 0x03uy; 0x40uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy |]
        |> ZCR
        |> Unpack.fromFixed64
        |> should equal 0x8000000040032001UL

    [<Fact>]
    let ``Fixed throws if buffer too small`` () =
        fun () ->
            [| 0x01uy; 0x20uy; 0x03uy |]
            |> ZCR
            |> Unpack.fromFixed32
            |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Unpack Single`` () =
        [| 0uy; 0uy; 0b00000000uy; 0b01000000uy |]
        |> ZCR
        |> Unpack.fromSingle
        |> should equal 2.0f

    [<Fact>]
    let ``Unpack Double`` () =
        [| 0x9Auy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0xB9uy; 0x3Fuy |]
        |> ZCR
        |> Unpack.fromDouble
        |> should equal 0.10

    [<Fact>]
    let ``Unpack length delimited`` () =

        // len=3; should not return last byte
        [| 0x03uy; 0x00uy; 0x01uy; 0x02uy; 0x00uy |]
        |> ZCR
        |> Unpack.fromLengthDelimited
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
        |> Pack.toVarint 0x01UL
        |> toArray
        |> should equal [| 0x01uy |]

    [<Fact>]
    let ``Pack two-byte varint`` () =
        ZCW(2)
        |> Pack.toVarint 0x81UL
        |> toArray
        |> should equal [| 0x81uy; 0x01uy |]

    [<Fact>]
    let ``Pack max-byte varint`` () =
        ZCW(10)
        |> Pack.toVarint 0x8000000000000000UL
        |> toArray
        |> should equal [| 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x01uy |]

    [<Fact>]
    let ``Pack max-value varint`` () =
        ZCW(10)
        |> Pack.toVarint System.UInt64.MaxValue
        |> toArray
        |> should equal [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x01uy |]

    [<Fact>]
    let ``Pack Fixed32`` () =
        ZCW(4)
        |> Pack.toFixed32 0x80000001u
        |> toArray
        |> should equal [| 0x01uy; 0x00uy; 0x00uy; 0x80uy |]

    [<Fact>]
    let ``Pack Fixed64`` () =
        ZCW(8)
        |> Pack.toFixed64 0x8000000000000001UL
        |> toArray
        |> should equal [| 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy |]

    [<Fact>]
    let ``Pack Single and Double`` () =
        ZCW(4)
            |> Pack.toSingle 2.0f
            |> ZeroCopyBuffer
            |> Unpack.fromSingle
            |> should equal 2.0f

        ZCW(8)
            |> Pack.toDouble 0.10
            |> ZeroCopyBuffer
            |> Unpack.fromDouble
            |> should equal 0.10

    [<Fact>]
    let ``Pack length delimited`` () =

        let src = [| 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
        let len = src.Length

        ZCW(256)
        |> Pack.toLengthDelimited
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
        |> Unpack.fromTag
        |> should equal (1, WireType.Varint)

        [| 0x11uy |]
        |> ZCR
        |> Unpack.fromTag
        |> should equal (2, WireType.Fixed64)

        [| 0xD2uy; 0x02uy |]
        |> ZCR
        |> Unpack.fromTag
        |> should equal (42, WireType.LengthDelimited )


    [<Fact>]
    let ``Tag validated to range [1,2^28)`` () =
        
        fun () ->
            [| 0x08uy |]
            |> ZCR
            |> Unpack.fromTag
            |> ignore
        |> should not' (throw typeof<WireFormatException>)

        fun () ->
            [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x08uy |]
            |> ZCR
            |> Unpack.fromTag
            |> ignore
        |> should not' (throw typeof<WireFormatException>)

        fun () ->
            [| 0x00uy |]
            |> ZCR
            |> Unpack.fromTag
            |> ignore
        |> should throw typeof<WireFormatException>

        fun () ->
            [| 0x80uy; 0x80uy; 0x80uy; 0x80uy; 0x10uy |]
            |> ZCR
            |> Unpack.fromTag
            |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Unpack varint field`` () =
        [| 0x08uy; 2uy |]
        |> ZCR
        |> Unpack.fromField
        |> should equal (Varint (1, 2UL))

    [<Fact>]
    let ``Unpack fixed64 field`` () =
        [| 0x09uy; 0x00uy;0x00uy;0x00uy;0x00uy; 0x00uy;0x01uy;0x02uy;0x03uy |]
        |> ZCR
        |> Unpack.fromField
        |> should equal (Fixed64 (1, 0x0302010000000000UL))

    [<Fact>]
    let ``Unpack length delimited field`` () =
        let field =
            [| 0x1Auy; 0x03uy; 0x00uy;0x00uy;0x01uy; 0x00uy;0x00uy;0x01uy;0x02uy;0x03uy |]
            |> ZCR
            |> Unpack.fromField

        match field with
        | LengthDelimited (num, seg) ->
            seg.ToArray()
            |> should equal [| 0uy; 0uy; 1uy |]
        | _ -> failwithf "Expected: LengthDelimited; Found: %A" field

    [<Fact>]
    let ``Unpack fixed32 field`` () =
        [| byte ((9<<<3) ||| 5); 0x00uy;0x01uy;0x02uy;0x03uy |]
        |> ZCR
        |> Unpack.fromField
        |> should equal (Fixed32 (9, 0x03020100u))

    [<Fact>]
    let ``Unpack StartGroup or EndGroup throws`` () =
        fun () ->
            [| byte ((1<<<3) ||| 3) |]
            |> ZCR
            |> Unpack.fromField
            |> ignore
        |> should throw typeof<WireFormatException>

        fun () ->
            [| byte ((1<<<3) ||| 4) |]
            |> ZCR
            |> Unpack.fromField
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
        |> Pack.toTag 2 WireType.Fixed64
        |> toArray
        |> should equal [| 0x11uy |]

    [<Fact>]
    let ``Field number validated to range [1, 2^28)`` () =
        let buf = ZCW(256)

        fun () -> buf |> Pack.toTag 1 WireType.Fixed64 |> ignore
        |> should not' (throw typeof<WireFormatException>)

        fun () -> buf |> Pack.toTag RawField.MaxFieldNum WireType.Fixed64 |> ignore
        |> should not' (throw typeof<WireFormatException>)

        fun () -> buf |> Pack.toTag 0 WireType.Fixed64 |> ignore
        |> should throw typeof<WireFormatException>

        fun () -> buf |> Pack.toTag (RawField.MaxFieldNum+1) WireType.Fixed64 |> ignore
        |> should throw typeof<WireFormatException>

        fun () -> buf |> Pack.toTag -1 WireType.Fixed64 |> ignore
        |> should throw typeof<WireFormatException>

        fun () -> buf |> Pack.toTag (Int32.MinValue) WireType.Fixed64 |> ignore
        |> should throw typeof<WireFormatException>

    [<Fact>]
    let ``Pack varint field`` () =
        ZCW(256)
        |> Pack.toFieldVarint 2 42UL 
        |> toArray
        |> should equal [| 0x10uy; 42uy |]

    [<Fact>]
    let ``Pack fixed64 field`` () =
        ZCW(256)
        |> Pack.toFieldFixed64 2 42UL 
        |> toArray
        |> should equal [| 0x11uy; 42uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]

    [<Fact>]
    let ``Pack length delimited field`` () =
        let src = [| 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
        let len = src.Length

        ZCW(256)
        |> Pack.toFieldLengthDelimited 2
            (uint32 len)
            (fun dest-> Array.Copy(src, 0L, dest.Array, int64 dest.Offset, int64 len) )
        |> toArray
        |> should equal [| 0x12uy; 0x05uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]

    let ``Pack bytes field`` () =
        let src = [| for i in 1..64 -> byte i|]

        ZCW(256)
        |> Pack.toFieldBytes 2 (ArraySegment(src))
        |> toArray
        |> should equal (Array.append [| 0x12uy; 64uy |] src)

    let ``Pack string field`` () =
        let src = "123"

        ZCW(256)
        |> Pack.toFieldString 2 src
        |> toArray
        |> should equal [| 0x12uy; 3uy; 0x31uy; 0x32uy; 0x33uy |]

    [<Fact>]
    let ``Pack fixed32 field`` () =
        ZCW(256)
        |> Pack.toFieldFixed32 2 42u 
        |> toArray
        |> should equal [| 0x15uy; 42uy; 0uy; 0uy; 0uy|]

    [<Fact>]
    let ``Pack RawField`` () =

        // Varint
        ZCW(256)
        |> Pack.toFieldRaw (Varint(2, 42UL))
        |> toArray
        |> should equal [| 0x10uy; 42uy |]

        // Fixed32
        ZCW(256)
        |> Pack.toFieldRaw (Fixed32(2, 42u))
        |> toArray
        |> should equal [| 0x15uy; 42uy; 0uy; 0uy; 0uy |]

        // Fixed64
        ZCW(256)
        |> Pack.toFieldRaw (Fixed64(2, 42UL)) 
        |> toArray
        |> should equal [| 0x11uy; 42uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]

        // LengthDelimited
        ZCW(256)
        |> Pack.toFieldRaw (LengthDelimited(2, ArraySegment([| 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |])))
        |> toArray
        |> should equal [| 0x12uy; 0x05uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]

