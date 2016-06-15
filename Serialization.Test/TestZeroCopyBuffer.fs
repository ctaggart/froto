[<Xunit.Trait("Kind", "Unit")>]
module TestZeroCopyBuffer

open Xunit
open FsUnit.Xunit

open System

open Froto.Serialization

type System.ArraySegment<'a>
    with member x.ToArray() =
                    x.Array.[ x.Offset .. x.Offset + x.Count - 1]


[<Fact>]
let ``Can read bytes`` () =
    let zc = ZeroCopyBuffer( [| 3uy; 2uy; 1uy |] )

    let a = zc.ReadByte()
    a |> should equal 3uy

    let b = zc.ReadByte()
    b |> should equal 2uy

    let c = zc.ReadByte()
    c |> should equal 1uy

    fun () -> zc.ReadByte() |> ignore
    |> should throw typeof<WireFormatException>

[<Fact>]
let ``Can read range`` () =
    let zc = ZeroCopyBuffer([| 3uy; 2uy; 1uy |])

    let r = zc.ReadByteSegment(2u)
    r.Count |> should equal 2
    r.Offset |> should equal 0
    r.ToArray() |> should equal [| 3uy; 2uy |]

    let rem = zc.Remainder
    rem.Offset |> should equal 2
    rem.Count |> should equal 1

    let r = zc.ReadByteSegment(1u)
    r.Count |> should equal 1
    r.Offset |> should equal 2
    r.ToArray() |> should equal [| 1uy |]

    fun () -> zc.ReadByte() |> ignore
    |> should throw typeof<WireFormatException>

[<Fact>]
let ``Can write bytes`` () =
    let zc = ZeroCopyBuffer([| 3uy; 2uy; 1uy |])

    zc.WriteByte(42uy)
    zc.WriteByte(43uy)
    zc.ToArray() |> should equal [| 42uy; 43uy |]

    let arr = zc.AsArraySegment
    arr.Offset |> should equal 0
    arr.Count |> should equal 2

    zc.WriteByte(44uy)

    zc.ToArray() |> should equal [| 42uy; 43uy; 44uy |]

    fun () -> zc.WriteByte(0uy) |> ignore
    |> should throw typeof<WireFormatException>

[<Fact>]
let ``Can write range`` () =

    let xs = [|1uy;2uy;3uy;4uy;5uy|]
    let len = xs.Length
    let emplace (dest:ArraySegment<byte>) =
        Array.Copy( xs, 0L, dest.Array, int64 dest.Offset, int64 len)

    let zc = ZeroCopyBuffer(256)
    zc.WriteByteSegment (uint32 len) emplace

    zc.ToArray()
    |> should equal [| 1uy; 2uy; 3uy; 4uy; 5uy |]

