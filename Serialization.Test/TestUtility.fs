namespace TestUtility

open Xunit
open FsUnit.Xunit

[<Xunit.Trait("Kind", "Unit")>]
module Utility =
    open Froto.Serialization.Encoding.Utility

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
