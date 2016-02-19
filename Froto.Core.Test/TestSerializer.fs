namespace TestSerializer

open Xunit
open FsUnit.Xunit

open Froto.Core
open Froto.Core.Encoding

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
        Assert.True( false ) 

    [<Fact>]
    let ``Optional message set to None`` () =
        Assert.True( false ) 
