module TestProto

open Froto.Core
open Froto.Core.WireFormat
open Froto.Core.Encoding



type ETest =
    | Nada = 0
    | One = 1
    | Two = 2


type Test () =

    inherit MessageBase()

    let mutable m_id = 0
    let mutable m_name = ""
    let mutable m_option = false
    let mutable m_test = ETest.Nada
    let mutable m_packedFixed32 = Array.empty
    let mutable m_repeatedInt32 = List.empty

    override x.DecoderRing =
        [
            1, ref m_id             |> Serializer.hydrateInt32
            2, ref m_name           |> Serializer.hydrateString
            3, ref m_option         |> Serializer.hydrateBool
            4, ref m_test           |> Serializer.hydrateEnum
            5, ref m_packedFixed32  |> Serializer.hydratePackedFixed32
            6, ref m_repeatedInt32  |> Serializer.hydrateRepeated Serializer.hydrateInt32
        ]
        |> Map.ofList

    override x.EncoderRing =
        [
            m_id            |> Serializer.dehydrateVarint 1
            m_name          |> Serializer.dehydrateString 2
            m_option        |> Serializer.dehydrateBool 3
            m_test          |> Serializer.dehydrateVarint 4
            m_packedFixed32 |> Serializer.dehydratePackedFixed32 5
            m_repeatedInt32 |> Serializer.dehydrateRepeated Serializer.dehydrateVarint 6
        ]

    static member Deserialize buf =
        let self = Test()
        self.Deserialize(buf) |> ignore
        self

    static member DeserializeLengthDelimited buf =
        let self = Test()
        self.DeserializeLengthDelimited(buf) |> ignore
        self
        

    member x.ID = m_id
    member x.Name = m_name
    member x.bOption = m_option
    member x.Test = m_test
    member x.PackedFixed32 = m_packedFixed32
    member x.RepeatedInt32 = m_repeatedInt32


type OuterTest() =
    inherit MessageBase()

    let mutable m_inner = Test()

    override x.DecoderRing =
        [
            42, ref m_inner |> Serializer.hydrateMessage (Test.Deserialize)
        ]
        |> Map.ofList

    override x.EncoderRing =
        [
            m_inner |> Serializer.dehydrateMessage 42
        ]

    static member Deserialize buf =
        let self = OuterTest()
        self.Deserialize(buf) |> ignore
        self

    static member DeserializeLengthDelimited buf =
        let self = OuterTest()
        self.DeserializeLengthDelimited(buf) |> ignore
        self

    member x.Inner = m_inner