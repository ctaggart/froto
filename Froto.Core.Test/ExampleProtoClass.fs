module SampleProto

open Froto.Core
open Froto.Core.WireFormat
open Froto.Core.Encoding

type Test () =

    inherit MessageBase()

    let m_id = ref 0
    let m_name = ref ""
    let m_option = ref false
    let m_test = ref ETest.Nada
    let m_packedFixed32 = ref List.empty
    let m_repeatedInt32 = ref List.empty

    override x.DecoderRing =
        [
            1, m_id             |> Serializer.hydrateInt32
            2, m_name           |> Serializer.hydrateString
            3, m_option         |> Serializer.hydrateBool
            4, m_test           |> Serializer.hydrateEnum
            5, m_packedFixed32  |> Serializer.hydratePackedFixed32
            6, m_repeatedInt32  |> Serializer.hydrateRepeated Serializer.hydrateInt32
        ]
        |> Map.ofList

    override x.EncoderRing =
        [
            !m_id            |> Serializer.dehydrateVarint 1
            !m_name          |> Serializer.dehydrateString 2
            !m_option        |> Serializer.dehydrateBool 3
            !m_test          |> Serializer.dehydrateVarint 4
            !m_packedFixed32 |> Serializer.dehydratePackedFixed32 5
            !m_repeatedInt32 |> Serializer.dehydrateRepeated Serializer.dehydrateVarint 6
        ]

    static member Deserialize buf =
        let self = Test()
        self.Deserialize(buf) |> ignore
        self

    static member DeserializeLengthDelimited buf =
        let self = Test()
        self.DeserializeLengthDelimited(buf) |> ignore
        self
        
    member x.ID = !m_id
    member x.Name = !m_name
    member x.bOption = !m_option
    member x.Test = !m_test
    member x.PackedFixed32 = !m_packedFixed32
    member x.RepeatedInt32 = !m_repeatedInt32

and ETest =
    | Nada = 0
    | One = 1
    | Two = 2

type OuterTest() =
    inherit MessageBase()

    let m_inner = ref <| Test()

    override x.DecoderRing =
        [
            42, m_inner |> Serializer.hydrateMessage (Test.Deserialize)
        ]
        |> Map.ofList

    override x.EncoderRing =
        [
            !m_inner |> Serializer.dehydrateMessage 42
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