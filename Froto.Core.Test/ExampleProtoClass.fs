module SampleProto

open Froto.Core.Encoding

type InnerMessage () =
    inherit MessageBase()
    let m_id = ref 0
    let m_name = ref ""
    let m_option = ref false
    let m_test = ref ETest.Nada
    let m_packedFixed32 = ref List.empty
    let m_repeatedInt32 = ref List.empty

    member x.ID             with get() = !m_id and set(v) = m_id := v
    member x.Name           with get() = !m_name and set(v) = m_name := v
    member x.bOption        with get() = !m_option and set(v) = m_option := v
    member x.Test           with get() = !m_test and set(v) = m_test := v
    member x.PackedFixed32  with get() =  !m_packedFixed32 and set(v) = m_packedFixed32 := v
    member x.RepeatedInt32  with get() = !m_repeatedInt32 and set(v) = m_repeatedInt32 := v

    override x.Clear() =
        m_id := 0
        m_name := ""
        m_option := false
        m_test := ETest.Nada
        m_packedFixed32 := List.empty
        m_repeatedInt32 := List.empty

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

    override x.EncoderRing(zcb) =
        let encode =
            (!m_id            |> Serializer.dehydrateVarint 1) >>
            (!m_name          |> Serializer.dehydrateString 2) >>
            (!m_option        |> Serializer.dehydrateBool 3) >>
            (!m_test          |> Serializer.dehydrateVarint 4) >>
            (!m_packedFixed32 |> Serializer.dehydratePackedFixed32 5) >>
            (!m_repeatedInt32 |> Serializer.dehydrateRepeated Serializer.dehydrateVarint 6)
        encode zcb
        
    static member FromArraySegment (buf:System.ArraySegment<byte>) =
        let self = InnerMessage()
        self.Deserialize(buf) |> ignore
        self

and ETest =
    | Nada = 0
    | One = 1
    | Two = 2

type OuterMessage() =
    inherit MessageBase()
    let m_inner = ref <| InnerMessage()

    member x.Inner with get() = !m_inner and set(v) = m_inner := v

    override x.Clear() =
        (!m_inner).Clear()

    override x.DecoderRing =
        [
            42, m_inner |> Serializer.hydrateMessage (InnerMessage.FromArraySegment)
        ]
        |> Map.ofList

    override x.EncoderRing(zcb) =
        let encode =
            (!m_inner |> Serializer.dehydrateMessage 42)
        encode zcb

    static member FromArraySegment (buf:System.ArraySegment<byte>) =
        let self = OuterMessage()
        self.Deserialize(buf) |> ignore
        self

