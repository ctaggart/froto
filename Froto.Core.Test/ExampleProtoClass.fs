module SampleProto

open Froto.Core.Encoding

(*
    // Example proto definition

    syntax = "proto3";

    message InnerMessage {
        int32 id = 1;
        string name = 2;
        bool option = 3;
        ETest test = 4; // note: forward reference
        repeated fixed32 packedFixed32 = 5 [packed = true];
        repeated int32 repeatedInt32 = 6;
        }

    enum ETest {
        Nada = 0;
        One = 1;
        Two = 2;
        }

    message OuterMessage {
        int32 id = 1;
        InnerMessage inner = 42;
        bool has_more = 43;
        }
 *)

type InnerMessage () =
    inherit MessageBase()
    let ETestDefault = ETest.One  // NOTE: Non-zero default is only supported in Proto2
    let m_id = ref 0
    let m_name = ref ""
    let m_option = ref false
    let m_test = ref ETestDefault
    let m_packedFixed32 = ref List.empty
    let m_repeatedInt32 = ref List.empty

    let m_decoderRing =
        [
            1, m_id             |> ClassSerializer.hydrateInt32
            2, m_name           |> ClassSerializer.hydrateString
            3, m_option         |> ClassSerializer.hydrateBool
            4, m_test           |> ClassSerializer.hydrateEnum
            5, m_packedFixed32  |> ClassSerializer.hydratePackedFixed32
            6, m_repeatedInt32  |> ClassSerializer.hydrateOneRepeatedInstance ClassSerializer.hydrateInt32
        ]
        |> Map.ofList

    member x.ID             with get() = !m_id and set(v) = m_id := v
    member x.Name           with get() = !m_name and set(v) = m_name := v
    member x.bOption        with get() = !m_option and set(v) = m_option := v
    member x.Test           with get() = !m_test and set(v) = m_test := v
    member x.PackedFixed32  with get() = !m_packedFixed32 and set(v) = m_packedFixed32 := v
    member x.RepeatedInt32  with get() = !m_repeatedInt32 and set(v) = m_repeatedInt32 := v

    override x.Clear() =
        m_id := 0
        m_name := ""
        m_option := false
        m_test := ETestDefault
        m_packedFixed32 := List.empty
        m_repeatedInt32 := List.empty

    override x.DecoderRing = m_decoderRing

    override x.Encode(zcb) =
        let encode =
            (!m_id            |> ClassSerializer.dehydrateVarint 1) >>
            (!m_name          |> ClassSerializer.dehydrateString 2) >>
            (!m_option        |> ClassSerializer.dehydrateBool 3) >>
            (!m_test          |> ClassSerializer.dehydrateDefaultedVarint ETestDefault 4) >>
            (!m_packedFixed32 |> ClassSerializer.dehydratePackedFixed32 5) >>
            (!m_repeatedInt32 |> ClassSerializer.dehydrateRepeated ClassSerializer.dehydrateVarint 6)
        encode zcb
        
    static member FromArraySegment (buf:System.ArraySegment<byte>) =
        let self = InnerMessage()
        self.Merge(buf) |> ignore
        self

and ETest =
    | Nada = 0
    | One = 1
    | Two = 2

type OuterMessage() =
    inherit MessageBase()
    let m_id = ref 0
    let m_inner = ref None
    let m_hasMore = ref false

    let m_decoderRing =
        [
             1, m_id |> ClassSerializer.hydrateInt32;
            42, m_inner |> ClassSerializer.hydrateOptionalMessage (InnerMessage.FromArraySegment);
            43, m_hasMore |> ClassSerializer.hydrateBool;
        ]
        |> Map.ofList

    member x.Id with get() = !m_id and set(v) = m_id := v
    member x.Inner with get() = !m_inner and set(v) = m_inner := v
    member x.HasMore with get() = !m_hasMore and set(v) = m_hasMore := v

    override x.Clear() =
        m_id := 0
        m_inner := None
        m_hasMore := false

    override x.DecoderRing = m_decoderRing

    override x.Encode(zcb) =
        let encode =
            (!m_id |> ClassSerializer.dehydrateVarint 1) >>
            (!m_inner |> ClassSerializer.dehydrateOptionalMessage 42) >>
            (!m_hasMore |> ClassSerializer.dehydrateBool 43)
        encode zcb

    static member FromArraySegment (buf:System.ArraySegment<byte>) =
        let self = OuterMessage()
        self.Merge(buf) |> ignore
        self

module PerformanceTest =

    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Test Serialization and Deserialization Performance`` () =
        let xs =
            [
                for id = 1 to 1000000 do
                    let inner = InnerMessage()
                    inner.ID <- 1
                    inner.Name <- "Jerry Smith"
                    inner.bOption <- true
                    inner.Test <- ETest.Two
                    inner.PackedFixed32 <- [1u; 2u; 3u; 4u]
                    inner.RepeatedInt32 <- [5;6;7;8;9]
                    yield inner
            ]

        let len =
            xs |> List.sumBy (fun x -> x.SerializedLengthDelimitedLength)

        let buf : byte array = Array.zeroCreate (len |> int)
        let bufAS = System.ArraySegment(buf)
        let zcw = Froto.Core.ZeroCopyBuffer(bufAS)

        xs
        |> List.iter (fun x -> zcw |> x.SerializeLengthDelimited |> ignore)

        let ys =
            let zcr = Froto.Core.ZeroCopyBuffer(zcw.Array)
            seq {
                while not zcr.IsEof do
                    let m = InnerMessage()
                    m.DeserializeLengthDelimited(zcr) |> ignore
                    yield m
            }

        ys |> Seq.iter ignore

        1 |> should equal 1

