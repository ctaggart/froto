module ExampleProtoRecord

open Froto.Core
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

type ETest =
    | Nada = 0
    | One = 1
    | Two = 2


type InnerRecord =
    {
        Id : int32
        Name : string
        Option : bool
        Test : ETest
        PackedFixed32 : int32 list
        RepeatedInt32 : int32 list
    }
    with
        static member Deserialize( zcb : ZeroCopyBuffer ) =
            let mutable id = 0
            let mutable name = ""
            let mutable option = false
            let mutable test = ETest.Nada
            let mutable packedFixed32 = List.empty
            let mutable repeatedInt32 = List.empty

            let decoder_ring =
                [
                    1, Serializer.hydrateInt32 &id
                    2, Serializer.hydrateString &name
                    3, Serializer.hydrateBool &option
                    4, Serializer.hydrateEnum &test
                    5, Serializer.hydratePackedFixed32 &packedFixed32
                    6, Serializer.hydrateRepeated Serializer.hydrateInt32Delegate &repeatedInt32
                ]
                |> Map.ofList

            ()


type InnerMessage () =
    inherit MessageBase()
    let ETestDefault = ETest.One  // NOTE: Non-zero default is only supported in Proto2
    let mutable m_id = 0
    let mutable m_name = ""
    let mutable m_option = false
    let mutable m_test = ETestDefault
    let mutable m_packedFixed32 = List.empty
    let mutable m_repeatedInt32 = List.empty

    let m_decoderRing =
        [
            1, Serializer.hydrateInt32 &m_id
            2, Serializer.hydrateString &m_name
            3, Serializer.hydrateBool &m_option
            4, Serializer.hydrateEnum &m_test
            5, Serializer.hydratePackedFixed32 &m_packedFixed32
            6, Serializer.hydrateRepeated Serializer.hydrateInt32Delegate &m_repeatedInt32
        ]
        |> Map.ofList

    member x.ID             with get() = m_id and set(v) = m_id <- v
    member x.Name           with get() = m_name and set(v) = m_name <- v
    member x.bOption        with get() = m_option and set(v) = m_option <- v
    member x.Test           with get() = m_test and set(v) = m_test <- v
    member x.PackedFixed32  with get() = m_packedFixed32 and set(v) = m_packedFixed32 <- v
    member x.RepeatedInt32  with get() = m_repeatedInt32 and set(v) = m_repeatedInt32 <- v

    override x.Clear() =
        m_id <- 0
        m_name <- ""
        m_option <- false
        m_test <- ETestDefault
        m_packedFixed32 <- List.empty
        m_repeatedInt32 <- List.empty

    override x.DecoderRing = m_decoderRing
    override x.Encode(zcb) =
        let encode =
            (Serializer.dehydrateVarint 1 m_id) >>
            (Serializer.dehydrateString 2 m_name) >>
            (Serializer.dehydrateBool 3 m_option) >>
            (Serializer.dehydrateDefaultedVarint ETestDefault 4 m_test) >>
            (Serializer.dehydratePackedFixed32 5 m_packedFixed32) >>
            (Serializer.dehydrateRepeated Serializer.dehydrateVarint 6 m_repeatedInt32)
        encode zcb
        
    static member FromArraySegment (buf:System.ArraySegment<byte>) =
        let self = InnerMessage()
        self.Merge(buf) |> ignore
        self

type OuterMessage() =
    inherit MessageBase()
    let mutable m_id = 0
    let mutable m_inner = None
    let mutable m_hasMore = false

    member x.Id with get() = m_id and set(v) = m_id <- v
    member x.Inner with get() = m_inner and set(v) = m_inner <- v
    member x.HasMore with get() = m_hasMore and set(v) = m_hasMore <- v

    override x.Clear() =
        m_id <- 0
        m_inner <- None
        m_hasMore <- false

    override x.DecoderRing =
        [
             1, Serializer.hydrateInt32 &m_id;
            42, Serializer.hydrateOptionalMessage (InnerMessage.FromArraySegment) &m_inner;
            43, Serializer.hydrateBool &m_hasMore;
        ]
        |> Map.ofList

    override x.Encode(zcb) =
        let encode =
            (Serializer.dehydrateVarint 1 m_id ) >>
            (Serializer.dehydrateOptionalMessage 42 m_inner) >>
            (Serializer.dehydrateBool 43 m_hasMore)
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
                for id = 1 to 1000 do
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

