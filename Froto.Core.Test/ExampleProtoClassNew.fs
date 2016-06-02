module ExampleProtoClassNew

module SampleNamespace =

    open System
    open Froto.Core
    open Froto.Core.RecordModel
    open Froto.Core.Hydration

    type ETest =
        | Nada = 0
        | One = 1
        | Two = 2

    let ETestDefault = ETest.One  // NOTE: Non-zero default is only supported in Proto2
    
    type InnerMessage () as self =
        let ETestDefault = ETest.One  // NOTE: Non-zero default is only supported in Proto2

        member val Id = 0 with get,set
        member val Name = "" with get,set
        member val Option = false with get,set
        member val Test = ETestDefault with get,set
        member val PackedFixed32 = ResizeArray() with get,set
        member val RepeatedInt32 = ResizeArray() with get,set
        member val _UnknownFields = List.empty with get,set
        member val _FoundFields = Set.empty with get,set

        member x.Clear() =
            x.Id <- 0
            x.Name <- ""
            x.Option <- false
            x.Test <- ETestDefault
            x.PackedFixed32 <- ResizeArray()
            x.RepeatedInt32 <- ResizeArray()

        static member Serializer (m:InnerMessage, zcb) =
            (m.Id             |> dehydrateVarint 1) >>
            (m.Name           |> dehydrateString 2) >>
            (m.Option         |> dehydrateBool 3) >>
            (m.Test           |> dehydrateDefaultedVarint ETestDefault 4) >>
            (m.PackedFixed32  |> List.ofSeq |> dehydratePackedFixed32 5) >>
            (m.RepeatedInt32  |> List.ofSeq |> dehydrateRepeated dehydrateVarint 6) >>
            (m._UnknownFields |> dehydrateRawFields )
            <| zcb

        static member DecoderRing = 
            [
                1, fun (m:InnerMessage) rawField -> m.Id          <- hydrateInt32 rawField ; m
                2, fun (m:InnerMessage) rawField -> m.Name        <- hydrateString rawField ; m
                3, fun (m:InnerMessage) rawField -> m.Option      <- hydrateBool rawField ; m
                4, fun (m:InnerMessage) rawField -> m.Test        <- hydrateEnum rawField ; m
                5, fun (m:InnerMessage) rawField -> m.PackedFixed32.Clear()
                                                    m.PackedFixed32.AddRange( List.rev <| hydratePackedFixed32 rawField )
                                                    m
                6, fun (m:InnerMessage) rawField -> m.RepeatedInt32.Add(hydrateInt32 rawField); m
            ]
            |> Map.ofList

        static member RememberFound (m:InnerMessage, found) =
            m._FoundFields <- m._FoundFields.Add( found )
            m

        static member DecodeFixup m = m

        static member RequiredFields =
            [ 1; 2 ] |> Set.ofList

        static member FoundFields (m:InnerMessage) =
            m._FoundFields

        static member UnknownFields (m:InnerMessage) =
            m._UnknownFields

        member m.Serialize () =
            Array.zeroCreate (serializedLength m |> int32)
            |> ZeroCopyBuffer
            |> serialize m
            |> ZeroCopyBuffer.asArraySegment

        member m.SerializeLengthDelimited () =
            Array.zeroCreate (serializedLengthDelimitedLength m |> int32)
            |> ZeroCopyBuffer
            |> serializeLengthDelimited m

        static member Deserialize (buf:ArraySegment<byte>) =
            buf
            |> ZeroCopyBuffer
            |> deserialize (InnerMessage())

        static member DeserializeLengthDelimited (buf:ArraySegment<byte>) =
            buf
            |> ZeroCopyBuffer
            |> deserializeLengthDelimited (InnerMessage())
                


module PerformanceTest =

    open Xunit
    open FsUnit.Xunit
    open Froto.Core.RecordModel
    open SampleNamespace

    [<Fact>]
    let ``Test Serialization and Deserialization Performance`` () =
        let xs =
            [
                for id = 1 to 1000000 do
                    let inner =
                        InnerMessage(
                            Id=1,
                            Name="Jerry Smith",
                            Option = true,
                            Test = ETest.Two,
                            PackedFixed32 = ResizeArray( [|1u; 2u; 3u; 4u|] ),
                            RepeatedInt32 = ResizeArray( [|5; 6; 7; 8; 9|] )
                            )
                    yield inner
            ]

        let len =
            xs |> List.sumBy (fun x -> serializedLengthDelimitedLength x)

        let buf : byte array = Array.zeroCreate (len |> int)
        let bufAS = System.ArraySegment(buf)
        let zcw = Froto.Core.ZeroCopyBuffer(bufAS)

        xs
        |> List.iter (fun x -> zcw |> serializeLengthDelimited x |> ignore)

        let ys =
            let zcr = Froto.Core.ZeroCopyBuffer(zcw.Array)
            seq {
                while not zcr.IsEof do
                    yield zcr |> deserializeLengthDelimited (InnerMessage())
            }

        ys |> Seq.iter ignore

        1 |> should equal 1
