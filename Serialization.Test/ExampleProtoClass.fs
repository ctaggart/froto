module ExampleProtoClass

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

module SampleNamespace =

    open System
    open Froto.Serialization
    open Froto.Serialization.Encoding
    open Froto.Serialization.Encoding.Encode
    open Froto.Serialization.Encoding.Decode


    type ETest =
        | Nada = 0
        | One = 1
        | Two = 2

    let ETest_Default = ETest.One  // NOTE: Non-zero default is only supported in Proto2
    
    type InnerMessage () =
        member val Id = 0 with get,set
        member val Name = "" with get,set
        member val Option = false with get,set
        member val Test = ETest_Default with get,set
        member val PackedFixed32 = List.empty with get,set
        member val RepeatedInt32 = List.empty with get,set
        member val _UnknownFields = List.empty with get,set
        member val _FoundFields = Set.empty with get,set

        member x.Clear() =
            x.Id <- 0
            x.Name <- ""
            x.Option <- false
            x.Test <- ETest_Default
            x.PackedFixed32 <- List.empty
            x.RepeatedInt32 <- List.empty
            x._UnknownFields <- List.empty
            x._FoundFields <- Set.empty

        static member Serializer (m:InnerMessage, zcb) =
            (m.Id             |> fromVarint 1) >>
            (m.Name           |> fromString 2) >>
            (m.Option         |> fromBool 3) >>
            (m.Test           |> fromDefaultedVarint ETest_Default 4) >>
            (m.PackedFixed32  |> fromPackedFixed32 5) >>
            (m.RepeatedInt32  |> fromRepeated fromVarint 6) >>
            (m._UnknownFields |> fromRawFields )
            <| zcb

        static member DecoderRing = 
            [
                0, fun (m:InnerMessage) rawField -> m._UnknownFields <- rawField :: m._UnknownFields; m
                1, fun (m:InnerMessage) rawField -> m.Id          <- toInt32 rawField ; m
                2, fun (m:InnerMessage) rawField -> m.Name        <- toString rawField ; m
                3, fun (m:InnerMessage) rawField -> m.Option      <- toBool rawField ; m
                4, fun (m:InnerMessage) rawField -> m.Test        <- toEnum rawField ; m
                5, fun (m:InnerMessage) rawField -> m.PackedFixed32 <- List.rev <| toPackedFixed32 rawField; m
                6, fun (m:InnerMessage) rawField -> m.RepeatedInt32 <- toInt32 rawField :: m.RepeatedInt32; m
            ]
            |> Map.ofList

        static member RememberFound (m:InnerMessage, found) =
            m._FoundFields <- m._FoundFields.Add( found )
            m

        static member DecodeFixup (m:InnerMessage) =
            m.RepeatedInt32  <- List.rev m.RepeatedInt32
            m._UnknownFields <- List.rev m._UnknownFields
            m

        static member RequiredFields =
            [ 1; 2 ] |> Set.ofList

        static member FoundFields (m:InnerMessage) =
            m._FoundFields

        static member UnknownFields (m:InnerMessage) =
            m._UnknownFields

        member m.Serialize ()                   = Serialize.toArray m
        member m.Serialize buf                  = Serialize.toArraySegment m buf
        member m.Serialize zcb                  = Serialize.toZeroCopyBuffer m zcb
        member m.SerializeLengthDelimited ()    = Serialize.toArrayLD m

        static member Deserialize buf                   = buf |> Deserialize.Proto2.fromArray (InnerMessage())
        static member Deserialize buf                   = buf |> Deserialize.Proto2.fromArraySegment (InnerMessage())
        static member Deserialize zcb                   = zcb |> Deserialize.Proto2.fromZeroCopyBuffer (InnerMessage())
        static member Deserialize raw                   = raw |> Deserialize.Proto2.fromRawField (InnerMessage())
        static member DeserializeLengthDelimited buf    = buf |> Deserialize.Proto2.fromArrayLD (InnerMessage())
        static member DeserializeLengthDelimited buf    = buf |> Deserialize.Proto2.fromArraySegmentLD (InnerMessage())
        static member DeserializeLengthDelimited zcb    = zcb |> Deserialize.Proto2.fromZcbLD (InnerMessage())
                


module PerformanceTest =

    open Xunit
    open FsUnit.Xunit
    open Froto.Serialization
    open SampleNamespace

    [<Fact>]
    let ``Test Serialization and Deserialization Performance`` () =
        let xs =
            [
                // let count = 10000
                let count = 100
                for id = 1 to count do
                    let inner =
                        InnerMessage(
                            Id=1,
                            Name="Jerry Smith",
                            Option = true,
                            Test = ETest.Two,
                            PackedFixed32 = [1u; 2u; 3u; 4u],
                            RepeatedInt32 = [5; 6; 7; 8; 9]
                            )
                    yield inner
            ]

        let len =
            xs |> List.sumBy (fun x -> Serialize.serializedLengthLD x)

        let buf : byte array = Array.zeroCreate (len |> int)
        let bufAS = System.ArraySegment(buf)
        let zcw = Froto.Serialization.ZeroCopyBuffer(bufAS)

        xs
        |> List.iter (fun x -> zcw |> Serialize.toZcbLD x |> ignore)

        let ys =
            let zcr = Froto.Serialization.ZeroCopyBuffer(zcw.Array)
            seq {
                while not zcr.IsEof do
                    yield zcr |> Deserialize.Proto2.fromZcbLD (InnerMessage())
            }

        ys |> Seq.iter ignore

        1 |> should equal 1
