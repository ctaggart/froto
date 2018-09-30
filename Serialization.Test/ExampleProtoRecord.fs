module ExampleProtoRecord

module SampleNamespace =
    open System
    open Froto.Serialization.Encoding

    type ETest =
        | Nada = 0
        | One = 1
        | Two = 2

    let ETestDefault = ETest.One  // NOTE: Non-zero default is only supported in Proto2
    
    type InnerSample = {
        id : int32
        name : string
        option : bool
        test : ETest
        packedFixed32 : uint32 list
        repeatedInt32 : int32 list
        _unknownFields : RawField list
        _foundFields : Set<FieldNum>
        }
        with
            static member Default = {
                id=0
                name=""
                option=false
                test=ETest.Nada
                packedFixed32=[]
                repeatedInt32=[]
                _unknownFields=List.empty
                _foundFields=Set.empty
            }

            static member Serializer (m, zcb) =
                (m.id             |> Encode.fromVarint 1) >>
                (m.name           |> Encode.fromString 2) >>
                (m.option         |> Encode.fromBool 3) >>
                (m.test           |> Encode.fromDefaultedVarint ETestDefault 4) >>
                (m.packedFixed32  |> Encode.fromPackedFixed32 5) >>
                (m.repeatedInt32  |> Encode.fromRepeated Encode.fromVarint 6) >>
                (m._unknownFields |> Encode.fromRawFields )
                <| zcb

            static member DecoderRing =
                [
                    0, fun m rawField -> { m with _unknownFields = rawField :: m._unknownFields }
                    1, fun m rawField -> { m with id = rawField |> Decode.toInt32 } : InnerSample
                    2, fun m rawField -> { m with name = rawField |> Decode.toString } : InnerSample
                    3, fun m rawField -> { m with option = rawField |> Decode.toBool } : InnerSample
                    4, fun m rawField -> { m with test = rawField |> Decode.toEnum } : InnerSample
                    5, fun m rawField -> { m with packedFixed32 = rawField |> Decode.toPackedFixed32 } : InnerSample
                    6, fun m rawField -> { m with repeatedInt32 = (rawField |> Decode.toInt32) :: m.repeatedInt32 } : InnerSample
                ]
                |> Map.ofList

            static member RememberFound (m,found) =
                { m with _foundFields = m._foundFields |> Set.add found }

            static member DecodeFixup m =
                { m with
                    packedFixed32 = List.rev m.packedFixed32
                    repeatedInt32 = List.rev m.repeatedInt32
                    _unknownFields = List.rev m._unknownFields }

            static member RequiredFields =
                [ 1; 2 ] |> Set.ofList

            static member FoundFields m =
                m._foundFields

            static member UnknownFields m =
                m._unknownFields


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
                    let inner = {
                        id=1
                        name="Jerry Smith"
                        option = true
                        test = ETest.Two
                        packedFixed32 = [1u; 2u; 3u; 4u]
                        repeatedInt32 = [5;6;7;8;9]
                        _unknownFields = List.empty
                        _foundFields = Set.empty
                        }
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
                    yield zcr |> Deserialize.Proto2.fromZcbLD InnerSample.Default
            }

        ys |> Seq.iter ignore

        1 |> should equal 1
