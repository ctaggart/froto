module SampleProtoRecord

module SampleNamespace =
    open Froto.Core
    open Froto.Core.Encoding.Utility
    open Froto.Core.Encoding.RecordSerializer

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
        }
        with
            static member Default = {
                id=0
                name=""
                option=false
                test=ETest.Nada
                packedFixed32=[]
                repeatedInt32=[]
            }

            member m.Serialize zcb                      = zcb |> serialize m
            member m.Deserialize zcb                    = zcb |> deserialize m
            member m.SerializeLengthDelimited zcb       = zcb |> serializeLengthDelimited m
            member m.DeserializeLengthDelimited zcb     = zcb |> deserializeLengthDelimited m

            member m.SerializeWithUnknowns unks zcb                 = zcb |> serializeWithUnknowns m unks
            member m.DeserializeWithUnknowns zcb                    = zcb |> deserializeWithUnknowns m
            member m.SerializeLengthDelimitedWithUnknowns unks zcb  = zcb |> serializeLengthDelimitedWithUnknowns m unks
            member m.DeserializeLengthDelimitedWithUnknowns zcb     = zcb |> deserializeLengthDelimitedWithUnknowns m
                
            member m.Serializer zcb =
                (m.id            |> dehydrateVarint 1) >>
                (m.name          |> dehydrateString 2) >>
                (m.option        |> dehydrateBool 3) >>
                (m.test          |> dehydrateDefaultedVarint ETestDefault 4) >>
                (m.packedFixed32 |> dehydratePackedFixed32 5) >>
                (m.repeatedInt32 |> dehydrateRepeated dehydrateVarint 6)
                <| zcb

            member m.DecoderRing =
                [
                    1, fun m rawField -> { m with id = rawField |> hydrateInt32 }
                    2, fun m rawField -> { m with name = rawField |> hydrateString }
                    3, fun m rawField -> { m with option = rawField |> hydrateBool }
                    4, fun m rawField -> { m with test = rawField |> hydrateEnum }
                    5, fun m rawField -> { m with packedFixed32 = rawField |> hydratePackedFixed32 }
                    6, fun m rawField -> { m with repeatedInt32 = (rawField |> hydrateInt32) :: m.repeatedInt32 }
                ]
                |> Map.ofList

            static member DecodeFixup m =
                { m with
                    packedFixed32 = List.rev m.packedFixed32
                    repeatedInt32 = List.rev m.repeatedInt32 }

module PerformanceTest =

    open Xunit
    open FsUnit.Xunit
    open Froto.Core.Encoding.RecordSerializer
    open SampleNamespace

    [<Fact>]
    let ``Test Serialization and Deserialization Performance`` () =
        let xs =
            [
                for id = 1 to 1000000 do
                    let inner = {
                        id=1
                        name="Jerry Smith"
                        option = true
                        test = ETest.Two
                        packedFixed32 = [1u; 2u; 3u; 4u]
                        repeatedInt32 = [5;6;7;8;9]
                        }
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
                    yield zcr |> deserializeLengthDelimited InnerSample.Default
            }

        ys |> Seq.iter ignore

        1 |> should equal 1
