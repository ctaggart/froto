module SampleProto

open Froto.Core
open Froto.Core.ClassModel
open Froto.Core.Hydration

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

type InnerMessage () as self =
    inherit MessageBase()
    let ETestDefault = ETest.One  // NOTE: Non-zero default is only supported in Proto2

    member val Id = 0 with get,set
    member val Name = "" with get,set
    member val Option = false with get,set
    member val Test = ETestDefault with get,set
    member val PackedFixed32 = ResizeArray() with get,set
    member val RepeatedInt32 = ResizeArray() with get,set

    override x.Clear() =
        x.Id <- 0
        x.Name <- ""
        x.Option <- false
        x.Test <- ETestDefault
        x.PackedFixed32 <- ResizeArray()
        x.RepeatedInt32 <- ResizeArray()

    override x.DecoderRing = 
        [
            1, fun rawField -> self.Id          <- hydrateInt32 rawField
            2, fun rawField -> self.Name        <- hydrateString rawField
            3, fun rawField -> self.Option      <- hydrateBool rawField
            4, fun rawField -> self.Test        <- hydrateEnum rawField
            5, fun rawField -> self.PackedFixed32.Clear()
                               self.PackedFixed32.AddRange( List.rev <| hydratePackedFixed32 rawField )
            6, fun rawField -> self.RepeatedInt32.Add(hydrateInt32 rawField)
        ]
        |> Map.ofList

    override x.Encode(zcb) =
        let encode =
            (x.Id            |> dehydrateVarint 1) >>
            (x.Name          |> dehydrateString 2) >>
            (x.Option        |> dehydrateBool 3) >>
            (x.Test          |> dehydrateDefaultedVarint ETestDefault 4) >>
            (x.PackedFixed32 |> Seq.toList |> dehydratePackedFixed32 5) >>
            (x.RepeatedInt32 |> Seq.toList |> dehydrateRepeated dehydrateVarint 6)
        encode zcb
        // Note: The Seq.toList above is less efficient than passing in a ResizeArray
        // However, that means duplicating all the dehydratePackedXXX functions with
        // a version that works on ResizeArray.  Having these methods use a Seq would
        // would be less efficient for cases that pass in a List.
        
    static member FromZeroCopyBuffer (zcb:ZeroCopyBuffer) =
        let self = InnerMessage()
        self.Merge(zcb) |> ignore
        self

and ETest =
    | Nada = 0
    | One = 1
    | Two = 2

type OuterMessage() as self =
    inherit MessageBase()

    member val Id = 0 with get,set
    member val Inner = None with get,set
    member val HasMore = false with get,set

    override x.Clear() =
        x.Id <- 0
        x.Inner <- None
        x.HasMore <- false

    override x.DecoderRing =
        [
            1,  fun rawField -> self.Id         <- hydrateInt32 rawField
            42, fun rawField -> self.Inner      <- hydrateOptionalMessage InnerMessage.FromZeroCopyBuffer rawField
            43, fun rawField -> self.HasMore    <- hydrateBool rawField
        ]
        |> Map.ofList

    override x.Encode(zcb) =
        let encode =
            (x.Id |> dehydrateVarint 1) >>
            (x.Inner |> dehydrateOptionalMessage serializeClassLengthDelimited 42) >>
            (x.HasMore |> dehydrateBool 43)
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
                    inner.Id <- 1
                    inner.Name <- "Jerry Smith"
                    inner.Option <- true
                    inner.Test <- ETest.Two
                    inner.PackedFixed32.AddRange([1u; 2u; 3u; 4u])
                    inner.RepeatedInt32.AddRange([5;6;7;8;9])
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

