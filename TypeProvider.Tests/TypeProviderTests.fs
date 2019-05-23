[<Xunit.Trait("Kind", "Unit")>]
module Froto.TypeProvider.Tests.TypeProviderTests

open System
open System.Linq

open Xunit
open FsUnit.Xunit

open Froto.TypeProvider
open Froto.TypeProvider.Runtime.Types
open Froto.Serialization

type Proto = ProtocolBuffersTypeProvider<"Proto/type_provider_test.proto">
type Sample = Proto.Froto.Sample
type ValueOneofCase = Sample.OneOfContainer.ValueOneofCase

type Books = ProtocolBuffersTypeProvider<"Proto/books.proto">

let private createPerson() =
    let address =
        Sample.Person.Address(
            Address1 = "Street",
            HouseNumber = 12)

    address.Whatever.AddRange [1; 2; 3]
    address.SomeInts.AddRange [Sample.Person.IntContainer(Value = 5); Sample.Person.IntContainer(Value = 7)]

    Sample.Person(
        Name = "Name",
         Id = 1,
         HasCriminalConvictions = false,
         Weight = 82.3,
         PersonGender = Sample.Person.Gender.Female,
         Email = Some "Email",
         PersonAddress = Some address,
         PassportDetails = Sample.Person.Passport())

let inline private serializeDeserialize(msg: ^T when ^T :> Message) =
    let buffer = ZeroCopyBuffer 1000
    msg.Serialize buffer

    let buffer' = ZeroCopyBuffer buffer.AsArraySegment
    (^T : (static member Deserialize: ZeroCopyBuffer -> ^T) (buffer'))

[<Fact>]
let ``Person test``() =
    let person = createPerson()
    person.Name |> should be (equal "Name")
    person.PersonGender |> should be (equal Sample.Person.Gender.Female)
    person.PersonAddress.Value.Address1 |> should be (equal "Street")

[<Fact>]
let ``Serialization test``() =
    let person = createPerson()
    let buffer = ZeroCopyBuffer 1000
    person.Serialize buffer |> ignore
    buffer.Position |> should be (greaterThan 0u)

[<Fact>]
let ``Deserialization test``() =
    let person = createPerson()
    let person' = serializeDeserialize person

    person'.Name |> should be (equal person.Name)
    person'.Id |> should be (equal person.Id)
    person'.HasCriminalConvictions |> should be (equal person.HasCriminalConvictions)
    person'.Weight |> should be (equal person.Weight)
    person'.PersonGender |> should be (equal person.PersonGender)
    person'.Email |> should be (equal person.Email)

    person'.PersonAddress.IsSome |> should be True
    let address = person.PersonAddress.Value
    let address' = person'.PersonAddress.Value
    address'.Address1 |> should be (equal address.Address1)
    address'.HouseNumber |> should be (equal address.HouseNumber)
    address'.Whatever |> List.ofSeq |> should be (equal <| List.ofSeq address.Whatever)

    address'.SomeInts
    |> Seq.map (fun v -> v.Value)
    |> List.ofSeq
    |> should be (equal (address.SomeInts |> Seq.map(fun v -> v.Value) |> List.ofSeq))

[<Fact>]
let ``Deserialize None optional value``() =
    let person = createPerson()
    person.PersonAddress <- None
    let person' = serializeDeserialize person

    person'.PersonAddress.IsSome |> should be False

[<Fact>]
let ``Deserialize empty repeated value``() =
    let person = createPerson()
    let address = person.PersonAddress.Value

    address.SomeInts.Clear()
    address.Whatever.Clear()

    let address' = serializeDeserialize address

    address'.SomeInts |> should be Empty
    address'.Whatever |> should be Empty

[<Fact>]
let ``Primitive types``() =
    let container =
        Sample.PrimitiveContainer(
            DoubleField = 1.2,
            Int32Field = 42,
            Int64Field = 12351L,
            Uint32Field = 123124ul,
            Uint64Field = 1146111UL,
            Sint32Field = 1112,
            Sint64Field = -1236134L,
            Fixed32Field = proto_fixed32.MaxValue,
            Fixed64Field = proto_fixed64.MaxValue,
            Sfixed32Field = proto_sfixed32.MinValue,
            Sfixed64Field = proto_sfixed64.MinValue,
            BoolField = true,
            StringField = "string field value",
            BytesField = ArraySegment [| 1uy; 2uy; 42uy |])

    let container2 = serializeDeserialize container

    container2.DoubleField |> should be (equal container.DoubleField)

    container2.Int32Field |> should be (equal container.Int32Field)
    container2.Int64Field |> should be (equal container.Int64Field)

    container2.Uint32Field |> should be (equal container.Uint32Field)
    container2.Uint64Field |> should be (equal container.Uint64Field)

    container2.Sint32Field |> should be (equal container.Sint32Field)
    container2.Sint64Field |> should be (equal container.Sint64Field)

    container2.Fixed32Field |> should be (equal container.Fixed32Field)
    container2.Fixed64Field |> should be (equal container.Fixed64Field)

    container2.Sfixed32Field |> should be (equal container.Sfixed32Field)
    container2.Sfixed64Field |> should be (equal container.Sfixed64Field)

    container2.BoolField |> should be (equal container.BoolField)
    container2.StringField |> should be (equal container.StringField)
    container2.BytesField.ToArray() |> should be (equal <| container.BytesField.ToArray())

[<Fact>]
let ``Oneof properties test``() =
    let oneofContainer = Sample.OneOfContainer()
    oneofContainer.ValueCase |> should be (equal ValueOneofCase.None)

    oneofContainer.Text <- Some "text"
    oneofContainer.Text |> should be (equal <| Some "text")
    oneofContainer.ValueCase |> should be (equal ValueOneofCase.Text)
    oneofContainer.Identifier.IsSome |> should be False

    oneofContainer.Identifier <- Some 10
    oneofContainer.Identifier |> should be (equal <| Some 10)
    oneofContainer.Text.IsSome |> should be False
    oneofContainer.ValueCase |> should be (equal ValueOneofCase.Identifier)

    oneofContainer.Identifier <- None
    oneofContainer.Identifier.IsSome |> should be False
    oneofContainer.Text.IsSome |> should be False
    oneofContainer.ValueCase |> should be (equal ValueOneofCase.None)

    oneofContainer.Identifier <- Some 10
    oneofContainer.ClearValue()
    oneofContainer.ValueCase |> should be (equal ValueOneofCase.None)
    oneofContainer.Identifier.IsSome |> should be False
    oneofContainer.Text.IsSome |> should be False

[<Fact>]
let ``Oneof properties serialization test``() =
    let oneofContainer = Sample.OneOfContainer()
    oneofContainer.Identifier <- Some 42
    oneofContainer.AnotherText <- "Some another text"

    let buffer = ZeroCopyBuffer 1000
    oneofContainer.Serialize buffer
    let oneofContainer' = Sample.OneOfContainer.Deserialize <| ZeroCopyBuffer buffer.AsArraySegment

    oneofContainer.Identifier |> should be (equal oneofContainer'.Identifier)
    oneofContainer.AnotherText |> should be (equal oneofContainer'.AnotherText)
    oneofContainer.ValueCase |> should be (equal oneofContainer'.ValueCase)

[<Fact>]
let ``Map test``() =
    let mapContainer = Sample.MapContainer()
    mapContainer.PrimitiveMap.Add(1, "foo")
    mapContainer.PrimitiveMap.Add(2, "bar")

    mapContainer.People.Add("Vasya", createPerson())

    mapContainer.Switches.Add(1, Sample.MapContainer.Switch.On)

    let buffer = mapContainer.SerializedLength |> int |> ZeroCopyBuffer
    mapContainer.Serialize buffer

    let mapContainer' = Sample.MapContainer.Deserialize <| ZeroCopyBuffer buffer.AsArraySegment

    mapContainer'.PrimitiveMap |> should be (not' Null)
    mapContainer'.PrimitiveMap.[1] |> should be (equal "foo")
    mapContainer'.PrimitiveMap.[2] |> should be (equal "bar")

    mapContainer'.Switches |> should be (not' Null)
    mapContainer'.Switches |> should haveCount 1
    mapContainer'.Switches.[1] |> should be (equal Sample.MapContainer.Switch.On)

    mapContainer'.People |> should be (not' Null)
    mapContainer'.People |> should haveCount 1
    mapContainer'.People.["Vasya"].Name |> should be (equal "Name")

[<Fact>]
let ``SerializedSize test``() =
    let address = Sample.Person.Address(Address1 = "")
    address.SerializedLength |> should be (equal 0u)

    address.Address1 <- "add1"
    address.Whatever.AddRange [1; 2; 3]

    address.SerializedLength |> should be (greaterThan 0u)

[<Fact>]
let ``Default values test``() =
    let p = Sample.Person()
    p.Name |> should be (equal String.Empty)
    p.Email.IsSome |> should be False

    let address = Sample.Person.Address()
    address.Whatever |> should be Empty
    address.SomeInts |> should be Empty

[<Fact>]
let ``Empty message serialization test``() =
    let p = Sample.Person()
    let b = ZeroCopyBuffer(100)
    p.Serialize b
    b.Position |> should be (equal 0u)

[<Fact>]
let ``Root enum usage test``() =
    let action = Sample.Action()
    action.Action <- "test"
    action.State <- Sample.State.Pending

    let action' = serializeDeserialize action

    action'.Action |> should be (equal action.Action)
    action'.State |> should be (equal action.State)

[<Fact>]
let ``Books test``() =
    let author = Books.BookAuthor(Name = Some "Author")
    let details = Books.BookDetails(Author = Some author)

    let details' = serializeDeserialize details

    details'.Author.IsSome |> should be True
    details'.Author.Value.Name |> should be (equal author.Name)