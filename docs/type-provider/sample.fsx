#load "paket-files/include-scripts/net45/include.froto.typeprovider.fsx" 

open Froto.TypeProvider

let [<Literal>] ProtoFile = __SOURCE_DIRECTORY__ + "/protocol.proto"
type Protocol = ProtocolBuffersTypeProvider<ProtoFile>
type People = Protocol.Sample.People

let person = People.Person()

person.Name <- "John Smith"
person.Email <- Some "john.smith@whatever.com"
person.Phones.Add(
    People.Person.PhoneNumber(
        Number = "222-33-455", 
        Type = Some People.Person.PhoneType.Home))

open Froto.Serialization
let buffer = ZeroCopyBuffer(int person.SerializedLength)
person.Serialize buffer

let person' = People.Person.Deserialize(ZeroCopyBuffer(buffer))
printfn "%s" person'.Name