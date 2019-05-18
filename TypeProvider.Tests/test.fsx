#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\FParsec.dll"
#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\FParsecCS.dll"
#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\Froto.Parser.dll"
#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\Froto.Serialization.dll"
#r @"..\TypeProvider.Runtime\bin\Debug\netstandard2.0\Froto.TypeProvider.Runtime.dll"

open Froto.TypeProvider

[<Literal>]
let protoFile = __SOURCE_DIRECTORY__ + "/Proto/type_provider_test.proto"
type Proto = ProtocolBuffersTypeProvider<protoFile>
type Sample = Proto.Froto.Sample

let address =    
    Sample.Person.Address(
        Address1 = "Street",
        HouseNumber = 12)

printfn "%A" address.HouseNumber