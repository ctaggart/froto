#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\FParsec.dll"
#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\FParsecCS.dll"
#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\Froto.Parser.dll"
#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\Froto.Serialization.dll"
#r @"..\TypeProvider.DesignTime\bin\Debug\netstandard2.0\Froto.TypeProvider.DesignTime.dll"


open Froto.TypeProvider

[<Literal>]
let protoFile = __SOURCE_DIRECTORY__ + "/Proto/type_provider_test.proto"
type Ololo = ProtocolBuffersTypeProvider<protoFile>