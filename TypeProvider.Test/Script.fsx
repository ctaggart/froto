#r "../TypeProvider/bin/Debug/net452/Froto.TypeProvider.dll"

open Froto.TypeProvider

[<Literal>]
let ProtoFilePath = __SOURCE_DIRECTORY__ + @"\..\test\type_provider_test.proto"

type Test = ProtocolBuffersTypeProvider<ProtoFilePath>