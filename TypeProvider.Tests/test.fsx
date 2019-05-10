#r @"D:\dev\code\open-source\froto\TypeProvider.DesignTime\bin\Debug\netstandard2.0\FParsec.dll"
#r @"D:\dev\code\open-source\froto\TypeProvider.DesignTime\bin\Debug\netstandard2.0\FParsecCS.dll"
#r @"D:\dev\code\open-source\froto\TypeProvider.DesignTime\bin\Debug\netstandard2.0\Froto.Parser.dll"
#r @"D:\dev\code\open-source\froto\TypeProvider.DesignTime\bin\Debug\netstandard2.0\Froto.Serialization.dll"
#r @"D:\dev\code\open-source\froto\TypeProvider.Runtime\bin\Debug\netstandard2.0\Froto.TypeProvider.DesignTime.dll"
#r @"D:\dev\code\open-source\froto\TypeProvider.Runtime\bin\Debug\netstandard2.0\Froto.TypeProvider.Runtime.dll"


open Froto.TypeProvider

type Ololo = ProtocolBuffersTypeProvider<"D:/dev/code/open-source/froto/TypeProvider.Tests/Proto/type_provider_test.proto">