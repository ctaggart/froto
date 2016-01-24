
# F# Protocol Buffers

What are Protocol Buffers
 * https://developers.google.com/protocol-buffers/
 * http://code.google.com/p/protobuf/
 * https://code.google.com/p/protobuf-net/

NuGet
 * [Froto.Parser](http://www.nuget.org/packages/Froto.Parser)

Status
 * 2014-02-28 Dusted off project and moved to GitHub
 * 2012-11-02 blog [Parsing a Protocol Buffers .proto File in F#](http://blog.ctaggart.com/2012/11/parsing-protocol-buffers-proto-file-in-f.html)

 Visual Studio Build Environment
  * Install [Paket for Visual Studio](https://github.com/fsprojects/Paket.VisualStudio) from the "Tools/Extensions and Updates..." menu
  * Solution path cannot contain pound sign (#), such as ".../F#/froto/" [due to a .net limitation](http://stackoverflow.com/questions/9319656/how-to-encode-a-path-that-contains-a-hash)
