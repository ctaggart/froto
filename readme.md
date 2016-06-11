
# F# Protocol Buffers

## What are Protocol Buffers
* https://developers.google.com/protocol-buffers/
* https://github.com/google/protobuf

## NuGet
* [Froto.Parser](http://www.nuget.org/packages/Froto.Parser) - Protobuf Parser
* [Froto.Core](http://www.nuget.org/packages/Froto.Core) - Protobuf F# Serialization Framework and low-level WireFormat library

## Status
* 0.3.0 2016-06 Rewrote F# serialization to support both Record and Class serialization.
* 0.2.1 2016-04 Added F# serialization framework to Core, w/full wire format support
  See the [0.2.1 release details](https://github.com/ctaggart/froto/releases/tag/0.2.1) for help upgrading from 0.1.0.
* 0.2.0 2016-02 Complete rewrite of parser to support full proto2 and proto3 syntax
* 0.1.0 2014-02 v0.1.0 Dusted off project and moved to GitHub
* 0.0.5 2012-11 blog [Parsing a Protocol Buffers .proto File in F#](http://blog.ctaggart.com/2012/11/parsing-protocol-buffers-proto-file-in-f.html)
* See the [release notes](https://github.com/ctaggart/froto/blob/master/release_notes.md) for more details.

## Build Environment Setup for Visual Studio
* Install [Paket for Visual Studio](https://github.com/fsprojects/Paket.VisualStudio) from the "Tools/Extensions and Updates..." menu
* Solution path cannot contain pound sign (#), such as ".../F#/froto/" [due to a .net limitation](http://stackoverflow.com/questions/9319656/how-to-encode-a-path-that-contains-a-hash)
