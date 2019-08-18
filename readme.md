
# F# Protocol Buffers

## What are Protocol Buffers
* https://developers.google.com/protocol-buffers/
* https://github.com/google/protobuf

## NuGet
* [Froto.Parser](http://www.nuget.org/packages/Froto.Parser) - Protocol Buffers Parser. Supports [proto2](https://developers.google.com/protocol-buffers/docs/proto), [proto3](https://developers.google.com/protocol-buffers/docs/proto3), & [gRPC](http://www.grpc.io/docs/guides/concepts.html) syntax.
* [Froto.Serialization](http://www.nuget.org/packages/Froto.Serialization) - Protobuf F# Serialization Framework and low-level WireFormat library.
* [Froto.TypeProvider](http://www.nuget.org/packages/Froto.TypeProvider) - A type provider for Protocol Buffers. Supports proto2 so far. [Example usage](https://github.com/ctaggart/froto/blob/master/docs/type-provider/type-provider.md).

## Status
* 0.7.x 2019-08 Type Provider support .NET Core. Parser supports import statements.
* 0.6.0 2018-05 Add proto3 specific deserialization that requires fewer SRTP constraints
* 0.5.0 2017-12 Froto.Parser & Froto.Serialization target netstandard2.0
* 0.4.0 2016-10 Parser now supports full gRPC syntax and options syntax. New Froto.TypeProvider package with support for proto2.
* 0.3.1 2016-06 Made Parser functions more discoverable and easier to use.
* 0.3.0 2016-06 Rewrote F# serialization to support both Record and Class serialization. Significant project structure, serialization namespace, and serialization module refactor.
* 0.2.1 2016-04 Added F# serialization framework to Core, w/full wire format support
  See the [0.2.1 release details](https://github.com/ctaggart/froto/releases/tag/0.2.1) for help upgrading
* 0.2.0 2016-02 Complete rewrite of parser to support full proto2 and proto3 syntax
* 0.1.0 2014-02 Dusted off project and moved to GitHub
* 0.0.5 2012-11 blog [Parsing a Protocol Buffers .proto File in F#](http://blog.ctaggart.com/2012/11/parsing-protocol-buffers-proto-file-in-f.html)
* See the [release notes](https://github.com/ctaggart/froto/blob/master/release_notes.md) for more details.
