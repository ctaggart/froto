
# F# Protocol Buffers

## What are Protocol Buffers
 * https://developers.google.com/protocol-buffers/docs/overview
 * https://github.com/google/protobuf
 * https://github.com/mgravell/protobuf-net

## NuGet
 * [Froto.Parser](http://www.nuget.org/packages/Froto.Parser) - Protobuf Parser
 * [Froto.Core](http://www.nuget.org/packages/Froto.Core) - Protobuf F# Serialization Framework and low-level WireFormat library.

## Build Environment Setup for Visual Studio
  * Install [Paket for Visual Studio](https://github.com/fsprojects/Paket.VisualStudio) from the "Tools/Extensions and Updates..." menu
  * Solution path cannot contain pound sign (#), such as ".../F#/froto/" [due to a .net limitation](http://stackoverflow.com/questions/9319656/how-to-encode-a-path-that-contains-a-hash)

## Status
 * v0.2.1 (2016-03-01) Added F# serialization framework to Core, w/full wire format support.
 * v0.2.0 (2016-01-26) Complete rewrite of parser to support full proto2 and proto3 syntax
 * v0.1.0 (2014-02-28) v0.1.0 Dusted off project and moved to GitHub
 * v0.0.5 (2012-11-02) blog [Parsing a Protocol Buffers .proto File in F#](http://blog.ctaggart.com/2012/11/parsing-protocol-buffers-proto-file-in-f.html)

## Updating from Froto 0.1.0

### Core
 * `Froto.Core` was reworked to provide serialization and deserialization of
   all supported wire types and to minimize buffer copying.

  * Code depending on `Froto.Core.IO` will need to be rewritten to use the
    following modules and types:
     - `Froto.Core.WireFormat`
     - `Froto.Core.Encoding.RawField`
     - `Froto.Core.ZeroCopyBuffer` and subclasses

  * Alternatively, the functions in the `Froto.Core.Encoding.Serializer`
    module can provide a slightly higher level of abstraction.

  * Or, see next.

 * Added a framework for easily constructing serializable class types.

  * See `Froto.Core.Encoding.MessageBase` for an abstract base class which
    provides the serialization framework, and see
   `Froto.Core.Test/ExampleProtoClass.fs` for example usage.

### Parser
 * The parser now generates an AST based on Discriminated Unions, rather than
   objects.  `Froto.Parser.Ast` (and the underlying parser) now support the full proto2
   and proto3 languages.

 * The old primary class, `Froto.Parser.ProtoAst.ProtoFile` has been renamed to
   `Froto.Parser.Model.ProtoFile` and given static factory methods to simplify
   access from C#, VB.net, etc.

   _Note that this model curently only supports a subset of the proto2 language.
   This will be expanded in later releases to fully support proto2 & proto3._

   * `ProtoFile.ParseString(s:string)`
   * `ProtoFile.ParseStream(streamName:string, stream:System.IO.Stream)`
   * `ProtoFile.ParseFile(fileName:string)`

## Todo for parser feature-parity with Google protoc
  - [ ] Load and parse files from import statements.
        See https://developers.google.com/protocol-buffers/docs/proto3#importing-definitions
  - [ ] Verify message identifiers used as field types are actually defined
        and enum values used in option value assignment are actually
        defined.  Note these must handle forward references.
  - [ ] For option assignment, verify type of literal matches type of option's
        message field definition.
  - [ ] Predefine standard options and enums, or load these from an
        external "descriptor.proto" file.  For standard option definitions,
        see https://github.com/google/protobuf/blob/master/src/google/protobuf/descriptor.proto#L251
  - [ ] Generate errors _with context including file/line/column_ when
        any of the above validations fail (note: this might be better than
        what protoc reports).
  - [ ] Record enough source information in the AST to generate SourceCodeInfo
        (see https://github.com/google/protobuf/blob/master/src/google/protobuf/descriptor.proto#L84)
  - [ ] Record any other information in the AST needed to generate a complete
        FileDescriptorProto and FileDescriptorSet.
