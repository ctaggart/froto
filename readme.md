
# F# Protocol Buffers

What are Protocol Buffers
 * https://developers.google.com/protocol-buffers/
 * http://code.google.com/p/protobuf/
 * https://code.google.com/p/protobuf-net/

NuGet
 * [Froto.Parser](http://www.nuget.org/packages/Froto.Parser)

Status
 * 2016-01-26 Complete rewrite of parser to support full proto2 and proto3 syntax
 * 2014-02-28 Dusted off project and moved to GitHub
 * 2012-11-02 blog [Parsing a Protocol Buffers .proto File in F#](http://blog.ctaggart.com/2012/11/parsing-protocol-buffers-proto-file-in-f.html)

Todo for parser feature-parity with Google protoc
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
  

 Visual Studio Build Environment
  * Install [Paket for Visual Studio](https://github.com/fsprojects/Paket.VisualStudio) from the "Tools/Extensions and Updates..." menu
  * Solution path cannot contain pound sign (#), such as ".../F#/froto/" [due to a .net limitation](http://stackoverflow.com/questions/9319656/how-to-encode-a-path-that-contains-a-hash)
