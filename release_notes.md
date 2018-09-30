### 0.5.0 _ 2017-12
  * [#78](https://github.com/ctaggart/froto/pull/78) Froto.Parser & Froto.Serialization target netstandard2.0

### 0.4.0 _ 2016-10
  * [#3](https://github.com/ctaggart/froto/issues/3) F# type provider
  * [#15](https://github.com/ctaggart/froto/issues/15) froto.exe code generator for proto3
  * [#68](https://github.com/ctaggart/froto/issues/68) Add Encoders for proto2 *required* fields

### 0.3.1 _ 2016-06
  * [#41](https://github.com/ctaggart/froto/issues/41) Make Parser functions more discoverable and easier to use.

### 0.3.0 _ 2016-06
  * [#37](https://github.com/ctaggart/froto/issues/37) Fix bug: O(n^2) Performance Problem on repeated fields
  * [#38](https://github.com/ctaggart/froto/issues/38) Record Serialization
  * Reimplement Class serialization using the new Record Serialization structure
  * Project reorganization for accessibility, readability and consistency
    * Rename base namespace from Froto.Core to Froto.Serialization and assembly from Froto.Core.dll to Froto.Serialization.dll
    * At the WireFormat level, rename encodeXXX/decodeXXX functions to Pack.toXXX/Unpack.fromXXX
    * At the RawField level, and rename dehydrateXXX/hydrateXXX functions to Encode.fromXXX and Decode.toXXX
    * At the Serialization level, created functions for serializing to/from ZeroCopyBuffer and ArraySegment
    * Rename �Exe� solution to �Froto.Compiler�
    * Rename several folders; e.g., �ProtoParser� to �Parser� and �Exe� to �Compiler�

### 0.2.1 _ 2016-04
  * [#19](https://github.com/ctaggart/froto/issues/19) Improve support for serialization/deserialization
  * [#29](https://github.com/ctaggart/froto/issues/29) publish Froto.Core NuGet package
  * See the [0.2.1 release details](https://github.com/ctaggart/froto/releases/tag/0.2.1) for help upgrading from 0.1.0.

### 0.2.0 _ 2016-02

  * [#6](https://github.com/ctaggart/froto/issues/6) Add support for proto3 language
  * [#8](https://github.com/ctaggart/froto/issues/8) Strict/complete proto2 compliance
  * [#9](https://github.com/ctaggart/froto/pull/9) Updated build & dependencies
  * switched from Apache 2 to MIT license

### 0.1.0 _ 2014
  * moved project to GitHub https://github.com/ctaggart/froto
  * removed experimental Froto.Gen Type Provider code

### 0.0.0 _ 2012-11
  * [Froto.Parser](https://www.nuget.org/packages/Froto.Parser/) 0.0.0.5 published to NuGet Gallery
  * [Froto.Roslyn](https://www.nuget.org/packages/Froto.Roslyn/) 0.0.0.2 published to NuGet Gallery
  * [Froto.Gen](https://www.nuget.org/packages/Froto.Gen/) 0.0.0.1 published to NuGet Gallery
  * blogged: [Parsing a Protocol Buffers .proto File in F#](http://blog.ctaggart.com/2012/11/parsing-protocol-buffers-proto-file-in-f.html)
  * blogged: [Generating C# code for .proto files using Roslyn and F#](http://blog.ctaggart.com/2012/11/generating-c-code-for-proto-files-using.html)
  * blogged: [1st build of an F# Type Provider for .proto files](http://blog.ctaggart.com/2012/11/1st-build-of-f-type-provider-for-proto.html)
