# Froto.TypeProvider 101

`Froto.TypeProvider` is a generative type provider which generates types from `.proto` files. It's available on [NuGet](https://www.nuget.org/packages/Froto.TypeProvider/)

## Example

This is an example of `protocol.proto` file - description of a simple protocol using `proto2` syntax.

```
package sample.people;

message Person { 

  message PersonAddress {
    required string street = 1;
    required uint32 house_number = 2;
  }

  enum PhoneType { 
    MOBILE = 0; 
    HOME = 1; 
    WORK = 2; 
  }

  message PhoneNumber { 
    required string number = 1; 
    optional PhoneType type = 2;
  }

  required string name = 1;
  optional string email = 4;
  repeated PhoneNumber phones = 5; 
}
```

Then, you can access types defined in this file in your `.fsx` file by doing the following:
1. Reference `Froto.TypeProvider.dll` and its dependencies. The easiest way to do this will be by using [Paket](https://fsprojects.github.io/Paket/):
```
> paket.exe generate-include-scripts framework net45 type fsx
```
After that, you should be able to find generated scripts under `paket-files` folder. You can add referencess to all required libraries by adding the following line to your `.fsx` file:
```
#load "paket-files/include-scripts/net45/include.froto.typeprovider.fsx" 
```
2. Then, let's generate some types:
```
open Froto.TypeProvider

let [<Literal>] ProtoFile = __SOURCE_DIRECTORY__ + "/protocol.proto"
type Protocol = ProtocolBuffersTypeProvider<ProtoFile>
```
Now, type `Protocol` contains all messages an enumerations defined in `protocol.proto`.
```
type People = Protocol.Sample.People
```
Nested `Sample.People` type corresponds to `package` declaraton in the `protocol.proto` file.
3. Now you can create messages:
```
let person = People.Person()

person.Name <- "John Smith"
person.Email <- Some "john.smith@whatever.com"
person.Phones.Add(
    People.Person.PhoneNumber(
        Number = "222-33-455", 
        Type = Some People.Person.PhoneType.Home))

```
4. Each message type has two methods that allow you to convert messages to/from binary format:
```
  member this.Serialize: ZeroCopyBuffer -> unit
  static member Deserialize: ZeroCopyBuffer -> 'T
```
They can be used like this:
```
open Froto.Serialization
let buffer = ZeroCopyBuffer(int person.SerializedLength)
person.Serialize buffer

let person' = People.Person.Deserialize(ZeroCopyBuffer(buffer))
printfn "%s" person'.Name
```