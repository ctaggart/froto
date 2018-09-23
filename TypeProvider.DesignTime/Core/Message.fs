namespace Froto.TypeProvider.Core

open Froto.Serialization

// Eventually, this class might be replaced by Froto.Core.Encoding.MessageBase, but so far
// this interface looks simpler and satisfies all needs.

/// Base class for types generated from proto messages.
[<AbstractClass>]
type Message() =

    member this.SerializedLength =
        let buffer = NullWriteBuffer()
        this.Serialize buffer
        buffer.Length

    abstract Serialize: ZeroCopyBuffer -> unit
    
    abstract ReadFrom: ZeroCopyBuffer -> unit

/// Simple implementation of Message class that does nothing useful
/// Basically, this class is needed only for type inference within quotations, because it satisfies requirements
/// to be inherited from Message and to have constructor without parameters
type internal Dummy() = 
    inherit Message()
    
    override __.Serialize(_) = ()
    override __.ReadFrom(_) = ()
    
/// Simple type used to simplify maps serialization and deserialization
type internal MapItem<'Key, 'Value>
    ( keyReader: Reader<'Key>,
      valueReader: Reader<'Value>,
      keyWriter: Writer<'Key>,
      valueWriter: Writer<'Value> ) =
    inherit Message()
    
    member val Key = x with get, set
    member val Value = x with get, set
    
    override this.Serialize(buffer) =
        keyWriter 1 buffer this.Key
        valueWriter 2 buffer this.Value
        
    override this.ReadFrom(buffer) =
        for field in ZeroCopyBuffer.allFields buffer do
            if field.FieldNum = 1 then
                this.Key <- keyReader field
            elif field.FieldNum = 2 then
                this.Value <- valueReader field