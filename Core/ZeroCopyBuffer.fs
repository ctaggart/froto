namespace Froto.Core

open System

/// <summary>
/// Stream-like object backed by caller-supplied ArraySegment.
///
/// Provides methods that work directly on the underlying backing buffer,
/// in order to minimize memory copies.
/// </summary>
///
/// Why not use a Stream?
/// This class allows use of an ArraySegment with a non-zero starting offset
/// to house the source (or desitination) for decoding (or encoding);
/// specifically, so that a length-encoded field can be extracted as an
/// ArraySegment, without copying the associated bytes to a new Array.
/// System.IO.MemoryStream can't do that.  Plus, the Stream API requires
/// additional error checking.

type ZeroCopyBufferBase (seg:ArraySegment<byte>) =

    member val internal Array = seg.Array with get
    member val internal Position = uint32 seg.Offset with get,set
    member val internal Limit = uint32 seg.Offset + uint32 seg.Count with get

    new ( backing : byte array ) =
        ZeroCopyBufferBase(ArraySegment(backing))

    member x.IsEof
        with get() = x.Position = x.Limit

/// Readable ZeroCopyBuffer (see @ZeroCopyBufferBase)
type ZeroCopyReadBuffer (seg:ArraySegment<byte>) =
    inherit ZeroCopyBufferBase(seg)

    new (o:ZeroCopyBufferBase) =
        ZeroCopyReadBuffer( o.Array )

    new (backing : byte array ) =
        ZeroCopyReadBuffer(ArraySegment(backing))

    // Return portion of buffer still unread as an ArraySegment
    member x.Remainder
        with get() = ArraySegment( seg.Array, int x.Position, int <| x.Limit - x.Position )

    /// Read one byte from the backing buffer.
    member x.ReadByte () =
        if x.Position < x.Limit then
            let b = x.Array.[int x.Position]
            x.Position <- x.Position + 1u
            b
        else
            raise <| ProtobufWireFormatException("Read past end of protobuf buffer")

    /// Read multiple bytes, returning an ArraySegment which points directly
    /// into the backing buffer.
    member x.ReadByteSegment (n:uint32) =
        if x.Position + n <= x.Limit then
            let buf = ArraySegment( x.Array, int x.Position, int n)
            x.Position <- x.Position + n
            buf
        else
            raise <| ProtobufWireFormatException("Read past end of protobuf buffer")

/// Writable ZeroCopyBuffer (see @ZeroCopyBufferBase)
///
/// TODO: Should this optionally growable?  If so, then by how much?
type ZeroCopyWriteBuffer (seg:ArraySegment<byte>) =
    inherit ZeroCopyBufferBase(seg)

    new (o:ZeroCopyBufferBase) =
        ZeroCopyWriteBuffer( o.Array )

    new (backing : byte array) =
        ZeroCopyWriteBuffer(ArraySegment(backing))

    new size =
        ZeroCopyWriteBuffer(Array.zeroCreate size)

    // Return portion of buffer written as an ArraySegment
    member x.AsArraySegment
        with get() = ArraySegment( seg.Array, seg.Offset, int x.Position - seg.Offset )

    // Return portion of buffer written as an Array (mainly for testing)
    member x.ToArray() =
        seg.Array.[ seg.Offset .. int x.Position - 1 ]

    /// Write one byte into the backing buffer.
    member x.WriteByte b =
        if x.Position < x.Limit then
            x.Array.[int x.Position] <- b
            x.Position <- x.Position + 1u
        else
            raise <| ProtobufWireFormatException("Write past end of protobuf buffer")

    /// Write 'len' bytes, via the caller supplied emplace function,
    /// directly into the backing buffer.
    member x.WriteByteSegment (len:uint32) (emplace:ArraySegment<byte>->unit) =
        if x.Position + len <= x.Limit then
            let buf = ArraySegment( x.Array, int x.Position, int len )
            emplace buf
            x.Position <- x.Position + len
        else
            raise <| ProtobufWireFormatException("Write past end of protobuf buffer")

/// Null ZeroCopyWriteBuffer.  Used to calculate serialized length, without
/// actually writing anything.
//
// Note: This class simplifies the serialization logic, because a single code
// path can be used to both serialize AND calculate size requiements.  However,
// this probably does cost a bit of performance.

type NullWriteBuffer() =
    inherit ZeroCopyWriteBuffer(0)

    member x.WriteByte b =
        x.Position <- x.Position + 1u

    member x.WriteByteSegment (len:uint32) (_:ArraySegment<byte>->unit) =
        x.Position <- x.Position + len

    member x.Length = x.Position
