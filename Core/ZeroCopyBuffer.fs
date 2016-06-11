namespace Froto.Serialization

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
///
/// TODO: Should this optionally growable?  If so, then by how much?

type ZeroCopyBuffer (seg:ArraySegment<byte>) =
    let m_array = seg.Array
    let mutable m_position = uint32 seg.Offset
    let m_limit = uint32 seg.Offset + uint32 seg.Count 

    member x.Array = m_array 
    member x.Position = m_position

    member x.IsEof
        with get() = m_position >= m_limit

    new (o:ZeroCopyBuffer) =
        ZeroCopyBuffer( o.Array )

    new (backing : byte array) =
        ZeroCopyBuffer(ArraySegment(backing))

    new size =
        ZeroCopyBuffer(Array.zeroCreate size)

    // Return portion of buffer still unread as an ArraySegment
    member x.Remainder
        with get() = ArraySegment( seg.Array, int m_position, int <| m_limit - m_position )

    /// Read one byte from the backing buffer.
    member x.ReadByte () =
        if m_position < m_limit then
            let b = m_array.[int m_position]
            m_position <- m_position + 1u
            b
        else
            raise <| WireFormatException("Read past end of protobuf buffer")

    /// Read multiple bytes, returning an ArraySegment which points directly
    /// into the backing buffer.
    member x.ReadByteSegment (n:uint32) =
        if m_position + n <= m_limit then
            let buf = ArraySegment( m_array, int m_position, int n)
            m_position <- m_position + n
            buf
        else
            raise <| WireFormatException("Read past end of protobuf buffer")

    /// Return portion of buffer written as an ArraySegment
    member x.AsArraySegment
        with get() = ArraySegment( seg.Array, seg.Offset, int m_position - seg.Offset )

    /// Return portion of buffeer writteen as an ArraySegment
    static member asArraySegment (zcb:ZeroCopyBuffer) =
        zcb.AsArraySegment

    /// Return portion of buffer written as an Array (mainly for testing)
    member x.ToArray() =
        seg.Array.[ seg.Offset .. int m_position - 1 ]

    abstract WriteByte : byte -> unit
    abstract WriteByteSegment : uint32 -> (ArraySegment<byte>->unit) -> unit

    /// Write one byte into the backing buffer.
    override x.WriteByte b =
        if m_position < m_limit then
            m_array.[int m_position] <- b
            m_position <- m_position + 1u
        else
            raise <| WireFormatException("Write past end of protobuf buffer")

    /// Write 'len' bytes, via the caller supplied emplace function,
    /// directly into the backing buffer.
    override x.WriteByteSegment (len:uint32) (emplace:ArraySegment<byte>->unit) =
        if m_position + len <= m_limit then
            let buf = ArraySegment( m_array, int m_position, int len )
            emplace buf
            m_position <- m_position + len
        else
            raise <| WireFormatException("Write past end of protobuf buffer")

/// Null ZeroCopyBuffer.  Used to calculate serialized length, without
/// actually writing anything.
//
// Note: This class simplifies the serialization logic, because a single code
// path can be used to both serialize AND calculate size requiements.  However,
// this probably does cost a bit of performance.

type NullWriteBuffer() =
    inherit ZeroCopyBuffer(0)

    let mutable m_length = 0u

    override x.WriteByte b =
        m_length <- m_length + 1u

    override x.WriteByteSegment (len:uint32) (_:ArraySegment<byte>->unit) =
        m_length <- m_length + len

    member x.Length = m_length
