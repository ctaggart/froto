
namespace Froto

open System.IO

/// Encapsulates a stream writer which does not close the underlying stream.
// useful when you need to support .NET less than 4.5
// in .NET 4.5, to leave the stream open: new StreamWriter(ms, Encoding.UTF8, 4096, true)
// http://stackoverflow.com/questions/1187700/does-disposing-a-streamwriter-close-the-underlying-stream/6784157#6784157
type NoCloseStreamWriter(stream:Stream, encoding) =
    inherit StreamWriter(stream, encoding)
    override x.Dispose(disposeManaged) =
        // Dispose the stream writer but pass false to the dispose
        // method to stop it from closing the underlying stream
        base.Dispose(false)


