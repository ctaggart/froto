[<RequireQualifiedAccess>]
module ProtoTypes.Core.ZeroCopyBuffer

open Froto.Core

/// Reads all fields from given instance of ZeroCopyBuffer
let allFields (zcb: ZeroCopyBuffer) = seq {
    while (not zcb.IsEof) && zcb.Array.[int zcb.Position] > 7uy do
        yield WireFormat.decodeField zcb
}