[<RequireQualifiedAccess>]
module Froto.TypeProvider.Core.ZeroCopyBuffer

open Froto.Serialization
open Froto.Serialization.Encoding.WireFormat

/// Reads all fields from given instance of ZeroCopyBuffer
let allFields (zcb: ZeroCopyBuffer) = seq {
    while (not zcb.IsEof) && zcb.Array.[int zcb.Position] > 7uy do
        yield Unpack.fromField zcb
}