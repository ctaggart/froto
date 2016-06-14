namespace Froto.Serialization.Encoding

open Froto.Serialization

///
/// Utility functions used by the serializer
///
module Utility =

    /// Convert int64 to bool
    let inline int64ToBool (u:uint64) = not (u=0UL)

    /// Convert bool to int64
    let inline boolToInt64 b = if b then 1UL else 0UL

    /// Encode SInt32
    let zigZag32 (n:int32) = (n <<< 1) ^^^ (n >>> 31)
    /// Encode SInt64
    let zigZag64 (n:int64) = (n <<< 1) ^^^ (n >>> 63)

    /// Decode SInt32
    let zagZig32 (n:int32) = int32(uint32 n >>> 1) ^^^ (if n&&&1  = 0  then 0  else -1 )
    /// Decode SInt64
    let zagZig64 (n:int64) = int64(uint64 n >>> 1) ^^^ (if n&&&1L = 0L then 0L else -1L)

    /// Calculate length when encoded as a Varint; if value is default, then length is 0
    let varIntLenDefaulted d (v:uint64) =
        if v = d
        then 0
        else
            let rec loop acc len =
                let bMore = acc > 0x7FUL
                if bMore
                then loop (acc >>> 7) (len+1)
                else len
            loop v 1

    /// Calculate length when encoded as a Varint; if value is default, then length is 0
    let varIntLen (v:uint64) =
        varIntLenDefaulted 0UL v

    /// Calculate length when encoded as a Varint without a default; e.g., a packed field
    let varIntLenNoDefault (v:uint64) =
        let rec loop acc len =
            let bMore = acc > 0x7FUL
            if bMore
            then loop (acc >>> 7) (len+1)
            else len
        loop v 1


    /// Calculate field number length when encoded in a tag
    let tagLen (t:int32) =
        varIntLenNoDefault ((uint64 t) <<< 3)

    /// Decode a ZeroCopyBuffer until the predicate is satisfied
    let decodeWhile (predicate:ZeroCopyBuffer->bool) (zcb:ZeroCopyBuffer) =
        seq {
            while predicate zcb do
                yield WireFormat.Unpack.fromField zcb
            }
    
    /// Decode an entire ZeroCopyBuffer (until EOF)
    let decodeBuffer zcb =
        zcb
        |> decodeWhile (fun zcb -> not zcb.IsEof)

    /// Decode a length-delimited ZeroCopyBuffer
    let unpackLengthDelimited zcb =
        let len = zcb |> WireFormat.Unpack.fromVarint |> uint32
        let end_ = zcb.Position + len
        zcb
        |>  decodeWhile (fun zcb -> zcb.Position < end_)


    /// Flip first two parameters and apply a function.
    /// Useful for returning a curried function that (partially) applies
    /// the second parameter, but leaves the first parameter free.
    let inline flip f a b = f b a

