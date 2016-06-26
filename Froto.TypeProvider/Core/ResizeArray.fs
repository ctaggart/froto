namespace ProtoTypes.Core


[<RequireQualifiedAccess>]
module ResizeArray =

    let add<'T> (list: obj) (item: 'T) =
        let list = list :?> ResizeArray<'T>
        list.Add item

    let toList<'T> (xs: obj) = xs :?> ResizeArray<'T> |> List.ofSeq