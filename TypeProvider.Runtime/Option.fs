[<RequireQualifiedAccess>]
module Froto.TypeProvider.Runtime.Option

/// Wraps x into Some. Useful when union-case constructor can't be called directly
/// e.g. from within expression when x is generated type (e.g. to create option<SomeProtoMessage>)
let some x = Some x