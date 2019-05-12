namespace Froto.TypeProvider.Core

open Froto.TypeProvider.Runtime.Types

/// Simple implementation of Message class that does nothing useful
/// Basically, this class is needed only for type inference within quotations, because it satisfies requirements
/// to be inherited from Message and to have constructor without parameters
type internal Dummy() = 
    inherit Message()
    
    override __.Serialize(_) = ()
    override __.ReadFrom(_) = ()