namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
[<assembly: AssemblyInformationalVersionAttribute("{\"buildVersion\":\"0.1.0-ci1601200003\",\"buildDate\":\"2016-01-20T00:03:30Z\",\"gitCommit\":\"2be14f48cbb86a720d2719331913c9593dcc438e\"}")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
